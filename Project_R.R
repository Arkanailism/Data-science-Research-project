# ============================================
# Cricket project — Clean -> EDA -> Prep -> Baselines -> Partial-state
# ============================================

# ---- Packages ----
req <- c("tidyverse","janitor","lubridate","zoo","pROC","DescTools","broom","scales",
         "stringr","glmnet","randomForest","xgboost","lmtest")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, Ncpus = 2)
invisible(lapply(req, library, character.only = TRUE))
set.seed(42)

# ---- Paths (edit if needed) ----
match_path <- "./data/cleaned_match_data.csv"
balls_path <- "./data/cleaned_ball_by_ball_data.csv"

# ---- Load + tidy names ----
match <- readr::read_csv(match_path, guess_max = 1e6) %>% janitor::clean_names()
balls  <- readr::read_csv(balls_path,  guess_max = 1e6) %>% janitor::clean_names()

cat("\n[Columns] match:\n"); print(names(match))
cat("\n[Columns] balls:\n"); print(names(balls))

# ---- Helpers ----
has_col <- function(df, nm) nm %in% names(df)
as01 <- function(x) as.integer(x %in% c(1,"1",TRUE))
log_loss <- function(p, y01) { p <- pmin(pmax(p, 1e-15), 1-1e-15); -mean(y01*log(p)+(1-y01)*log(1-p)) }
eval_bin <- function(p, y) {
  y01 <- as01(y)
  tibble(
    AUC      = if(length(unique(y01))==2) as.numeric(pROC::auc(y01, p)) else NA_real_,
    LogLoss  = log_loss(p, y01),
    Brier    = mean((p - y01)^2)
  )
}
calib_plot <- function(p, y, bins=10, title="Calibration") {
  df <- tibble(p=p, y=as01(y)) |>
    mutate(bin = ntile(p, bins)) |>
    group_by(bin) |>
    summarise(pred=mean(p), obs=mean(y), n=n(), .groups="drop")
  ggplot(df, aes(pred, obs, size=n)) +
    geom_point(alpha=.85) +
    geom_abline(slope=1, intercept=0, linetype=2) +
    scale_x_continuous(labels=scales::percent) +
    scale_y_continuous(labels=scales::percent) +
    labs(x="Predicted", y="Observed", title=title) +
    theme_minimal()
}

# ========== STEP 1: DATA CLEANING ==========
# Basic harmonisation
if (!has_col(match,"date"))   match$date <- as.Date("2000-01-01") + dplyr::row_number()
if (!has_col(match,"format")) match$format <- "ODI"
stopifnot(has_col(match,"match_id"), has_col(balls,"match_id"))

# Create total_runs if missing (rename or build from batsman_runs + extras)
rename_candidates <- c("total_runs","runs_total","total","runs","score","runs_this_ball","all_runs")
hit <- intersect(rename_candidates, names(balls))
if (length(hit) >= 1) balls <- balls %>% dplyr::rename(total_runs = !! rlang::sym(hit[1]))

if (!has_col(balls,"total_runs")) {
  if (!has_col(balls,"batsman_runs")) {
    alt_bat <- intersect(names(balls), c("runs_off_bat","bat_runs","batter_runs","striker_runs"))
    if (length(alt_bat)) balls <- balls %>% dplyr::rename(batsman_runs = !! rlang::sym(alt_bat[1]))
  }
  extras_cols <- intersect(names(balls),
                           c("extras","wides","wide","w","noballs","no_balls","nb",
                             "byes","b","legbyes","leg_byes","lb","penalty"))
  if (!has_col(balls,"extras") && length(extras_cols) > 0) {
    balls <- balls %>% dplyr::mutate(extras = rowSums(dplyr::across(dplyr::all_of(extras_cols)), na.rm = TRUE))
  }
  if (all(c("batsman_runs","extras") %in% names(balls))) {
    balls <- balls %>% dplyr::mutate(total_runs = batsman_runs + extras)
  } else if (has_col(balls,"batsman_runs")) {
    warning("No explicit extras found; using batsman_runs as total_runs (may undercount).")
    balls <- balls %>% dplyr::mutate(total_runs = batsman_runs)
  } else {
    stop("Couldn't create total_runs. Please tell me the correct per-ball total column name.")
  }
}

# Create is_wicket (from common dismissal cues)
if (!has_col(balls,"is_wicket")) {
  disp_cols <- grep("dismiss|wicket|out", names(balls), value = TRUE, ignore.case = TRUE)
  cat("\n[Dismissal-like columns]: ", paste(disp_cols, collapse=", "), "\n")
  if (has_col(balls,"player_dismissed")) {
    balls <- balls %>% dplyr::mutate(is_wicket = as.integer(!is.na(player_dismissed)))
  }
  if (!has_col(balls,"is_wicket") && has_col(balls,"dismissal_kind")) {
    balls <- balls %>% dplyr::mutate(is_wicket = as.integer(!is.na(dismissal_kind) &
                                                              !stringr::str_detect(tolower(as.character(dismissal_kind)), "retired")))
  }
  if (!has_col(balls,"is_wicket") && has_col(balls,"wicket_kind")) {
    balls <- balls %>% dplyr::mutate(is_wicket = as.integer(!is.na(wicket_kind) &
                                                              !stringr::str_detect(tolower(as.character(wicket_kind)), "retired")))
  }
  flag_candidates <- intersect(names(balls), c("is_out","out","wicket","was_wicket"))
  if (!has_col(balls,"is_wicket") && length(flag_candidates)) {
    balls <- balls %>% dplyr::rename(is_wicket = !! rlang::sym(flag_candidates[1])) %>%
      dplyr::mutate(is_wicket = as.integer(is_wicket))
  }
  if (!has_col(balls,"is_wicket")) stop("Couldn't construct is_wicket; please point me to your dismissal field.")
}
if (!has_col(balls,"is_dot")) balls <- balls %>% dplyr::mutate(is_dot = as.integer(total_runs == 0))

# Drop draws / rain-outs / bowl-offs if flagged
if (has_col(match,"outcome_type")) {
  match <- match %>%
    dplyr::filter(!str_detect(tolower(outcome_type), "draw"),
                  !str_detect(tolower(outcome_type), "rain|abandon|no.?result"),
                  !str_detect(tolower(outcome_type), "bowl"))
}

# Ensure first-innings totals exist in match (compute from balls if missing)
if (!all(c("first_innings_runs","first_innings_wkts") %in% names(match))) {
  stopifnot(all(c("innings","total_runs","is_wicket") %in% names(balls)))
  fir <- balls %>%
    dplyr::filter(innings == 1) %>%
    dplyr::group_by(match_id) %>%
    dplyr::summarise(first_innings_runs = sum(total_runs, na.rm = TRUE),
                     first_innings_wkts = sum(is_wicket,  na.rm = TRUE), .groups="drop")
  match <- match %>% dplyr::left_join(fir, by = "match_id")
}

# Create binary response: home_win (robust column detection)
cat("\n[match columns]\n"); print(names(match))
detect_first <- function(nms, patterns) {
  hits <- nms[stringr::str_detect(nms, stringr::regex(paste(patterns, collapse="|"), ignore_case = TRUE))]
  if (length(hits)) hits[1] else NA_character_
}
cols <- names(match)
home_guess <- detect_first(cols, c("^home[_ ]?team$", "home[_ ]?side", "host[_ ]?team", "homeclub", "home_name"))
team1_col  <- detect_first(cols, c("^team1$", "team_?1", "innings1_team", "side1", "bat_first_team"))
team2_col  <- detect_first(cols, c("^team2$", "team_?2", "innings2_team", "side2", "bowl_first_team"))
home_flag  <- detect_first(cols, c("^home$", "home_flag", "is_home", "home_side"))
winner_guess <- detect_first(cols, c("^result[_ ]?winner$", "^winner$", "winning[_ ]?team", "match[_ ]?winner", "winner[_ ]?team"))
winner_code  <- detect_first(cols, c("winner[_ ]?code$", "^winner_id$", "^winner_num$", "^winner_side$", "^winner_team$"))

if (is.na(winner_guess) && !is.na(winner_code) && !is.na(team1_col) && !is.na(team2_col)) {
  match <- match %>%
    dplyr::mutate(result_winner = ifelse(.data[[winner_code]] %in% c(1, "1", "team1", "home"),
                                         .data[[team1_col]],
                                         ifelse(.data[[winner_code]] %in% c(2, "2", "team2", "away"),
                                                .data[[team2_col]], NA)))
  winner_guess <- "result_winner"
}
if (is.na(home_guess) && !is.na(team1_col) && !is.na(team2_col) && !is.na(home_flag)) {
  match <- match %>%
    dplyr::mutate(home_team = ifelse(.data[[home_flag]] %in% c(1, "1", "team1", "home", TRUE),
                                     .data[[team1_col]], .data[[team2_col]]))
  home_guess <- "home_team"
}
if (is.na(home_guess)) {
  cand_home <- grep("home|host|club|team", cols, value = TRUE, ignore.case = TRUE)
  stop(paste0("Could not find a home-team column. Candidates: ", paste(cand_home, collapse=", "),
              "\nSet HOME_COL <- \"<name>\" and re-run."))
}
if (is.na(winner_guess)) {
  cand_win <- grep("winner|win|result|outcome", cols, value = TRUE, ignore.case = TRUE)
  stop(paste0("Could not find a winner column. Candidates: ", paste(cand_win, collapse=", "),
              "\nSet WINNER_COL <- \"<name>\" and re-run, or tell me a numeric winner code."))
}
if (exists("HOME_COL"))   home_guess   <- HOME_COL
if (exists("WINNER_COL")) winner_guess <- WINNER_COL

match <- match %>%
  dplyr::rename(home_team = !! rlang::sym(home_guess),
                result_winner = !! rlang::sym(winner_guess)) %>%
  dplyr::mutate(home_win = as.integer(result_winner == home_team))

cat("\n[home_win balance]\n")
print(match %>% dplyr::count(home_win) %>% dplyr::mutate(prop = n/sum(n)))

# ========== EDA ==========
cat("\n[First-innings summary]\n")
print(match %>% dplyr::summarise(
  n = dplyr::n(),
  runs_mean = mean(first_innings_runs, na.rm=TRUE),
  runs_sd   = sd(first_innings_runs, na.rm=TRUE),
  wkts_mean = mean(first_innings_wkts, na.rm=TRUE),
  wkts_sd   = sd(first_innings_wkts, na.rm=TRUE)
))

runs_bins <- match %>%
  dplyr::mutate(runs_bin = cut(first_innings_runs, breaks = pretty(first_innings_runs, n = 12), include.lowest = TRUE)) %>%
  dplyr::group_by(runs_bin) %>%
  dplyr::summarise(win_rate = mean(home_win, na.rm=TRUE),
                   mid = (as.numeric(sub("\\((.*),(.*)]","\\1", as.character(runs_bin))) +
                            as.numeric(sub("\\((.*),(.*)]","\\2", as.character(runs_bin))))/2,
                   n=dplyr::n(), .groups="drop") %>%
  dplyr::filter(!is.na(win_rate), !is.na(mid))

p1 <- ggplot(match, aes(first_innings_runs)) +
  geom_histogram(bins=40) +
  labs(title="First-innings runs — distribution", x="Runs", y="Count") +
  theme_minimal()
p2 <- ggplot(runs_bins, aes(mid, win_rate)) +
  geom_point() + geom_smooth(se=FALSE) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Home win rate vs first-innings runs (binned)", x="Runs (bin mid)", y="Win rate") +
  theme_minimal()
print(p1); print(p2)
print(p1)
# ========== STEP 3: PRE-PROCESSING ==========
# Time split (80/20 by date)
match$date <- as.Date(match$date)
split_num  <- quantile(as.numeric(match$date), probs = 0.80, na.rm = TRUE, type = 7)
split_date <- as.Date(split_num, origin = "1970-01-01")

train_df <- match %>%
  dplyr::filter(date <= split_date) %>%
  tidyr::drop_na(home_win, first_innings_runs, first_innings_wkts)
test_df  <- match %>%
  dplyr::filter(date >  split_date) %>%
  tidyr::drop_na(home_win, first_innings_runs, first_innings_wkts)

# Wickets as factor bins
wk_breaks <- c(-1, 2, 5, 7, 10)
wk_labels <- c("0-2","3-5","6-7","8-10")
train_df <- train_df %>% dplyr::mutate(wickets_cat = cut(first_innings_wkts, breaks = wk_breaks, labels = wk_labels))
test_df  <- test_df  %>% dplyr::mutate(wickets_cat = cut(first_innings_wkts, breaks = wk_breaks, labels = wk_labels))

# ========== STEP 4: BASELINE MODELS ==========
# (a) runs only
m_a <- glm(home_win ~ first_innings_runs, data=train_df, family=binomial())
pa  <- plogis(predict(m_a, test_df))
ra  <- eval_bin(pa, test_df$home_win) %>% dplyr::mutate(Model="Baseline: runs")

# (b) runs + wickets (numeric)
m_b <- glm(home_win ~ first_innings_runs + first_innings_wkts, data=train_df, family=binomial())
pb  <- plogis(predict(m_b, test_df))
rb  <- eval_bin(pb, test_df$home_win) %>% dplyr::mutate(Model="Baseline: runs + wkts (num)")

# (c) runs + wickets (factor)
m_c <- glm(home_win ~ first_innings_runs + wickets_cat, data=train_df, family=binomial())
pc  <- plogis(predict(m_c, test_df))
rc  <- eval_bin(pc, test_df$home_win) %>% dplyr::mutate(Model="Baseline: runs + wkts (factor)")

# (d) log(runs) + wickets
m_d <- glm(home_win ~ log1p(first_innings_runs) + first_innings_wkts, data=train_df, family=binomial())
pd  <- plogis(predict(m_d, test_df))
rd  <- eval_bin(pd, test_df$home_win) %>% dplyr::mutate(Model="Baseline: log(runs)+wkts")

baseline_tbl <- dplyr::bind_rows(ra, rb, rc, rd) %>% dplyr::arrange(desc(AUC))
cat("\n== Baseline comparison ==\n"); print(baseline_tbl)

# Optional: compare models via LR test vs (a)
cat("\n[LR tests vs (a) runs only]\n")
suppressWarnings({
  print(lmtest::lrtest(m_a, m_b))
  print(lmtest::lrtest(m_a, m_c))
  print(lmtest::lrtest(m_a, m_d))
})

# ---- Baseline calibration for best by AUC
best_base <- baseline_tbl$Model[which.max(baseline_tbl$AUC)]
best_p_b  <- list("Baseline: runs"=pa, "Baseline: runs + wkts (num)"=pb,
                  "Baseline: runs + wkts (factor)"=pc, "Baseline: log(runs)+wkts"=pd)[[best_base]]
print(calib_plot(best_p_b, test_df$home_win, title = paste("Calibration —", best_base)))

# ========== STEP 5: PARTIAL-STATE MODELS ==========
# Ensure boundary/dot exist
if(!has_col(balls,"is_boundary")) balls <- balls %>% dplyr::mutate(is_boundary = 0L)
if(!has_col(balls,"is_dot"))      balls <- balls %>% dplyr::mutate(is_dot      = as.integer(total_runs==0))

# Ensure 'over' is an integer index
if (!has_col(balls,"over")) stop("balls is missing an 'over' column.")
if (!is.integer(balls$over)) balls <- balls %>% dplyr::mutate(over = as.integer(floor(as.numeric(over))))

# Aggregate per over and build cumulative features
over_summ <- balls %>%
  dplyr::group_by(match_id, innings, over) %>%
  dplyr::summarise(
    runs_over = sum(total_runs, na.rm=TRUE),
    wkts_over = sum(is_wicket,  na.rm=TRUE),
    boundaries_over = sum(is_boundary, na.rm=TRUE),
    dots_over       = sum(is_dot,      na.rm=TRUE),
    balls_over      = dplyr::n(), .groups="drop"
  ) %>%
  dplyr::arrange(match_id, innings, over) %>%
  dplyr::group_by(match_id, innings) %>%
  dplyr::mutate(
    runs_cum   = cumsum(runs_over),
    wkts_cum   = cumsum(wkts_over),
    overs_faced= dplyr::row_number(),
    crr        = runs_cum / overs_faced,
    w_in_hand  = 10 - wkts_cum,
    momentum3  = zoo::rollapply(runs_over, 3, mean, align="right", fill=NA),
    boundary_rate = cumsum(boundaries_over)/cumsum(balls_over),
    dot_rate      = cumsum(dots_over)/cumsum(balls_over)
  ) %>%
  dplyr::ungroup()

# First-innings target
targets <- balls %>%
  dplyr::filter(innings==1) %>%
  dplyr::group_by(match_id) %>%
  dplyr::summarise(target = sum(total_runs, na.rm=TRUE), .groups="drop")

# Toss fields & day/night flag (one-time)
if (!has_col(match,"toss_winner"))   match$toss_winner   <- NA_character_
if (!has_col(match,"toss_decision")) match$toss_decision <- NA_character_
dn_guess <- names(match)[stringr::str_detect(names(match),
                                             stringr::regex("day.?night|daynight|\\bdn\\b|night", ignore_case = TRUE))]
dn_guess <- if (length(dn_guess)) dn_guess[1] else NA_character_
match$day_night_f <- if (!is.na(dn_guess)) {
  val <- tolower(as.character(match[[dn_guess]]))
  as01(stringr::str_detect(val, "night") | stringr::str_detect(val, "\\bdn\\b"))
} else 0L

# Context table
ctx <- match %>%
  dplyr::transmute(match_id, date, format, home_win,
                   home_team, toss_winner, toss_decision, day_night_f)

# Partial-state table
ps <- over_summ %>%
  dplyr::left_join(targets, by="match_id") %>%
  dplyr::left_join(ctx,     by="match_id") %>%
  dplyr::mutate(
    total_overs  = dplyr::if_else(format=="T20", 20L, 50L),
    overs_remain = total_overs - overs_faced,
    req_rr = dplyr::if_else(innings==2,
                            pmax((target + 1 - runs_cum) / pmax(overs_remain, 0.1), NA_real_),
                            0),
    par_gap = dplyr::if_else(innings==2,
                             runs_cum - ((overs_faced/total_overs)*(target + 1)),
                             0),
    toss_win_home = as01(toss_winner == home_team),
    toss_bat      = as01(stringr::str_to_lower(as.character(toss_decision)) == "bat")
  )

# Checkpoints (ODI 10/20/40; T20 6/10/15)
checkpoints <- tibble(format=c(rep("ODI",3),rep("T20",3)),
                      over_cp=c(10,20,40, 6,10,15))
snap <- ps %>%
  dplyr::semi_join(checkpoints, by=c("format","over"="over_cp")) %>%
  dplyr::select(match_id, date, format, innings, over, home_win,
                runs_cum, w_in_hand, crr, req_rr, momentum3,
                boundary_rate, dot_rate, par_gap,
                toss_win_home, toss_bat, day_night_f) %>%
  tidyr::drop_na(home_win, runs_cum, w_in_hand, crr)

# Time split for partial-state
tr_p <- snap %>% dplyr::filter(date <= split_date)
te_p <- snap %>% dplyr::filter(date >  split_date)

# ---- Feature prep: impute + scale (glmnet); keep unscaled copy for trees
feat_cols <- c("runs_cum","w_in_hand","crr","req_rr","momentum3",
               "boundary_rate","dot_rate","par_gap",
               "toss_win_home","toss_bat","day_night_f")
Xtr <- tr_p %>% dplyr::select(dplyr::all_of(feat_cols))
Xte <- te_p %>% dplyr::select(dplyr::all_of(feat_cols))
fix_bad <- function(df) {
  df[] <- lapply(df, function(x) { x <- as.numeric(x); x[is.infinite(x)] <- NA_real_; x[is.nan(x)] <- NA_real_; x })
  as.data.frame(df)
}
Xtr <- fix_bad(Xtr); Xte <- fix_bad(Xte)
meds <- sapply(Xtr, function(col) median(col, na.rm=TRUE))
for (nm in names(meds)) { Xtr[[nm]][is.na(Xtr[[nm]])] <- meds[[nm]]; Xte[[nm]][is.na(Xte[[nm]])] <- meds[[nm]] }
means <- sapply(Xtr, mean); sds <- sapply(Xtr, sd); sds[sds==0 | is.na(sds)] <- 1
scale_with <- function(df, m, s) as.data.frame(mapply(function(col, mm, ss) (col-mm)/ss, df, m, s, SIMPLIFY = FALSE))
Xtr_s <- scale_with(Xtr, means, sds); Xte_s <- scale_with(Xte, means, sds)
y_tr <- as01(tr_p$home_win); y_te <- as01(te_p$home_win)
x_tr <- as.matrix(Xtr_s); x_te <- as.matrix(Xte_s)

# ---- Models
# (1) Interpretable logistic (unscaled df is fine here too)
m_logit <- glm(home_win ~ runs_cum + w_in_hand + crr + req_rr + momentum3 +
                 boundary_rate + dot_rate + par_gap +
                 toss_win_home + toss_bat + day_night_f,
               data = tr_p, family = binomial())
p_logit <- plogis(predict(m_logit, te_p))
r_logit <- eval_bin(p_logit, y_te) %>% dplyr::mutate(Model="Partial-state: Logistic")

# (2) Elastic Net (scaled)
set.seed(42)
cv_en <- glmnet::cv.glmnet(x_tr, y_tr, family="binomial", alpha=0.5, nfolds=5, type.measure="deviance")
p_en  <- as.numeric(predict(cv_en, newx = x_te, s = "lambda.min", type="response"))
r_en  <- eval_bin(p_en, y_te) %>% dplyr::mutate(Model="Partial-state: Elastic Net")

# (3) Random Forest (unscaled)
set.seed(42)
rf <- randomForest::randomForest(x = Xtr, y = factor(y_tr),
                                 ntree = 500, mtry = floor(sqrt(ncol(Xtr))), nodesize = 10)
p_rf <- predict(rf, newdata = Xte, type="prob")[,2]
r_rf <- eval_bin(p_rf, y_te) %>% dplyr::mutate(Model="Partial-state: Random Forest")

# (4) XGBoost (unscaled)
set.seed(42)
dtr <- xgboost::xgb.DMatrix(data = as.matrix(Xtr), label = y_tr)
dte <- xgboost::xgb.DMatrix(data = as.matrix(Xte), label = y_te)
params <- list(objective="binary:logistic", eval_metric="logloss",
               max_depth=3, eta=0.08, subsample=0.8, colsample_bytree=0.8, min_child_weight=5)
xgbm <- xgboost::xgb.train(params, dtr, nrounds=500, verbose=0)
p_xgb <- predict(xgbm, dte)
r_xgb <- eval_bin(p_xgb, y_te) %>% dplyr::mutate(Model="Partial-state: XGBoost")

partial_tbl <- dplyr::bind_rows(r_logit, r_en, r_rf, r_xgb) %>% dplyr::arrange(desc(AUC))
cat("\n== Partial-state comparison ==\n"); print(partial_tbl)

# ---- Calibration (best partial-state)
best_idx <- which.max(c(r_logit$AUC, r_en$AUC, r_rf$AUC, r_xgb$AUC))
best_p   <- list(p_logit, p_en, p_rf, p_xgb)[[best_idx]]
best_nm  <- c("Logistic","Elastic Net","Random Forest","XGBoost")[best_idx]
print(calib_plot(best_p, y_te, title = paste("Calibration — Partial-state", best_nm)))

# ---- Save results
readr::write_csv(baseline_tbl, "baseline_results.csv")
readr::write_csv(partial_tbl,  "partial_state_results.csv")

cat("\nAll done ✅  Results saved to baseline_results.csv and partial_state_results.csv\n")

