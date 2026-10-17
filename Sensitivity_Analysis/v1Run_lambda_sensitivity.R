library(dplyr)
library(tidyr)
library(tibble)

# =============================================================================
# LAMBDA SENSITIVITY TABLE: same lambdas for VAR-Lasso, AR-Lasso, NB
# Per lambda and model: slope-weighted RMSE, MAE, and number of non-zero
# coefficients (per forecast origin, SDC equation). Untested (no R here).
# Needs loaded: data_year, TRAIN_WEEK, list_jur, run_var_sd(), AR_model(),
#   run_nb_sd(), EXTRACT_NONZERO_LASSO_COEFFS(), get_train_test_data(),
#   GET_TS_DATA(), GET_DF_WEIGHTED_SLOPE(), GET_SLOPE_METRICS_JURISDICTION()
# =============================================================================

DATA    <- data_year
LAMBDAS <- c(0.025, 0.05, 0.1, 0.5, 1, 5, 10, 20, 50)
N_LAGS  <- 25
TARGET  <- "SanDiego"
NB_PRED_TYPE <- "level"      # negative binomial: lagged log(1+y) LEVELS only
if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""
print(TRAIN_WEEK)            # must be the SAME for all three models (your NB call used 84)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
data_model <- DATA %>% filter(Jurisdiction %in% list_jur)
split      <- get_train_test_data(data_model, TRAIN_WEEK)
train_ts   <- GET_TS_DATA(split$train_data)
future_ts  <- GET_TS_DATA(split$future_data)
df_slope   <- GET_DF_WEIGHTED_SLOPE(data_model, list_jur)

# slope-weighted RMSE/MAE for SDC (used for the AR model; VAR and NB use your own wrappers)
score_sd <- function(fc) {
  fc <- fc %>% filter(Jurisdiction == TARGET)
  GET_SLOPE_METRICS_JURISDICTION(fc, df_slope, TARGET) %>%
    filter(Jurisdiction == TARGET) %>%
    transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
}

# Number of non-zero coefficients per forecast origin (intercept excluded).
# `coeffs` = the non-zero-coefficient table your functions return (one row per
# non-zero coefficient per origin, with a Week_Number column); origins with no
# non-zero coefficients are counted as 0.
nz_summary <- function(coeffs, n_origins) {
  if (is.null(coeffs) || nrow(coeffs) == 0)
    return(tibble(nz_mean = 0, nz_min = 0, nz_max = 0))
  cf  <- coeffs %>% filter(!grepl("intercept", as.character(Predictor), ignore.case = TRUE))
  per <- cf %>% count(Week_Number)
  n_vec <- c(per$n, rep(0, max(0, n_origins - nrow(per))))
  tibble(nz_mean = mean(n_vec), nz_min = min(n_vec), nz_max = max(n_vec))
}

# -----------------------------------------------------------------------------
# One runner per model: lambda in -> one row (rmse, mae, nz_mean, nz_min, nz_max)
# -----------------------------------------------------------------------------
run_var <- function(lam) {
  res <- run_var_sd(DATA, TRAIN_WEEK, list_jur, lam)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, nz_summary(res$coeffs, nrow(res$forecasts)))
}

tr_sd <- train_ts[,  c("date_week_start", "Week_Number", TARGET)]
fu_sd <- future_ts[, c("date_week_start", "Week_Number", TARGET)]
run_ar <- function(lam) {
  res <- AR_model(tr_sd, fu_sd, fu_sd, jur_name = TARGET, n_lags = N_LAGS, lambda = lam)
  bind_cols(score_sd(res$forecasts_ar_model),
            nz_summary(res$df_coeffs, nrow(res$forecasts_ar_model)))
}

run_nb <- function(lam) {
  res <- run_nb_sd(DATA, TRAIN_WEEK, list_jur, lambda_value = lam,
                   predictor_type = NB_PRED_TYPE)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, nz_summary(res$coeffs, nrow(res$forecasts)))
}

# -----------------------------------------------------------------------------
# STEP 1: quick checks BEFORE the sweep
# -----------------------------------------------------------------------------
v <- run_var(0.05); print(v, digits = 7)   # expect rmse 1.716735, mae 1.447338
# See how BigVAR names the intercept in your coefficient table (nz_summary drops it):
chkv <- run_var_sd(DATA, TRAIN_WEEK, list_jur, 0.05)
print(head(unique(as.character(chkv$coeffs$Predictor))))
# NB at 0.1 with level predictors: compare with the NB row of Table 1 if that run
# is the one Table 1 came from (earlier Table 1 NB: 2.662726 / 2.239108).
# n <- run_nb(0.1); print(n, digits = 7)

# -----------------------------------------------------------------------------
# STEP 2: sweep (saves each model as it finishes)
# -----------------------------------------------------------------------------
run_grid <- function(fun, name) {
  rows <- lapply(LAMBDAS, function(lam) {
    message(name, ": lambda = ", lam)
    tryCatch(fun(lam), error = function(e) {
      message("   failed: ", conditionMessage(e))
      tibble(rmse = NA_real_, mae = NA_real_, nz_mean = NA_real_,
             nz_min = NA_real_, nz_max = NA_real_)
    }) %>% mutate(lambda = lam, model = name, .before = 1)
  })
  detail <- bind_rows(rows)
  saveRDS(detail, paste0(FOLDER_SAVE, "sweep_", name, ".rds"))
  detail
}

det_var <- run_grid(run_var, "VAR")
det_ar  <- run_grid(run_ar,  "AR")
det_nb  <- run_grid(run_nb,  "NB")       # slowest (11 equations per origin)

# -----------------------------------------------------------------------------
# STEP 3: tables
# -----------------------------------------------------------------------------
detail <- bind_rows(det_var, det_ar, det_nb)
write.csv(detail, paste0(FOLDER_SAVE, "Supplementary_lambda_sensitivity_detail.csv"),
          row.names = FALSE)               # includes min / max non-zero per origin

sens_table <- detail %>%
  transmute(lambda, model, RMSE = round(rmse, 2), MAE = round(mae, 2),
            NonZero = round(nz_mean, 1)) %>%
  pivot_wider(names_from = model, values_from = c(RMSE, MAE, NonZero),
              names_glue = "{model}_{.value}") %>%
  select(lambda, starts_with("VAR"), starts_with("AR"), starts_with("NB"))

print(sens_table)
write.csv(sens_table, paste0(FOLDER_SAVE, "Supplementary_lambda_sensitivity.csv"),
          row.names = FALSE)

# Table note: NonZero = mean number of non-zero coefficients per forecast origin in
# the SDC equation (intercept excluded). Candidates: VAR 275 (11 series x 25 lags),
# AR 25, NB 275 (lagged log(1+y) levels of 11 series x 25 lags). Main analysis used
# VAR lambda = 0.05; penalties are on each package's own scale and are not
# comparable across models. These results describe sensitivity; they were not used
# to select the penalty.