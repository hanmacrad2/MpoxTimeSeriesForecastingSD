library(dplyr)
library(tidyr)
library(tibble)

# =============================================================================
# PENALTY SENSITIVITY for the three Lasso models, using the FINAL model calls:
#   VAR-Lasso : run_var_sd(data_year, TRAIN_WEEK, list_jur, lambda)
#   AR-Lasso  : run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 8, lambda = lambda)
#   NB        : run_nb_sd(data_mpox, 84, list_jur, lambda_value = lambda,
#                         predictor_type = "level", refit_step2 = TRUE)
# For each lambda: slope-weighted RMSE and MAE for SDC, and the number of non-zero
# coefficients per forecast origin (intercept excluded), summarised as mean and SD
# across the forecast origins (the model is refit at every origin).
# Only LAMBDA varies (the NB elastic-net mixing parameter alpha stays at 1 = pure lasso).
# Untested (no R here).
# =============================================================================

LAMBDAS <- c(0.01, 0.025, 0.05, 0.1, 0.5, 1, 5, 10, 20, 50)
TARGET  <- "SanDiego"
if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""

# Same inputs as your final NB call. If data_mpox / 84 differ from data_year /
# TRAIN_WEEK, the NB model is trained on a different window than the other two.
NB_DATA       <- data_mpox
NB_TRAIN_WEEK <- 84
print(c(TRAIN_WEEK = TRAIN_WEEK, NB_TRAIN_WEEK = NB_TRAIN_WEEK))
print(c(rows_data_year = nrow(data_year), rows_data_mpox = nrow(data_mpox)))

# -----------------------------------------------------------------------------
# Non-zero coefficient counts per forecast origin (intercept excluded):
# mean, SD, min and max ACROSS ORIGINS. Origins with no non-zero coefficients
# count as 0.
#   lag_suffix = TRUE : VAR/AR tables, predictors named like "Illinois_L24"
#                       (the intercept has no "_L<number>" suffix, so it is dropped)
#   lag_suffix = FALSE: NB table (Predictor = "Illinois", with a separate Lag column)
# -----------------------------------------------------------------------------
nz_summary <- function(coeffs, n_origins, lag_suffix = TRUE) {
  if (is.null(coeffs) || nrow(coeffs) == 0)
    return(tibble(nz_mean = 0, nz_sd = 0, nz_min = 0, nz_max = 0))
  cf <- coeffs %>% filter(!grepl("intercept", as.character(Predictor), ignore.case = TRUE))
  if (lag_suffix) cf <- cf %>% filter(grepl("_L[0-9]+$", as.character(Predictor)))
  per   <- cf %>% count(Week_Number)
  n_vec <- c(per$n, rep(0, max(0, n_origins - nrow(per))))
  tibble(nz_mean = mean(n_vec),
         nz_sd   = if (length(n_vec) > 1) sd(n_vec) else NA_real_,
         nz_min  = min(n_vec), nz_max = max(n_vec))
}

# -----------------------------------------------------------------------------
# One runner per model: lambda in -> one row (rmse, mae, nz_mean, nz_sd, nz_min, nz_max)
# -----------------------------------------------------------------------------
run_var <- function(lam) {
  res <- run_var_sd(data_year, TRAIN_WEEK, list_jur, lam)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, nz_summary(res$coeffs, nrow(res$forecasts), lag_suffix = TRUE))
}

run_ar <- function(lam) {
  res <- run_AR_model(data_year, 0, TRAIN_WEEK, TARGET, n_lags_ar = 8, lambda = lam)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, nz_summary(res$df_coeffs, nrow(res$forecasts$AR_Lasso), lag_suffix = TRUE))
}

run_nb <- function(lam) {
  res <- run_nb_sd(NB_DATA, NB_TRAIN_WEEK, list_jur, lambda_value = lam,
                   predictor_type = "level", refit_step2 = TRUE)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, nz_summary(res$coeffs, nrow(res$forecasts), lag_suffix = FALSE))
}

# -----------------------------------------------------------------------------
# STEP 1: reproduce the main-analysis numbers (lambda = 0.05) BEFORE the sweep
# -----------------------------------------------------------------------------
print(run_var(0.05), digits = 7)   # expect rmse 1.716735, mae 1.447338
print(run_ar(0.05),  digits = 7)   # expect rmse 2.529528, mae 2.066214
# print(run_nb(0.05), digits = 7)  # compare with the NB row of the final Table 1 (slow; run once)

# -----------------------------------------------------------------------------
# STEP 2: sweep (saves each model as it finishes)
# -----------------------------------------------------------------------------
run_grid <- function(fun, name) {
  rows <- lapply(LAMBDAS, function(lam) {
    message(name, ": lambda = ", lam)
    tryCatch(fun(lam), error = function(e) {
      message("   failed: ", conditionMessage(e))
      tibble(rmse = NA_real_, mae = NA_real_, nz_mean = NA_real_, nz_sd = NA_real_,
             nz_min = NA_real_, nz_max = NA_real_)
    }) %>% mutate(lambda = lam, model = name, .before = 1)
  })
  detail <- bind_rows(rows)
  saveRDS(detail, paste0(FOLDER_SAVE, "sweep_", name, ".rds"))
  detail
}

det_var <- run_grid(run_var, "VAR")
det_ar  <- run_grid(run_ar,  "AR")
det_nb  <- run_grid(run_nb,  "NB")       # slowest: 11 equations, refit at both steps

# -----------------------------------------------------------------------------
# STEP 3: tables
# -----------------------------------------------------------------------------
detail <- bind_rows(det_var, det_ar, det_nb)
write.csv(detail, paste0(FOLDER_SAVE, "Supplementary_lambda_sensitivity_detail.csv"),
          row.names = FALSE)     # numeric: rmse, mae, nz_mean, nz_sd, nz_min, nz_max

# Display table: Non-zero shown as "mean (SD)" across forecast origins
sens_table <- detail %>%
  transmute(lambda, model,
            RMSE    = round(rmse, 2),
            MAE     = round(mae, 2),
            NonZero = ifelse(is.na(nz_mean), NA_character_,
                             sprintf("%.1f (%.1f)", nz_mean, nz_sd))) %>%
  pivot_wider(names_from = model, values_from = c(RMSE, MAE, NonZero),
              names_glue = "{model}_{.value}") %>%
  select(lambda, starts_with("VAR"), starts_with("AR"), starts_with("NB"))

print(sens_table, width = Inf)
write.csv(sens_table, paste0(FOLDER_SAVE, "Supplementary_lambda_sensitivity.csv"),
          row.names = FALSE)

# Table note: slope-weighted RMSE and MAE for SDC in 2024. Non-zero = mean (SD),
# across forecast origins, of the number of non-zero coefficients in the SDC equation
# (intercept excluded). Candidates: VAR-Lasso 275 (11 series x 25 lags), AR-Lasso 8
# (own lags), negative binomial 275 (lagged log(1+y) of 11 series x 25 lags).
# Main analysis: lambda = 0.05 for all three. Penalty values are on each package's
# own scale and are not comparable across models. These results describe
# sensitivity and were not used to select the penalty.