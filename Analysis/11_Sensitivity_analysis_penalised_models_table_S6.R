library(dplyr)
library(tidyr)
library(tibble)

# =============================================================================
# PENALTY SENSITIVITY for the three penalised models, using the FINAL model calls:
#   VAR-Lasso       : run_var_sd(data_year, TRAIN_WEEK, list_jur, lambda)
#   AR-Lasso        : run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego",
#                                  n_lags_ar = 8, lambda = lambda)
#   Neg Bin AR Lasso: run_nb_sd(data_year, 84, n_lags = 8, lambda = lambda)
#
# For each lambda: slope-weighted RMSE and MAE for SDC, and the number of
# non-zero coefficients per forecast origin (intercept excluded), summarised
# as mean and SD across origins (the model is refit at every origin).
# Only LAMBDA varies. The NB mixing parameter alpha stays at 1 (pure lasso).
# =============================================================================

LAMBDAS <- c(0.01, 0.025, 0.05, 0.1, 0.5, 1, 5, 10, 20, 50)
TARGET  <- "SanDiego"
if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""

NB_TRAIN_WEEK <- 84
NB_N_LAGS     <- 8       

print(c(TRAIN_WEEK = TRAIN_WEEK, NB_TRAIN_WEEK = NB_TRAIN_WEEK))


# -----------------------------------------------------------------------------
# Non-zero coefficient counts per forecast origin (intercept excluded):
# mean, SD, min and max ACROSS ORIGINS. Origins with none count as 0.
#   lag_suffix = TRUE : VAR/AR tables, predictors named like "Illinois_L24"
#   lag_suffix = FALSE: NB table (Predictor = "SanDiego", separate Lag column)
# -----------------------------------------------------------------------------
non_zero_summary <- function(coeffs, n_origins, lag_suffix = TRUE) {
  
  if (is.null(coeffs) || nrow(coeffs) == 0)
    return(tibble(non_zero_mean = 0, non_zero_sd = 0, non_zero_min = 0, non_zero_max = 0))
  
  cf <- coeffs %>% filter(!grepl("intercept", as.character(Predictor), ignore.case = TRUE))
  if (lag_suffix) cf <- cf %>% filter(grepl("_L[0-9]+$", as.character(Predictor)))
  
  per   <- cf %>% count(Week_Number)
  counts <- c(per$n, rep(0, max(0, n_origins - nrow(per))))
  
  tibble(non_zero_mean = mean(counts),
         non_zero_sd   = if (length(counts) > 1) sd(counts) else NA_real_,
         non_zero_min  = min(counts),
         non_zero_max  = max(counts))
}


# -----------------------------------------------------------------------------
# One runner per model: lambda in -> one row
# -----------------------------------------------------------------------------
run_var <- function(lam) {
  res <- run_var_sd(data_year, TRAIN_WEEK, list_jur, lam)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, non_zero_summary(res$coeffs, nrow(res$forecasts), lag_suffix = TRUE))
}

run_ar <- function(lam) {
  res <- run_AR_model(data_year, 0, TRAIN_WEEK, TARGET, n_lags_ar = 8, lambda = lam)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  bind_cols(m, non_zero_summary(res$df_coeffs, nrow(res$forecasts$AR_Lasso), lag_suffix = TRUE))
}

run_nb <- function(lam) {
  res <- run_nb_sd(data_year, NB_TRAIN_WEEK, n_lags = NB_N_LAGS, lambda = lam)
  m   <- res$overall %>% transmute(rmse = slope_weighted_rmse, mae = slope_weighted_mae)
  # the univariate NB table has Predictor = "SanDiego" and a separate Lag column
  bind_cols(m, non_zero_summary(res$coeffs, nrow(res$forecasts), lag_suffix = FALSE))
}


# -----------------------------------------------------------------------------
# STEP 1: reproduce the main-analysis numbers (lambda = 0.1) BEFORE the sweep
# -----------------------------------------------------------------------------
print(run_var(0.1), digits = 7)   # compare with the VAR row of Table 2 (1.80, 1.51)
print(run_ar(0.1),  digits = 7)   # compare with the AR row  of Table 2 (2.53, 2.07)
print(run_nb(0.1),  digits = 7)   # compare with the NB row  of Table 2 (2.71, 2.26)


# -----------------------------------------------------------------------------
# STEP 2: sweep (saves each model as it finishes)
# -----------------------------------------------------------------------------
run_grid <- function(fun, name) {
  
  rows <- lapply(LAMBDAS, function(lam) {
    
    message(name, ": lambda = ", lam)
    
    tryCatch(fun(lam), error = function(e) {
      message("   failed: ", conditionMessage(e))
      tibble(rmse = NA_real_, mae = NA_real_, non_zero_mean = NA_real_,
             non_zero_sd = NA_real_, non_zero_min = NA_real_, non_zero_max = NA_real_)
    }) %>% mutate(lambda = lam, model = name, .before = 1)
  })
  
  detail <- bind_rows(rows)
  saveRDS(detail, paste0(FOLDER_SAVE, "sweep_", name, ".rds"))
  detail
}

det_var <- run_grid(run_var, "VAR")
det_ar  <- run_grid(run_ar,  "AR")
det_nb  <- run_grid(run_nb,  "NB")


# -----------------------------------------------------------------------------
# STEP 3: tables
# -----------------------------------------------------------------------------
detail <- bind_rows(det_var, det_ar, det_nb)
write.csv(detail, paste0(FOLDER_SAVE, "Supplementary_lambda_sensitivity_detail.csv"),
          row.names = FALSE)

sens_table <- detail %>%
  transmute(lambda, model,
            RMSE    = round(rmse, 2),
            MAE     = round(mae, 2),
            Non_Zero = ifelse(is.na(non_zero_min), NA_character_,
                              ifelse(non_zero_min == non_zero_max,
                                     sprintf("%d", non_zero_min),
                                     sprintf("%d-%d", non_zero_min, non_zero_max)))) %>%
  pivot_wider(names_from = model, values_from = c(RMSE, MAE, Non_Zero),
              names_glue = "{model}_{.value}") %>%
  dplyr::select(lambda, starts_with("VAR"), starts_with("AR"), starts_with("NB"))

print(sens_table, width = Inf)
write.csv(sens_table, paste0(FOLDER_SAVE, "Supplementary_lambda_sensitivity.csv"),
          row.names = FALSE)
