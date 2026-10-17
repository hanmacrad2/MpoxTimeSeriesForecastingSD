library(dplyr)
library(tidyr)
library(tibble)
library(BigVAR)

# =============================================================================
# COEFFICIENT STABILITY for the three Lasso models (VAR-Lasso, AR-Lasso, NB)
# Answers the reviewer's four requests:
#   1. stability across rolling fits       -> uses the coefficient tables of the FINAL runs
#   2. bootstrap intervals                 -> moving-block bootstrap of the 2023 training data
#   3. permutation / null comparisons      -> circular shift of the Illinois series
#   4. sensitivity to lag order            -> refits with smaller maximum lags
# Untested (no R here). Needs your existing objects and functions:
#   var_sd, nb_glm_out, ar_model            (outputs of the final model runs)
#   data_year, data_mpox, TRAIN_WEEK, list_jur
#   get_train_test_data(), GET_TS_DATA(), EXTRACT_NONZERO_LASSO_COEFFS(),
#   nb_build_X(), nb_fit_one()               (negative binomial file)
#   run_var_sd(), run_AR_model(), run_nb_sd()  (only for the optional forecast-level sections)
# =============================================================================

set.seed(123)
TARGET     <- "SanDiego"
LAMBDA     <- 0.05                 # main-analysis penalty, all three models
N_LAGS_VAR <- 25                   # VAR-Lasso and negative binomial
N_LAGS_AR  <- 8                    # AR-Lasso
ILL_LAGS   <- 23:25
ILL_TERMS  <- paste0("Illinois_L", ILL_LAGS)
AR_TERMS   <- paste0(TARGET, "_L", seq_len(N_LAGS_AR))

B_BOOT    <- 200                   # bootstrap replicates (raise to 1000 for the final run)
B_PERM    <- 200                   # permutation replicates
BLOCK     <- 8                     # bootstrap block length in weeks
MIN_SHIFT <- 6                     # smallest circular shift (weeks) for the permutation null

LAG_ORDERS_VAR <- c(5, 10, 15, 20, 25)   # lag-order sensitivity, VAR-Lasso and NB
LAG_ORDERS_AR  <- c(1, 2, 4, 8, 12)      # lag-order sensitivity, AR-Lasso

RUN_FORECAST_LAG_SENS <- FALSE     # TRUE = also rerun the full rolling forecasts per lag order (slow)
RUN_FORECAST_NULL     <- FALSE     # TRUE = rerun VAR-Lasso forecasts with Illinois shifted (slow)
B_NULL                <- 20

NB_DATA       <- data_mpox         # same inputs as the final NB call
NB_TRAIN_WEEK <- 84
if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""
options(width = 200)

# =============================================================================
# 1. STABILITY ACROSS ROLLING FITS (no refitting: uses the final runs' tables)
# =============================================================================
# Unify term names: VAR/AR tables have Predictor = "Illinois_L24"; the NB table has
# Predictor = "Illinois" plus Lag and Type columns. Intercept rows are dropped.
with_term <- function(cf) {
  cf <- as_tibble(cf)
  if ("Lag" %in% names(cf)) {
    cf <- cf %>% mutate(term = paste0(as.character(Predictor), "_",
                                      ifelse(Type == "growth", "dL", "L"), Lag))
  } else {
    cf <- cf %>% mutate(term = as.character(Predictor))
  }
  cf %>% filter(grepl("_d?L[0-9]+$", term))
}

# For each term: how often non-zero across origins, size and sign stability.
# Origins where the term (or every term) was removed count as 0.
stability_table <- function(cf, n_orig, terms) {
  cf <- with_term(cf)
  bind_rows(lapply(terms, function(tm) {
    v     <- cf %>% filter(term == tm) %>% pull(Coefficient)
    v_all <- c(v, rep(0, max(0, n_orig - length(v))))
    tibble(term = tm,
           pct_origins_nonzero = 100 * length(v) / n_orig,
           median_when_nonzero = if (length(v)) median(v) else NA_real_,
           min_when_nonzero    = if (length(v)) min(v)    else NA_real_,
           max_when_nonzero    = if (length(v)) max(v)    else NA_real_,
           mean_all_origins    = mean(v_all),
           sd_all_origins      = sd(v_all),
           sign_flips          = if (length(v)) sum(sign(v) != sign(median(v))) else 0L)
  }))
}

# Share of the total absolute coefficient weight carried by Illinois terms, and
# whether an Illinois term is the largest positive coefficient, per origin.
illinois_share <- function(cf) {
  with_term(cf) %>%
    group_by(Week_Number) %>%
    summarise(share = sum(abs(Coefficient)[grepl("^Illinois_", term)]) / sum(abs(Coefficient)),
              illinois_is_top = grepl("^Illinois_", term[which.max(Coefficient)]),
              .groups = "drop")
}

n_var <- nrow(var_sd$forecasts)
n_nb  <- nrow(nb_glm_out$forecasts)
n_ar  <- nrow(ar_model$forecasts$AR_Lasso)

roll_var <- stability_table(var_sd$coeffs,       n_var, ILL_TERMS) %>% mutate(model = "VAR-Lasso", .before = 1)
roll_nb  <- stability_table(nb_glm_out$coeffs,   n_nb,  ILL_TERMS) %>% mutate(model = "Negative binomial", .before = 1)
roll_ar  <- stability_table(ar_model$df_coeffs,  n_ar,  AR_TERMS)  %>% mutate(model = "AR-Lasso", .before = 1)
roll_all <- bind_rows(roll_var, roll_nb, roll_ar)
cat("\n--- 1. Stability across rolling fits (Illinois lags 23-25; AR own lags) ---\n")
print(roll_all, n = Inf)

share_var <- illinois_share(var_sd$coeffs)
share_nb  <- illinois_share(nb_glm_out$coeffs)
share_summary <- bind_rows(
  tibble(model = "VAR-Lasso",         mean_share = mean(share_var$share), min_share = min(share_var$share),
         max_share = max(share_var$share), pct_origins_illinois_top = 100 * mean(share_var$illinois_is_top),
         origins_with_any_terms = nrow(share_var), n_origins = n_var),
  tibble(model = "Negative binomial", mean_share = mean(share_nb$share),  min_share = min(share_nb$share),
         max_share = max(share_nb$share),  pct_origins_illinois_top = 100 * mean(share_nb$illinois_is_top),
         origins_with_any_terms = nrow(share_nb), n_origins = n_nb))
cat("\n--- 1b. Share of coefficient weight on Illinois terms, and Illinois as the largest positive term ---\n")
print(share_summary)

write.csv(roll_all,      paste0(FOLDER_SAVE, "stability_rolling_fits.csv"), row.names = FALSE)
write.csv(share_summary, paste0(FOLDER_SAVE, "stability_illinois_share.csv"), row.names = FALSE)

# =============================================================================
# 2-4 need refits on the 2023 TRAINING data (the first forecast origin)
# =============================================================================
prep_levels <- function(df, train_week) {
  d  <- df %>% filter(Jurisdiction %in% list_jur)
  sp <- get_train_test_data(d, train_week)
  GET_TS_DATA(sp$train_data) %>% dplyr::select(-Week_Number, -date_week_start) %>% as.matrix()
}
Y_var  <- prep_levels(data_year, TRAIN_WEEK)          # levels, all jurisdictions
Y_nb   <- prep_levels(NB_DATA, NB_TRAIN_WEEK)
Yd_var <- diff(Y_var)                                  # differenced, as fitted by the VAR-Lasso
Yd_ar  <- diff(Y_var[, TARGET, drop = FALSE])          # differenced SDC series for the AR-Lasso
stopifnot("Illinois" %in% colnames(Y_var), TARGET %in% colnames(Y_var))
cat("\nTraining weeks (VAR/AR):", nrow(Y_var), "| (NB):", nrow(Y_nb), "\n")

# Fit functions: return the named vector of NON-ZERO SDC-equation coefficients
fit_bigvar_coefs <- function(Yd, p, lam) {
  m  <- constructModel(Yd, p = p, struct = "Basic", gran = c(lam), ownlambdas = TRUE,
                       T1 = floor(nrow(Yd) * 0.65), verbose = FALSE)
  cv <- cv.BigVAR(m)
  cf <- EXTRACT_NONZERO_LASSO_COEFFS(coef(cv), colnames(Yd)) %>%
    filter(Response == TARGET, grepl("_L[0-9]+$", as.character(Predictor)))   # drops intercept
  setNames(cf$Coefficient, as.character(cf$Predictor))
}

fit_nb_coefs <- function(Y, p, lam) {     # SDC equation only (faster than fitting all 11)
  n <- nrow(Y)
  X <- nb_build_X(Y, p, "level")
  f <- nb_fit_one(X, Y[(p + 2):n, TARGET], lam, alpha = 1)
  b <- f$beta
  b[b != 0]
}

get_or <- function(nz, terms, default = 0) {
  v <- nz[terms]; v[is.na(v)] <- default; unname(v)
}

obs_var <- fit_bigvar_coefs(Yd_var, N_LAGS_VAR, LAMBDA)
obs_ar  <- fit_bigvar_coefs(Yd_ar,  N_LAGS_AR,  LAMBDA)
obs_nb  <- fit_nb_coefs(Y_nb, N_LAGS_VAR, LAMBDA)
cat("Observed 2023 fits at lambda =", LAMBDA, "| non-zero terms: VAR", length(obs_var),
    "AR", length(obs_ar), "NB", length(obs_nb), "\n")
