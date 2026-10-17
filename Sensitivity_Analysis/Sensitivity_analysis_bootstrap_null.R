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

# =============================================================================
# 2. MOVING-BLOCK BOOTSTRAP (resamples blocks of consecutive weeks, all series together)
# =============================================================================
mbb_idx <- function(n, block) {
  nb     <- ceiling(n / block)
  starts <- sample.int(n - block + 1, nb, replace = TRUE)
  idx    <- unlist(lapply(starts, function(s) s:(s + block - 1)))
  idx[seq_len(n)]
}

boot_run <- function(fit_fun, Y, terms, B) {
  out <- matrix(NA_real_, B, length(terms), dimnames = list(NULL, terms))
  for (b in seq_len(B)) {
    Yb <- Y[mbb_idx(nrow(Y), BLOCK), , drop = FALSE]
    nz <- tryCatch(fit_fun(Yb), error = function(e) NULL)
    if (!is.null(nz)) out[b, ] <- get_or(nz, terms)
    if (b %% 25 == 0) message("bootstrap replicate ", b, " / ", B)
  }
  out[complete.cases(out), , drop = FALSE]
}

summ_boot <- function(mat, obs_nz, model) {
  tibble(model = model, term = colnames(mat),
         observed        = get_or(obs_nz, colnames(mat)),
         pct_nonzero     = 100 * colMeans(mat != 0),
         boot_median     = apply(mat, 2, median),
         boot_lo_2.5     = apply(mat, 2, quantile, probs = 0.025),
         boot_hi_97.5    = apply(mat, 2, quantile, probs = 0.975),
         n_replicates    = nrow(mat))
}

cat("\n--- 2. Moving-block bootstrap (block =", BLOCK, "weeks, B =", B_BOOT, ") ---\n")
bs_var <- boot_run(function(Yb) fit_bigvar_coefs(Yb, N_LAGS_VAR, LAMBDA), Yd_var, ILL_TERMS, B_BOOT)
bs_nb  <- boot_run(function(Yb) fit_nb_coefs(Yb, N_LAGS_VAR, LAMBDA),     Y_nb,   ILL_TERMS, B_BOOT)
bs_ar  <- boot_run(function(Yb) fit_bigvar_coefs(Yb, N_LAGS_AR, LAMBDA),  Yd_ar,  AR_TERMS,  B_BOOT)

boot_all <- bind_rows(summ_boot(bs_var, obs_var, "VAR-Lasso"),
                      summ_boot(bs_nb,  obs_nb,  "Negative binomial"),
                      summ_boot(bs_ar,  obs_ar,  "AR-Lasso"))
print(boot_all, n = Inf)
write.csv(boot_all, paste0(FOLDER_SAVE, "stability_bootstrap.csv"), row.names = FALSE)
# NOTE: percentile intervals include the replicates where a term was removed (coefficient 0).
# Penalised estimates are biased toward zero, so these intervals describe the STABILITY of the
# estimates; they are not confidence intervals for the true coefficients.

# =============================================================================
# 3. PERMUTATION NULL: circularly shift the Illinois series (keeps its own
#    autocorrelation, breaks its alignment with San Diego), refit, compare.
# =============================================================================
circ_shift <- function(x, k) { n <- length(x); x[((seq_len(n) - 1 - k) %% n) + 1] }

perm_run <- function(fit_fun, Y, B) {
  n   <- nrow(Y)
  res <- matrix(NA_real_, B, length(ILL_TERMS), dimnames = list(NULL, ILL_TERMS))
  for (b in seq_len(B)) {
    k  <- sample(MIN_SHIFT:(n - MIN_SHIFT), 1)
    Yp <- Y
    Yp[, "Illinois"] <- circ_shift(Y[, "Illinois"], k)
    nz <- tryCatch(fit_fun(Yp), error = function(e) NULL)
    if (!is.null(nz)) res[b, ] <- get_or(nz, ILL_TERMS)
    if (b %% 25 == 0) message("permutation ", b, " / ", B)
  }
  res[complete.cases(res), , drop = FALSE]
}

perm_summary <- function(null_mat, obs_nz, model) {
  obs      <- get_or(obs_nz, ILL_TERMS); names(obs) <- ILL_TERMS
  stat_obs <- max(obs)                                   # strongest positive Illinois term, lags 23-25
  stat_null <- apply(null_mat, 1, max)
  tibble(model = model,
         observed_L24 = obs["Illinois_L24"],
         null_median_L24 = median(null_mat[, "Illinois_L24"]),
         null_95th_L24   = quantile(null_mat[, "Illinois_L24"], 0.95),
         p_L24 = (1 + sum(null_mat[, "Illinois_L24"] >= obs["Illinois_L24"])) / (nrow(null_mat) + 1),
         observed_max_23_25 = stat_obs,
         null_median_max    = median(stat_null),
         null_95th_max      = quantile(stat_null, 0.95),
         p_max_23_25 = (1 + sum(stat_null >= stat_obs)) / (nrow(null_mat) + 1),
         pct_null_with_any_Illinois_term = 100 * mean(rowSums(null_mat != 0) > 0),
         n_permutations = nrow(null_mat))
}

cat("\n--- 3. Circular-shift permutation null (B =", B_PERM, ") ---\n")
pm_var <- perm_run(function(Yp) fit_bigvar_coefs(Yp, N_LAGS_VAR, LAMBDA), Yd_var, B_PERM)  # shift of the differenced series
pm_nb  <- perm_run(function(Yp) fit_nb_coefs(Yp, N_LAGS_VAR, LAMBDA),     Y_nb,   B_PERM)  # shift of the level series
perm_all <- bind_rows(perm_summary(pm_var, obs_var, "VAR-Lasso"),
                      perm_summary(pm_nb,  obs_nb,  "Negative binomial"))
print(perm_all)
write.csv(perm_all, paste0(FOLDER_SAVE, "stability_permutation.csv"), row.names = FALSE)
# p_max_23_25: how often a shifted Illinois series gives a lag 23-25 coefficient as large as the
# observed one. It tests the coefficient GIVEN the penalty (lambda was fixed by the hypothesis).
# The AR-Lasso has no Illinois term, so it has no permutation comparison here.

# =============================================================================
# 4. SENSITIVITY TO LAG ORDER (coefficients, 2023 training fit)
#    Maximum lags below 23 cannot contain the Illinois lag-23 to 25 terms: a negative control.
# =============================================================================
lag_order_table <- function(fit_fun, orders, model) {
  bind_rows(lapply(orders, function(p) {
    nz <- tryCatch(fit_fun(p), error = function(e) NULL)
    if (is.null(nz)) return(NULL)
    pos <- nz[nz > 0]
    top <- if (length(pos)) names(pos)[which.max(pos)] else NA_character_
    ill <- if (p >= 23) get_or(nz, ILL_TERMS) else rep(NA_real_, 3)
    tibble(model = model, max_lag = p, n_nonzero = length(nz),
           Illinois_L23 = ill[1], Illinois_L24 = ill[2], Illinois_L25 = ill[3],
           largest_positive_term = top,
           largest_positive_coef = if (!is.na(top)) unname(pos[top]) else NA_real_)
  }))
}

lag_var <- lag_order_table(function(p) fit_bigvar_coefs(Yd_var, p, LAMBDA), LAG_ORDERS_VAR, "VAR-Lasso")
lag_nb  <- lag_order_table(function(p) fit_nb_coefs(Y_nb, p, LAMBDA),       LAG_ORDERS_VAR, "Negative binomial")
lag_ar  <- lag_order_table(function(p) fit_bigvar_coefs(Yd_ar, p, LAMBDA),  LAG_ORDERS_AR,  "AR-Lasso")
lag_all <- bind_rows(lag_var, lag_nb, lag_ar)
cat("\n--- 4. Lag-order sensitivity (2023 fit) ---\n")
print(lag_all, n = Inf)
write.csv(lag_all, paste0(FOLDER_SAVE, "stability_lag_order.csv"), row.names = FALSE)

# ---- 4b. (optional, slow) forecast accuracy for each maximum lag ----------------
if (RUN_FORECAST_LAG_SENS) {
  one <- function(model, p, r) tibble(model = model, max_lag = p,
                                      slope_rmse = r$overall$slope_weighted_rmse,
                                      slope_mae  = r$overall$slope_weighted_mae)
  fl <- bind_rows(
    lapply(LAG_ORDERS_VAR, function(p) one("VAR-Lasso", p,
                                           run_var_sd(data_year, TRAIN_WEEK, list_jur, LAMBDA, n_lags = p))),
    lapply(LAG_ORDERS_AR, function(p) one("AR-Lasso", p,
                                          run_AR_model(data_year, 0, TRAIN_WEEK, TARGET, n_lags_ar = p, lambda = LAMBDA))),
    lapply(LAG_ORDERS_VAR, function(p) one("Negative binomial", p,
                                           run_nb_sd(NB_DATA, NB_TRAIN_WEEK, list_jur, lambda_value = LAMBDA,
                                                     predictor_type = "level", refit_step2 = TRUE, n_lags = p))))
  print(fl, n = Inf)
  write.csv(fl, paste0(FOLDER_SAVE, "stability_lag_order_forecasts.csv"), row.names = FALSE)
}

# =============================================================================
# 5. (optional, slow) FORECAST-LEVEL NULL: rerun the VAR-Lasso with the Illinois
#    series circularly shifted and compare slope-weighted RMSE with the real run.
#    Assumes data_year has columns Jurisdiction, Week_Number and Cases.
# =============================================================================
shift_illinois <- function(df, k) {
  idx <- which(df$Jurisdiction == "Illinois")
  idx <- idx[order(df$Week_Number[idx])]
  df$Cases[idx] <- circ_shift(df$Cases[idx], k)
  df
}

if (RUN_FORECAST_NULL) {
  n_ill <- sum(data_year$Jurisdiction == "Illinois")
  null_rmse <- vapply(seq_len(B_NULL), function(b) {
    k <- sample(MIN_SHIFT:(n_ill - MIN_SHIFT), 1)
    r <- run_var_sd(shift_illinois(data_year, k), TRAIN_WEEK, list_jur, LAMBDA)
    r$overall$slope_weighted_rmse
  }, numeric(1))
  real_rmse <- var_sd$overall$slope_weighted_rmse
  cat("\n--- 5. Forecast-level null (Illinois shifted), B =", B_NULL, "---\n")
  cat("Real slope-weighted RMSE:", round(real_rmse, 3),
      "| null mean:", round(mean(null_rmse), 3),
      "| null range:", round(min(null_rmse), 3), "to", round(max(null_rmse), 3),
      "| p (null <= real):", round((1 + sum(null_rmse <= real_rmse)) / (B_NULL + 1), 3), "\n")
  write.csv(tibble(replicate = seq_len(B_NULL), null_rmse = null_rmse),
            paste0(FOLDER_SAVE, "stability_forecast_null.csv"), row.names = FALSE)
}