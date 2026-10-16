library(dplyr)
library(tibble)
library(BigVAR)

# =============================================================================
# 2023-ONLY FIT: which coefficients survive, and does Illinois at lag 23/24/25
# make the cut, as lambda varies? One fit per model per lambda on the 2023
# training data (no 2024 data, no forecasting loop). Untested (no R here).
# Needs your existing: data_year, list_jur, GET_TS_DATA(),
#   EXTRACT_NONZERO_LASSO_COEFFS(), nb_fit_all()  (the negative binomial file)
# =============================================================================

TARGET   <- "SanDiego"
N_LAGS   <- 25
ILL_LAGS <- 23:25
LAMBDAS_VAR <- c(0.01, 0.025, 0.05, 0.1, 0.5, 1, 5, 10, 20)          # BigVAR scale
LAMBDAS_NB  <- c(0.01, 0.025, 0.05, 0.1, 0.5, 1, 5, 10, 20) # mpath scale (your NB tuning grid, extended)
NB_PRED_TYPE <- "level"        # lagged log(1+y) levels, as in the main NB run
ILL_PREFIX_NB <- if (NB_PRED_TYPE == "diff") "Illinois_dL" else "Illinois_L"

# ---- 2023-only data ---------------------------------------------------------
data_2023 <- data_year %>% filter(Year == 2023, Jurisdiction %in% list_jur)
train_ts  <- GET_TS_DATA(data_2023)
Y_all     <- train_ts %>% select(-Week_Number, -date_week_start) %>% as.matrix()
stopifnot("Illinois" %in% colnames(Y_all), TARGET %in% colnames(Y_all))
cat("2023 weeks used:", nrow(Y_all), "| series:", ncol(Y_all), "\n")

# ---- summarise one fit from its NON-ZERO coefficients -------------------------
# nz: named numeric vector of the non-zero SDC-equation coefficients,
#     names like "Illinois_L24"
RANK_BY <- "positive"    # "positive" = largest positive coefficient first
# "abs"      = largest absolute value (the old behaviour)

summarise_fit <- function(model, lam, nz, n_candidates, ill_prefix = "Illinois_L") {
  get_coef <- function(k) {
    v <- nz[paste0(ill_prefix, k)]
    if (length(v) == 0 || is.na(v)) 0 else unname(v)
  }
  ill <- vapply(ILL_LAGS, get_coef, numeric(1))
  names(ill) <- paste0("Illinois_L", ILL_LAGS)
  
  # rank by signed value (largest positive = rank 1) or by absolute value
  score <- if (RANK_BY == "positive") nz else abs(nz)
  rnk   <- if (length(nz) > 0) rank(-score, ties.method = "min") else numeric(0)
  ill_rank <- vapply(ILL_LAGS, function(k) {
    nm <- paste0(ill_prefix, k)
    if (nm %in% names(nz)) as.integer(rnk[nm]) else NA_integer_
  }, integer(1))
  
  # largest term: the largest POSITIVE coefficient (or largest |coef| if RANK_BY = "abs")
  cand <- if (RANK_BY == "positive") nz[nz > 0] else nz
  top  <- if (length(cand) > 0) names(cand)[which.max(if (RANK_BY == "positive") cand else abs(cand))]
  else NA_character_
  
  tibble(model = model, lambda = lam,
         n_candidates = n_candidates, n_nonzero = length(nz),
         !!!as.list(ill),
         illinois_in_model = any(ill != 0),
         best_illinois_rank = if (all(is.na(ill_rank))) NA_integer_ else min(ill_rank, na.rm = TRUE),
         largest_term = top,
         largest_coef = if (!is.na(top)) unname(nz[top]) else NA_real_)
}

# ---- VAR-Lasso (BigVAR, differenced counts, one fit on all of 2023) -----------
fit_var <- function(lam) {
  Yd <- diff(Y_all)
  m  <- constructModel(Yd, p = N_LAGS, struct = "Basic", gran = c(lam),
                       ownlambdas = TRUE, T1 = floor(nrow(Yd) * 0.65), verbose = FALSE)
  cv <- cv.BigVAR(m)
  cf <- EXTRACT_NONZERO_LASSO_COEFFS(coef(cv), colnames(Y_all)) %>%
    filter(Response == TARGET,
           grepl("_L[0-9]+$", as.character(Predictor)))      # drops the intercept
  nz <- setNames(cf$Coefficient, as.character(cf$Predictor))
  summarise_fit("VAR-Lasso", lam, nz, n_candidates = ncol(Y_all) * N_LAGS)
}

# ---- Negative binomial (your nb_fit_all, same settings as the main run) ------
fit_nb <- function(lam) {
  fits <- nb_fit_all(Y_all, N_LAGS, lam, alpha = 1, protected = list(),
                     predictor_type = NB_PRED_TYPE)
  b  <- fits[[TARGET]]$beta
  nz <- b[b != 0]
  summarise_fit("Negative binomial", lam, nz, n_candidates = length(b),
                ill_prefix = ILL_PREFIX_NB)
}

safe <- function(f, lam) tryCatch(f(lam), error = function(e) {
  message("lambda ", lam, " failed: ", conditionMessage(e)); NULL })

out <- bind_rows(
  lapply(LAMBDAS_VAR, function(l) safe(fit_var, l)),
  lapply(LAMBDAS_NB,  function(l) safe(fit_nb,  l))
)

options(width = 200)
print(out, n = Inf)
write.csv(out, "illinois_lags_2023_fit.csv", row.names = FALSE)

# How to read:
#  n_nonzero          coefficients kept in the SDC equation (intercept excluded)
#  Illinois_L23/24/25 fitted coefficient (0 = removed by the penalty)
#  illinois_in_model  TRUE if any of lags 23-25 survives
#  best_illinois_rank rank of the best Illinois term by |coefficient| among kept terms
#                     (1 = largest coefficient in the equation)
#  VAR coefficients are on the scale of weekly changes; NB coefficients are on the
#  log scale of lagged log(1+y); the two are not comparable with each other.