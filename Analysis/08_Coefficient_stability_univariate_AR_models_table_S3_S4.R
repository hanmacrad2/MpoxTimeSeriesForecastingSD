library(dplyr)

# Run first (as you already do):
# nb_out   <- run_nb_sd(data_year, 84, n_lags = 8, lambda = 0.1)
# ar_model <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 8, lambda = 0.1)

# ---- Collect non-zero SDC lag coefficients per forecast origin ----
# AR-Lasso: Predictor looks like "SanDiego_L3"; drop any intercept rows
ar_cf <- ar_model$df_coeffs %>%
  mutate(Predictor = as.character(Predictor)) %>%
  filter(grepl("_L[0-9]+$", Predictor)) %>%
  transmute(Week_Number, Lag = as.integer(sub(".*_L", "", Predictor)), Coefficient)

# NB AR-Lasso: already has Lag, Coefficient, Week_Number (non-zero only)
nb_cf <- nb_out$coeffs %>%
  transmute(Week_Number, Lag = as.integer(Lag), Coefficient)

# Number of forecast origins (should be 45)
n_ar <- length(unique(ar_model$forecasts$Week_Number))
n_nb <- length(unique(nb_out$forecasts$Week_Number))

# ---- Stability table: one row per lag ----
lag_stability <- function(cf, n_origins, lags = 1:8) {
  # lag with the largest positive coefficient at each origin
  top <- cf %>%
    group_by(Week_Number) %>%
    filter(Coefficient == max(Coefficient), Coefficient > 0) %>%
    slice(1) %>% ungroup() %>% count(Lag, name = "n_top")
  
  bind_rows(lapply(lags, function(l) {
    v <- cf$Coefficient[cf$Lag == l]
    n_top <- top$n_top[top$Lag == l]; if (length(n_top) == 0) n_top <- 0
    tibble(Lag = l,
           Mean   = if (length(v)) mean(v)   else NA_real_,
           Median = if (length(v)) median(v) else NA_real_,
           SD     = if (length(v) > 1) sd(v) else NA_real_,
           Min    = if (length(v)) min(v)    else NA_real_,
           Max    = if (length(v)) max(v)    else NA_real_,
           Pct_nonzero = 100 * length(v) / n_origins,
           Pct_largest_positive = 100 * n_top / n_origins)
  }))
}

ar_tab <- lag_stability(ar_cf, n_ar)
nb_tab <- lag_stability(nb_cf, n_nb)

options(width = 200)
cat("\nAR-Lasso (origins =", n_ar, ")\n");               print(ar_tab, n = Inf)
cat("\nNegative binomial AR-Lasso (origins =", n_nb, ")\n"); print(nb_tab, n = Inf)

write.csv(ar_tab, "stability_AR_Lasso.csv", row.names = FALSE)
write.csv(nb_tab, "stability_NB_AR_Lasso.csv", row.names = FALSE)

#*******************************
#Negative Binomial Dispersion parameter

library(dplyr)

# nb_out <- run_nb_sd(data_year, 84, n_lags = 8, lambda = 0.1)

theta_by_origin <- nb_out$coeffs %>%
  group_by(Week_Number) %>%
  summarise(Theta = first(Theta), .groups = "drop")   # one theta per origin

n_origins <- length(unique(nb_out$forecasts$Week_Number))   # 45
cat("Origins with a saved theta:", nrow(theta_by_origin), "of", n_origins, "\n")

theta_summary <- theta_by_origin %>%
  summarise(Median = median(Theta), Mean = mean(Theta), SD = sd(Theta),
            Min = min(Theta), Max = max(Theta))
print(as.data.frame(theta_summary))
print(as.data.frame(summary(theta_by_origin$Theta)))

write.csv(theta_by_origin, "NB_theta_by_origin.csv", row.names = FALSE)