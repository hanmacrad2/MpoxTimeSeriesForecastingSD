library(dplyr)

# Uses the final NB code (with NB_FIT_SERIES / refit at step 2) already loaded.
LAMBDAS <- c(0.01, 0.025, 0.05, 0.1, 0.5, 1, 5, 10, 20, 50)   # same grid as the other two models

run_one <- function(lam) {
  cat("\n=== NB lambda =", lam, "===\n")
  out <- tryCatch(
    run_nb_sd(data_year, 84, n_lags = 8, lambda = lam),
    error = function(e) { message("Failed at lambda ", lam, ": ", e$message); NULL })
  if (is.null(out)) {
    return(tibble(lambda = lam, slope_weighted_rmse = NA_real_,
                  slope_weighted_mae = NA_real_, mean_nonzero = NA_real_))
  }
  n_orig <- nrow(out$forecasts)                       # forecast origins (should be 45)
  # coeffs holds only NON-zero lag coefficients, so origins with none have no rows
  mean_nz <- if (is.null(out$coeffs) || nrow(out$coeffs) == 0) 0 else nrow(out$coeffs) / n_orig
  tibble(lambda = lam,
         slope_weighted_rmse = out$overall$slope_weighted_rmse,
         slope_weighted_mae  = out$overall$slope_weighted_mae,
         mean_nonzero        = mean_nz,
         n_origins           = n_orig)
}

nb_sens <- bind_rows(lapply(LAMBDAS, run_one))
options(width = 200)
print(nb_sens %>% mutate(across(where(is.numeric), ~ round(.x, 2))), n = Inf)
#write.csv(nb_sens, "sensitivity_NB_univariate.csv", row.names = FALSE)