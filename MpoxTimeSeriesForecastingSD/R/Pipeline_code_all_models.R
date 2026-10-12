library(dplyr)

# ============================================================
# San Diego-only model comparison, in the same pattern as before:
# each model returns $overall (one row), then bind_rows().
#
#   - VAR-Lasso and Negative Binomial are fitted on ALL of list_jur
#     (San Diego + its correlated jurisdictions) -- they are multivariate.
#     Only San Diego's row is kept for the metrics.
#   - AR-Lasso, ARIMA and Naive are univariate: use your existing
#     run_univariate_models() with just "SanDiego".
#
# Needs: your existing RUN_VAR, RUN_NEG_BIN (with the no-fallback
# NEG_BIN_FORECAST loaded) and run_univariate_models.
# ============================================================

run_var_sd <- function(data_mpox, TRAIN_WEEK, list_jur, lambda_value,
                       jurisdiction_interest = "SanDiego",
                       correlation_threshold = NA, n_lags = 25) {
  
  if (!(jurisdiction_interest %in% list_jur)) stop(jurisdiction_interest, " is not in list_jur")
  if (length(list_jur) < 2) stop("list_jur has no predictors -- VAR-Lasso needs at least one.")
  
  # Your RUN_VAR, unchanged: VAR-Lasso is fitted on ALL of list_jur
  result <- RUN_VAR(jurisdiction_interest = jurisdiction_interest, data_mpox = data_mpox,
                    TRAIN_WEEK = TRAIN_WEEK, list_jur = list_jur,
                    correlation_threshold = correlation_threshold,
                    ROLL_WINDOW = 0, n_lags = n_lags)
  
  overall <- result$jur_level_metrics %>%
    filter(Jurisdiction == jurisdiction_interest) %>%
    dplyr::select(-Jurisdiction)
  
  coeffs <- result$results_var$df_coeffs
  if (!is.null(coeffs) && "Response" %in% names(coeffs)) {
    coeffs <- coeffs %>% filter(Response == jurisdiction_interest)
  }
  
  list(overall = overall,
       forecasts = result$var_forecasts %>% filter(Jurisdiction == jurisdiction_interest),
       coeffs = coeffs,
       result = result)
}

run_univariate_models <- function(data_mpox, ROLL_WINDOW, TRAIN_WEEK,
                                  jurs_selection, n_lags_ar = 25,
                                  weight_type = "flat") {
  
  data_raw <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  split <- get_train_test_data(data_raw, TRAIN_WEEK)
  future_ts <- GET_TS_DATA(split$future_data)
  
  if (ROLL_WINDOW != 0) {
    data_model <- GET_SMOOTH_DATA_WEIGHTED(data_raw, ROLL_WINDOW, weight_type = weight_type)
  } else {
    print('NO SMOOOOOOOOOOOOOTHING!')
    data_model <- data_raw
  }
  split_smooth <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts_smooth <- GET_TS_DATA(split_smooth$train_data)
  future_ts_smooth <- GET_TS_DATA(split_smooth$future_data)
  
  results_AR <- AR_model_jurs(train_ts_smooth, future_ts_smooth, future_ts, jurs_selection, n_lags_ar, n_step_ahead = 2)
  forecasts_ar = results_AR$forecasts_ar
  forecasts_arima <- ARIMA_model_jurs(train_ts_smooth, future_ts_smooth, future_ts, jurs_selection, n_step_ahead = 2)
  # NB is no longer run here: it now uses each jurisdiction's own predictor set
  # (see run_neg_bin_all_jurs below), so it is threshold-dependent like VAR.
  forecasts_naive <- SIMPLE_NAIVE_FORECAST(train_ts_smooth, future_ts_smooth, future_ts, n_step_ahead = 2)
  forecasts_naive <- forecasts_naive$df_pred_results
  
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  jur_metrics_all <- bind_rows(
    build_jur_metrics(forecasts_ar, "AR-Lasso"),
    build_jur_metrics(forecasts_arima, "ARIMA"),
    build_jur_metrics(forecasts_naive, "Naive")
  ) %>% dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = list(AR_Lasso = forecasts_ar, ARIMA = forecasts_arima, Naive = forecasts_naive))
}

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

# % improvement of VAR-Lasso over each other model: (other - VAR) / other * 100.
# Absolute values, so bias is compared by size (as in the original paper).
# Positive = VAR-Lasso better.
pct_improve_vs_var <- function(comparison,
                               metric_cols = c("rmse", "mae", "slope_weighted_rmse",
                                               "slope_weighted_mae", "slope_weighted_bias")) {
  var_row <- comparison %>% filter(Model == "VAR-Lasso")
  
  comparison %>%
    filter(Model != "VAR-Lasso") %>%
    transmute(Model,
              across(all_of(metric_cols),
                     ~ (abs(.x) - abs(var_row[[cur_column()]])) / abs(.x) * 100,
                     .names = "pct_improve_{.col}"))
}

# The five models must be scored on the same forecast weeks
check_same_weeks <- function(forecast_list) {
  n_rows <- sapply(forecast_list, nrow)
  if (length(unique(n_rows)) > 1) {
    warning("Different numbers of forecast rows: ", paste(names(n_rows), n_rows, collapse = ", "))
  }
  if (all(sapply(forecast_list, function(x) "Week_Number" %in% names(x)))) {
    wk <- lapply(forecast_list, function(x) sort(unique(x$Week_Number)))
    if (!all(sapply(wk, identical, wk[[1]]))) warning("The models forecast different weeks.")
  }
  invisible(n_rows)
}

plot_sd_five_models <- function(var_sd, nb_sd, univariate_results, title_plot,
                                jurisdiction_interest = "SanDiego") {
  uf <- lapply(univariate_results$forecasts, function(x) x %>% filter(Jurisdiction == jurisdiction_interest))
  check_same_weeks(list(VAR = var_sd$forecasts, AR = uf$AR_Lasso, ARIMA = uf$ARIMA,
                        NB = nb_sd$forecasts, Naive = uf$Naive))
  PLOT_FORECASTS_SINGLE_JURISDICITON(var_sd$forecasts, uf$AR_Lasso, uf$ARIMA,
                                     nb_sd$forecasts, uf$Naive, title_plot)
}

# Print the train and test windows -- compare with the methods text
print_split_check <- function(data_mpox, TRAIN_WEEK, list_jur) {
  d <- data_mpox %>% filter(Jurisdiction %in% list_jur)
  split <- get_train_test_data(d, TRAIN_WEEK)
  tr <- GET_TS_DATA(split$train_data)
  fu <- GET_TS_DATA(split$future_data)
  cat("Train:", format(min(as.Date(tr$date_week_start))), "to", format(max(as.Date(tr$date_week_start))),
      "(", nrow(tr), "weeks )  |  Test:", format(min(as.Date(fu$date_week_start))), "to",
      format(max(as.Date(fu$date_week_start))), "(", nrow(fu), "weeks )\n")
}
