#NAIVE MODEL


# =============================================================================
# Naive benchmarks on RAW weekly counts, no smoothing:

#   window = 1 -> last-week (persistence) forecast
# The forecast made at origin T (training data end at week T) is scored against
# week T + n_step_ahead. Only observed counts enter the average (never forecasts).
# Untested (no R here).
# =============================================================================

NAIVE_PERSISTANCE_MODEL <- function(train_data_ts, future_data_ts, future_real_ts,
                                  n_step_ahead = 2, window = 1) {
  
  'Naive (persistence) forecast: the last observed weekly count is carried forward n_step_ahead weeks.'
  week_number_forecast <- future_data_ts$Week_Number
  date_week_start_list <- as.Date(future_data_ts$date_week_start)
  future_real_ts <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  # Remove Week_Number and date_week_start from train
  train_data_ts <- train_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  juris_names <- colnames(train_data_ts)
  n_forecasts <- nrow(future_data_ts) - (n_step_ahead - 1)
  
  results_df <- data.frame()
  
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    
    # Mean of the last `window` OBSERVED weeks at the forecast origin
    forecast_level <- as.numeric(colMeans(tail(train_data_ts, window)))
    
    #STEP AHEAD: Actual values at t + n
    actual_values <- as.numeric(future_real_ts[week_t + (n_step_ahead - 1), ])
    
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + (n_step_ahead - 1)], length(juris_names)),
      date_week_start = rep(date_week_start_list[week_t + (n_step_ahead - 1)], length(juris_names)),
      Jurisdiction = juris_names,
      Predicted = forecast_level,
      Actual = actual_values
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # Extend the training set forward by 1 observed week
    train_data_ts <- rbind(train_data_ts, future_data_ts[week_t, juris_names])
  }
  
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  
  # Reshape
  df_pred <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Predicted) %>%
    arrange(Week_Number)
  df_true <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Actual) %>%
    arrange(Week_Number)
  
  return(list(
    df_pred_results = results_df,
    preds = df_pred,
    true = df_true
  ))
}

# =============================================================================
# Wrapper (same pattern as run_AR_model / run_ARIMA_model; no ROLL_WINDOW argument)
# =============================================================================
run_naive_model <- function(data_mpox, TRAIN_WEEK, jurs_selection,
                            window = 4, n_step_ahead = 2,
                            model_label = "4-week moving average") {
  
  data_model <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)   # raw counts
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  res <- NAIVE_PERSISTANCE_MODEL(train_ts, future_ts, future_ts,
                               n_step_ahead = n_step_ahead, window = window)
  forecasts_naive <- res$df_pred_results %>% filter(Jurisdiction %in% jurs_selection)
  
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  jur_metrics_all <- build_jur_metrics(forecasts_naive, model_label) %>%
    dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = list(Naive = forecasts_naive))
}
