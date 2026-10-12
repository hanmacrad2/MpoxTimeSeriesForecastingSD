#*********************
#* NAIVE ESTIMATE
#* ********************

SIMPLE_NAIVE_FORECAST <- function(train_data_ts, future_data_ts, future_real_ts, n_step_ahead = 2) {
  
  'Simple Naive estimate. Calculates 4 week average of weekly count data and projects n_step_ahead in weeks (2 weeks)'
  # Remove non-jurisdictional columns
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
    
    # Get last observed value at time t
    last_obs <- as.numeric(tail(train_data_ts, 1))
    
    #STEP AHEAD: Actual values at t + n
    actual_values <- as.numeric(future_real_ts[week_t + (n_step_ahead - 1), ])
    
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + (n_step_ahead - 1)], length(juris_names)),
      date_week_start = rep(date_week_start_list[week_t + (n_step_ahead - 1)], length(juris_names)),
      Jurisdiction = juris_names,
      Predicted = last_obs,
      Actual = actual_values
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # Extend the training set forward by 1 week
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
