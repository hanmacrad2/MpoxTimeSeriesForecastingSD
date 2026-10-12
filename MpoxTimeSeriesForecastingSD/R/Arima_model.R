#ARIMA MODEL
library(forecast)


ARIMA_model <- function(train_data_ts_jur1,
                        future_data_ts_jur1,
                        future_real_ts_jur1,
                        jur_name,
                        n_step_ahead = 2){
  
  #-------------------------------------------------
  # PREPARE
  #-------------------------------------------------
  
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  
  # keep only the jurisdiction column
  train_week_t <- train_data_ts_jur1[[jur_name]]
  
  future_values <- future_data_ts_jur1[[jur_name]]
  
  future_real <- future_real_ts_jur1[[jur_name]]
  
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  
  results_df <- data.frame()
  
  #-------------------------------------------------
  # ROLLING FORECASTS
  #-------------------------------------------------
  
  for(week_t in seq_len(n_forecasts)){
    
    print(paste0("week_t: ", week_t))
    
    #################################################
    ## STEP 1
    #################################################
    
    fit <- auto.arima(
      train_week_t,
      seasonal = FALSE,
      stepwise = TRUE,
      approximation = FALSE
    )
    
    pred1 <- as.numeric(forecast(fit, h = 1)$mean)
    
    #################################################
    ## STEP 2 (recursive)
    #################################################
    
    extended_series <- c(train_week_t, pred1)
    
    fit2 <- auto.arima(
      extended_series,
      seasonal = FALSE,
      stepwise = TRUE,
      approximation = FALSE
    )
    
    pred2 <- as.numeric(forecast(fit2, h = 1)$mean)
    
    #################################################
    ## SAVE RESULT
    #################################################
    
    actual_value <- future_real[week_t + 1]
    
    df_t <- data.frame(
      Week_Number     = week_number_forecast[week_t + 1],
      date_week_start = list_date_week_start[week_t + 1],
      Jurisdiction    = jur_name,
      Predicted       = max(pred2, 0),
      Actual          = actual_value
    )
    
    results_df <- dplyr::bind_rows(results_df, df_t)
    
    #################################################
    ## EXPAND TRAINING SET WITH OBSERVED WEEK
    #################################################
    
    train_week_t <- c(train_week_t, future_values[week_t])
    
  }
  
  results_df$error <- results_df$Actual - results_df$Predicted
  
  return(results_df)
}

ARIMA_model_jurs <- function(train_data_ts, future_data_ts, future_real_ts, list_jur,
                             n_step_ahead = 2) {
  
  'Simple Naive estimate. Calculates 4 week average of weekly count data and projects n_step_ahead in weeks (2 weeks)'
  df_results_ar = data.frame()
  
  for (jur_name in list_jur) {
    
    print(jur_name)
    train_data_ts_jur1 = train_data_ts[, c('date_week_start', 'Week_Number', jur_name)]
    future_data_ts_jur1 = future_data_ts[, c('date_week_start', 'Week_Number', jur_name)]
    future_real_ts_jur1 = future_real_ts[, c('date_week_start', 'Week_Number', jur_name)] # %>% filter(Jurisdiction == jur_name)
    df_result_jur = ARIMA_model(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                                jur_name)
    df_results_ar = rbind(df_results_ar, df_result_jur)
    
  }
  
  return(df_results_ar)
}

#RUN ARIMA 
RUN_ARIMA <- function(data_mpox, ROLL_WINDOW, TRAIN_WEEK, FORECAST_END_WEEK, list_jur){
  
  #-------------------------------------------------
  # PREPARE DATA
  #-------------------------------------------------
  
  print(paste0('Roll window: ', ROLL_WINDOW))
  data_mpox <- data_mpox %>%
    filter(Jurisdiction %in% list_jur)
  
  split <- get_train_test_data(data_mpox, TRAIN_WEEK)
  
  train_ts  <- GET_TS_DATA(split$train_data)
  future_ts <- GET_TS_DATA(split$future_data)
  
  #SMOOTH
  if(ROLL_WINDOW != 0){
    
    data_model <- GET_SMOOTH_DATA(data_mpox, ROLL_WINDOW) 
    
  } 
  
  split_smooth <- get_train_test_data(data_model, TRAIN_WEEK)
  
  train_ts_smooth  <- GET_TS_DATA(split_smooth$train_data)
  future_ts_smooth <- GET_TS_DATA(split_smooth$future_data)
  
  #-------------------------------------------------
  # RUN
  #-------------------------------------------------
  
  ## VAR-Lasso
  forecasts_arima <- ARIMA_model_jurs(
    train_ts_smooth,
    future_ts_smooth,
    future_ts,
    list_jur)
  
  metrics_arima <- GET_METRICS(forecasts_arima)
  
  print(metrics_arima)
  
  #PLOT
  # data_24_forecast_start = data_mpox_model %>% filter(Week_Number >= TRAIN_WEEK)
  # data_24_forecast_start = data_24_forecast_start %>% filter(Week_Number <= WEEK_END)
  # data_24_ts_forecast_start = GET_TS_DATA(data_24_forecast_start)
  
  title_plot = 'ARIMA Rolling window 5'
  PLOT_CASES_FORECASTS(future_ts, forecasts_arima, forecasts_arima, forecasts_arima, list_jur, title_plot)
  
  return(forecasts_arima)
  
}




#RUN BEST
#TRAIN_WEEK = 87
#BEST_ROLL_WINDOW = 5
#arima_results = RUN_ARIMA(data_val_fit, BEST_ROLL_WINDOW,
#                       TRAIN_WEEK, FORECAST_END_WEEK, list_jur)
