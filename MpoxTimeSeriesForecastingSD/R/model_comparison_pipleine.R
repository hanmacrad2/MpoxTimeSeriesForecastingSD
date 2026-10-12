#RUN
xRUN_MODEL_COMPARISON <- function(data_mpox, ROLL_WINDOW, TRAIN_WEEK,
                                 list_jur, n_lags = 25, n_lags_ar = 4,
                                 title_plot = "Two-week ahead forecasts"){
  
  #-------------------------------------------------
  # PREPARE DATA
  #-------------------------------------------------
  #browser()
  data_raw <- data_mpox %>%
    filter(Jurisdiction %in% list_jur)
  
  # raw split for evaluation
  split <- get_train_test_data(data_raw, TRAIN_WEEK)
  
  train_ts <- GET_TS_DATA(split$train_data)
  future_ts <- GET_TS_DATA(split$future_data)
  
  print(paste0('BEST ROLLING WINDOW,', ROLL_WINDOW))
  
  # smoothed split for model fitting
  if(ROLL_WINDOW != 0){
    
    data_model <- GET_SMOOTH_DATA_WEIGHTED(data_raw, ROLL_WINDOW, weight_type = weight_type)  # <-- CHANGED
    #data_model <- GET_SMOOTH_DATA(data_raw, ROLL_WINDOW)
    
  } else {
    
    data_model <- data_raw
    
  }
  
  split_smooth <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts_smooth <- GET_TS_DATA(split_smooth$train_data)
  future_ts_smooth <- GET_TS_DATA(split_smooth$future_data)
  
  #-------------------------------------------------
  # RUN ALL MODELS
  #-------------------------------------------------
  
  ## VAR-Lasso
  forecasts_var <- VAR_LASSO_FORECAST_TWO_STEP(
    train_ts_smooth,
    future_ts_smooth,
    future_ts,
    ROLL_WINDOW,
    n_lags = n_lags
  )
  
  forecasts_var = forecasts_var$df_pred_results
  
  ## AR-Lasso
  forecasts_ar <- AR_model_jurs(
    train_ts_smooth,
    future_ts_smooth,
    future_ts,
    list_jur, 
    n_lags_ar,
    n_step_ahead = 2
  )
  
  
  ##Forecasts ARIMA
  forecasts_arima <- ARIMA_model_jurs(
    train_ts_smooth,
    future_ts_smooth,
    future_ts,
    list_jur, 
    n_step_ahead = 2
  )
  
  ## Negative Binomial autoregressio
  forecasts_nb <- NEG_BIN_FORECAST(
    train_ts_smooth,
    future_ts_smooth,
    future_ts,
    ROLL_WINDOW,
    n_lags = n_lags,
    protected_predictors = list(San_Diego = "Illinois_lag25")   
  )
  
  forecasts_nb = forecasts_nb$results_df
  
  ## Naive moving average
  forecasts_naive <- SIMPLE_NAIVE_FORECAST(
    train_ts_smooth,
    future_ts_smooth,
    future_ts,
    n_step_ahead = 2
  )
  forecasts_naive = forecasts_naive$df_pred_results
  
  #-------------------------------------------------
  # COMPUTE METRICS
  #-------------------------------------------------
  
  print('VAR Lasso model')
  metrics_var <- GET_METRICS(
    forecasts_var
  )
  
  print('AR Lasso model')
  metrics_ar <- GET_METRICS(
    forecasts_ar
  )
  
  print('Arima model')
  metrics_arima <- GET_METRICS(
    forecasts_arima
  )
  
  print('Negative binomial model')
  metrics_nb <- GET_METRICS(
    forecasts_nb
  )
  
  print('Naive model')
  metrics_naive <- GET_METRICS(
    forecasts_naive
  )
  
  #-------------------------------------------------
  # SLOPE WEIGHTED METRICS
  #-------------------------------------------------
  # Slope weighted metric
  df_slope <- GET_DF_WEIGHTED_SLOPE(
    data_model,
    list_jur
  )
  
  print('Slope weighted metrics')
  print('VAR Lasso model')
  slope_metrics_var <- GET_SLOPE_WEIGHTED_METRICS(
    df_slope,
    forecasts_var
  )
  
  print('AR Lasso model')
  slope_metrics_ar <- GET_SLOPE_WEIGHTED_METRICS(
    df_slope,
    forecasts_ar
  )
  
  print('ARIMA  model')
  slope_metrics_arima <- GET_SLOPE_WEIGHTED_METRICS(
    df_slope, forecasts_arima)
  
  print('Neg Bin Lasso model')
  slope_metrics_nb <- GET_SLOPE_WEIGHTED_METRICS(
    df_slope,
    forecasts_nb
  )
  
  print('Naive model')
  slope_metrics_naive <- GET_SLOPE_WEIGHTED_METRICS(
    df_slope,
    forecasts_naive
  )
  
  
  #-------------------------------------------------
  # RETURN RESULTS
  #-------------------------------------------------
  
  metrics <- rbind(
    
    data.frame(
      Model = "VAR-Lasso",
      RMSE = metrics_var$rmse,
      slope_weighted_RMSE = slope_metrics_var$slope_weighted_rmse,
      MAE = metrics_var$mae,
      slope_weighted_MAE = slope_metrics_var$slope_weighted_mae,
      bias = metrics_var$bias,
      slope_weighted_bias = slope_metrics_var$slope_weighted_bias
    ),
    
    data.frame(
      Model = "AR-Lasso",
      RMSE = metrics_ar$rmse,
      slope_weighted_RMSE = slope_metrics_ar$slope_weighted_rmse,
      MAE = metrics_ar$mae,
      slope_weighted_MAE = slope_metrics_ar$slope_weighted_mae,
      bias = metrics_ar$bias,
      slope_weighted_bias = slope_metrics_ar$slope_weighted_bias
    ),
    
    data.frame(
      Model = "ARIMA",
      RMSE = metrics_arima$rmse,
      slope_weighted_RMSE = slope_metrics_arima$slope_weighted_rmse,
      MAE = metrics_arima$mae,
      slope_weighted_MAE = slope_metrics_arima$slope_weighted_mae,
      bias = metrics_arima$bias,
      slope_weighted_bias = slope_metrics_arima$slope_weighted_bias
    ),
    
    data.frame(
      Model = "Negative-Binomial-AR",
      RMSE = metrics_nb$rmse,
      slope_weighted_RMSE = slope_metrics_nb$slope_weighted_rmse,
      MAE = metrics_nb$mae,
      slope_weighted_MAE = slope_metrics_nb$slope_weighted_mae,
      bias = metrics_nb$bias,
      slope_weighted_bias = slope_metrics_nb$slope_weighted_bias
    ),
    
    data.frame(
      Model = "Naive",
      RMSE = metrics_naive$rmse,
      slope_weighted_RMSE = slope_metrics_naive$slope_weighted_rmse,
      MAE = metrics_naive$mae,
      slope_weighted_MAE = slope_metrics_naive$slope_weighted_mae,
      bias = metrics_naive$bias,
      slope_weighted_bias = slope_metrics_naive$slope_weighted_bias
    )
    
  )
  
  
  PLOT_FORECASTS(forecasts_var,
                 forecasts_ar,
                 forecasts_arima,
                 forecasts_nb,
                 forecasts_naive, list_jur, title_plot)
  
  forecasts = list(
    VAR_Lasso = forecasts_var,
    AR_Lasso = forecasts_ar,
    ARIMA = forecasts_arima,
    Negative_Binomial_AR = forecasts_nb,
    Naive = forecasts_naive
  )
  
  return(list(forecasts = forecasts, metrics = metrics))
  
}


RUN_MODEL_COMPARISON <- function(data_mpox, ROLL_WINDOW, TRAIN_WEEK,
                                 list_jur, n_lags = 25, n_lags_ar = 4,
                                 weight_type = "flat",   # <-- NEW: "flat" | "sawtooth" | "normal"
                                 title_plot = "Two-week ahead forecasts"){
  
  #-------------------------------------------------
  # PREPARE DATA
  #-------------------------------------------------
  data_raw <- data_mpox %>%
    filter(Jurisdiction %in% list_jur)
  
  # raw split for evaluation
  split <- get_train_test_data(data_raw, TRAIN_WEEK)
  
  train_ts <- GET_TS_DATA(split$train_data)
  future_ts <- GET_TS_DATA(split$future_data)
  
  print(paste0('ROLL_WINDOW = ', ROLL_WINDOW, ', weight_type = ', weight_type))
  
  # smoothed split for model fitting
  if(ROLL_WINDOW != 0){
    
    data_model <- GET_SMOOTH_DATA_WEIGHTED(data_raw, ROLL_WINDOW, weight_type = weight_type)  # <-- CHANGED
    
  } else {
    
    data_model <- data_raw
    
  }
  
  split_smooth <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts_smooth <- GET_TS_DATA(split_smooth$train_data)
  future_ts_smooth <- GET_TS_DATA(split_smooth$future_data)
  
  #-------------------------------------------------
  # RUN ALL MODELS  (unchanged below this point)
  #-------------------------------------------------
  
  forecasts_var <- VAR_LASSO_FORECAST_TWO_STEP(
    train_ts_smooth, future_ts_smooth, future_ts, ROLL_WINDOW, n_lags = n_lags
  )
  forecasts_var = forecasts_var$df_pred_results
  
  forecasts_ar <- AR_model_jurs(
    train_ts_smooth, future_ts_smooth, future_ts, list_jur, n_lags_ar, n_step_ahead = 2
  )
  
  forecasts_arima <- ARIMA_model_jurs(
    train_ts_smooth, future_ts_smooth, future_ts, list_jur, n_step_ahead = 2
  )
  
  forecasts_nb <- NEG_BIN_FORECAST(
    train_ts_smooth, future_ts_smooth, future_ts, ROLL_WINDOW, n_lags = n_lags,
    protected_predictors = list(San_Diego = "Illinois_lag25")
  )
  forecasts_nb = forecasts_nb$results_df
  
  forecasts_naive <- SIMPLE_NAIVE_FORECAST(
    train_ts_smooth, future_ts_smooth, future_ts, n_step_ahead = 2
  )
  forecasts_naive = forecasts_naive$df_pred_results
  
  #-------------------------------------------------
  # METRICS  (unchanged)
  #-------------------------------------------------
  metrics_var   <- GET_METRICS(forecasts_var)
  metrics_ar    <- GET_METRICS(forecasts_ar)
  metrics_arima <- GET_METRICS(forecasts_arima)
  metrics_nb    <- GET_METRICS(forecasts_nb)
  metrics_naive <- GET_METRICS(forecasts_naive)
  
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, list_jur)
  
  slope_metrics_var    <- GET_SLOPE_WEIGHTED_METRICS(df_slope, forecasts_var)
  slope_metrics_ar     <- GET_SLOPE_WEIGHTED_METRICS(df_slope, forecasts_ar)
  slope_metrics_arima  <- GET_SLOPE_WEIGHTED_METRICS(df_slope, forecasts_arima)
  slope_metrics_nb     <- GET_SLOPE_WEIGHTED_METRICS(df_slope, forecasts_nb)
  slope_metrics_naive  <- GET_SLOPE_WEIGHTED_METRICS(df_slope, forecasts_naive)
  
  metrics <- rbind(
    data.frame(Model = "VAR-Lasso", RMSE = metrics_var$rmse,
               slope_weighted_RMSE = slope_metrics_var$slope_weighted_rmse,
               MAE = metrics_var$mae, slope_weighted_MAE = slope_metrics_var$slope_weighted_mae,
               bias = metrics_var$bias, slope_weighted_bias = slope_metrics_var$slope_weighted_bias),
    data.frame(Model = "AR-Lasso", RMSE = metrics_ar$rmse,
               slope_weighted_RMSE = slope_metrics_ar$slope_weighted_rmse,
               MAE = metrics_ar$mae, slope_weighted_MAE = slope_metrics_ar$slope_weighted_mae,
               bias = metrics_ar$bias, slope_weighted_bias = slope_metrics_ar$slope_weighted_bias),
    data.frame(Model = "ARIMA", RMSE = metrics_arima$rmse,
               slope_weighted_RMSE = slope_metrics_arima$slope_weighted_rmse,
               MAE = metrics_arima$mae, slope_weighted_MAE = slope_metrics_arima$slope_weighted_mae,
               bias = metrics_arima$bias, slope_weighted_bias = slope_metrics_arima$slope_weighted_bias),
    data.frame(Model = "Negative-Binomial-AR", RMSE = metrics_nb$rmse,
               slope_weighted_RMSE = slope_metrics_nb$slope_weighted_rmse,
               MAE = metrics_nb$mae, slope_weighted_MAE = slope_metrics_nb$slope_weighted_mae,
               bias = metrics_nb$bias, slope_weighted_bias = slope_metrics_nb$slope_weighted_bias),
    data.frame(Model = "Naive", RMSE = metrics_naive$rmse,
               slope_weighted_RMSE = slope_metrics_naive$slope_weighted_rmse,
               MAE = metrics_naive$mae, slope_weighted_MAE = slope_metrics_naive$slope_weighted_mae,
               bias = metrics_naive$bias, slope_weighted_bias = slope_metrics_naive$slope_weighted_bias)
  )
  
  PLOT_FORECASTS(forecasts_var, forecasts_ar, forecasts_arima, forecasts_nb, forecasts_naive,
                 list_jur, title_plot)
  
  forecasts = list(
    VAR_Lasso = forecasts_var, AR_Lasso = forecasts_ar, ARIMA = forecasts_arima,
    Negative_Binomial_AR = forecasts_nb, Naive = forecasts_naive
  )
  
  return(list(forecasts = forecasts, metrics = metrics))
}

