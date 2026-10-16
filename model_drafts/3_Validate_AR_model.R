#RUN AR MODEL

run_AR_model <- function(data_mpox, ROLL_WINDOW, TRAIN_WEEK,
                                  jurs_selection, Jurisdiction, n_lags_ar = 25) {
  
  data_raw <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  split <- get_train_test_data(data_raw, TRAIN_WEEK)
  future_ts <- GET_TS_DATA(split$future_data)
  
  if (ROLL_WINDOW != 0) {
    #data_model <- GET_SMOOTH_DATA_WEIGHTED(data_raw, ROLL_WINDOW, weight_type = weight_type)
  } else {
    data_model <- data_raw
  }
  split_smooth <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts_smooth <- GET_TS_DATA(split_smooth$train_data)
  future_ts_smooth <- GET_TS_DATA(split_smooth$future_data)
  
  results_AR <- AR_model_jurs(train_ts_smooth, future_ts_smooth, future_ts, jurs_selection, n_lags_ar, n_step_ahead = 2)
  forecasts_ar = results_AR$forecasts_ar
  
  #forecasts_arima <- ARIMA_model_jurs(train_ts_smooth, future_ts_smooth, future_ts, jurs_selection, n_step_ahead = 2)
  #forecasts_naive <- SIMPLE_NAIVE_FORECAST(train_ts_smooth, future_ts_smooth, future_ts, n_step_ahead = 2)
  #forecasts_naive <- forecasts_naive$df_pred_results
  
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  browser()
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  
  jur_metrics_all <- bind_rows(
    build_jur_metrics(forecasts_ar, "AR-Lasso"),
    #build_jur_metrics(forecasts_arima, "ARIMA"),
    #build_jur_metrics(forecasts_naive, "Naive")
  ) %>% dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = list(AR_Lasso = forecasts_ar),
       df_coeffs = results_AR$df_coeffs) #, ARIMA = forecasts_arima, Naive = forecasts_naive))
  
}


#AR MODEL
Jurisdiction = 'SanDiego'
jurs_selection = c("SanDiego") #, "Illinois")
TRAIN_WEEK = 84
ar_results_pen = run_AR_model(data_year, 0, TRAIN_WEEK, jurs_selection, Jurisdiction)

#COEFFS
coeffs_ar2 = ar_results$df_coeffs

#*******************
#PLOT

title_plot = 'AR model. San diego'
ar_forecasts = ar_results_pen$forecasts$AR_Lasso
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(ar_forecasts, ar_forecasts, ar_forecasts,
                                   ar_forecasts,
                                   ar_forecasts, title_plot)

#*************
#COEFFICIENTS

df_ar_coeff <- coeffs_ar %>%
  mutate(Predictor = ifelse(Predictor %in% c("1", "intercept"), "Intercept", Predictor))

# how often each predictor is active, and its typical size
df_ar_coeff %>%
  group_by(Predictor) %>%
  summarise(weeks_active = n_distinct(Week_Number),
            share_of_weeks = weeks_active / n_distinct(df$Week_Number),
            mean_coef = mean(Coefficient),
            first_week = min(Week_Number), last_week = max(Week_Number)) %>%
  arrange(desc(weeks_active))

# lag 20 over time
df %>% filter(Predictor == "SanDiego_L20") %>% arrange(Week_Number) %>%
  dplyr::select(Week_Number, Coefficient)


#AR HIERARCHICAL MODEL

jurs_selection = c("SanDiego") #, "Illinois")
TRAIN_WEEK = 84
ar_results_sparse = run_AR_model(data_year, 0, TRAIN_WEEK, jurs_selection)
