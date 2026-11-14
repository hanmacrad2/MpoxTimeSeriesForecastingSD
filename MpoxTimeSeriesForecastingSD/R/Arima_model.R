#ARIMA MODEL
library(forecast)

#*****************************************
# ARIMA FORECASTING FUNCTIONS
#
# Purpose
#   Rolling two-week-ahead ARIMA forecasts for San Diego County (SDC), used as
#   a conventional univariate time-series comparison model for the VAR-Lasso.
#
# Method (Methods Sections 4.5.4 and 4.7.2)
#   At each forecast origin, using an expanding window of SDC weekly counts up
#   to that week:
#     1. Fit a non-seasonal ARIMA with auto.arima() (stepwise search), which
#        selects its own order of differencing and its own AR and MA orders.
#        The autoregressive order is capped at 8 (max_p), matching the maximum
#        lag of the AR-Lasso and negative binomial models.
#     2. Forecast t+1, append that forecast to the series, refit, and forecast
#        t+2.
#     3. Keep only the t+2 forecast, truncated at zero.
#     4. Advance the origin one week and add the newly observed count.
#   Raw weekly counts are used (no smoothing). The order is re-selected at
#   every origin, so no ARIMA setting is fixed using the 2024 data.
#
# Functions
#   ARIMA_model()     : the rolling two-step forecast loop for one jurisdiction
#   run_ARIMA_model() : data preparation, forecasting and metrics (slope-weighted
#                       and unweighted), returning $per_jurisdiction, $overall
#                       and $forecasts
#
# Needs
#   Packages: forecast, dplyr
#   Functions defined elsewhere: get_train_test_data(), GET_TS_DATA(),
#   GET_DF_WEIGHTED_SLOPE(), GET_METRICS_JURISDICTION(),
#   GET_SLOPE_METRICS_JURISDICTION()
#*****************************************

#ARIMA MODEL (maximum autoregressive order 8, matching the AR-Lasso)
ARIMA_model <- function(train_data_ts_jur1,
                        future_data_ts_jur1,
                        future_real_ts_jur1,
                        jur_name,
                        n_step_ahead = 2,
                        max_p = 8){
  
  'Rolling, expanding-window, two-week-ahead ARIMA forecasts for one
  jurisdiction.
  
  At each forecast origin: fit a non-seasonal ARIMA with auto.arima to the
  observed counts, forecast t+1, append that forecast to the series, refit
  (the orders p, d, q are re-selected), forecast t+2 and keep it. The training
  window then grows by one observed week.
  
  Args:
    train_data_ts_jur1  : initial training weeks. Columns date_week_start,
                          Week_Number and one count column named jur_name.
    future_data_ts_jur1 : evaluation weeks, same columns. Observed weeks are
                          added to the training window one at a time.
    future_real_ts_jur1 : observed counts used as Actual (run_ARIMA_model
                          passes the same object as future_data_ts_jur1).
    jur_name            : name of the count column, e.g. SanDiego.
    n_step_ahead        : forecast horizon in weeks (2). The number of origins
                          is the number of evaluation weeks minus
                          (n_step_ahead - 1).
    max_p               : maximum autoregressive order (8, matching the
                          AR-Lasso and negative binomial models). The maximum
                          moving-average and differencing orders are left at
                          the auto.arima defaults (5 and 2). The max.order
                          argument has no effect with stepwise = TRUE.
  
  Fits use raw counts with no transformation. auto.arima differences the
  series internally through the selected order d. Warnings and the selected
  orders are not saved.
  
  Returns a data frame (not a list) with one row per origin:
    Week_Number, date_week_start : the TARGET week (t+2).
    Jurisdiction                 : jur_name.
    Predicted                    : t+2 forecast, clipped at zero.
    Actual                       : observed count in the target week.
    error                        : Actual - Predicted.'
  
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

    #Step 1. Fit on observed data
    fit <- auto.arima(
      train_week_t,
      seasonal = FALSE,
      stepwise = TRUE,
      approximation = FALSE,
      max.p = max_p,                 # autoregressive order capped at 8 (as for the AR-Lasso)
      max.order = max_p + 2          # only used if stepwise = FALSE
    )
    
    #Step 2.  Step 1 forecast (t+1)
    pred1 <- as.numeric(forecast(fit, h = 1)$mean)
    
    # 3. Step 2 forecast (t+2): append t+1 prediction, REFIT, then predict
    extended_series <- c(train_week_t, pred1)
    
    fit2 <- auto.arima(
      extended_series,
      seasonal = FALSE,
      stepwise = TRUE,
      approximation = FALSE,
      max.p = max_p,
      max.order = max_p + 2
    )
    
    pred2 <- as.numeric(forecast(fit2, h = 1)$mean)

    #Create dataframe of restusl 
    actual_value <- future_real[week_t + 1]
    df_t <- data.frame(
      Week_Number     = week_number_forecast[week_t + 1],
      date_week_start = list_date_week_start[week_t + 1],
      Jurisdiction    = jur_name,
      Predicted       = max(pred2, 0),
      Actual          = actual_value
    )
    
    results_df <- dplyr::bind_rows(results_df, df_t)
    
    #Expand training set with observed week 
    train_week_t <- c(train_week_t, future_values[week_t])
    
  }
  
  results_df$error <- results_df$Actual - results_df$Predicted
  
  return(results_df)
}

run_ARIMA_model <- function(data_mpox, ROLL_WINDOW = 0, TRAIN_WEEK,
                            jurs_selection, max_p = 8,
                            n_step_ahead = 2,
                            model_label = "ARIMA") {
  
  'End-to-end driver for the ARIMA comparison model.
  
  Filters the data to the chosen jurisdictions, splits it into training (2023)
  and evaluation (2024) weeks, runs ARIMA_model once per jurisdiction, and
  computes unweighted and slope-weighted error metrics.
  
  Args:
    data_mpox      : long data frame, one row per jurisdiction-week (needs a
                     Jurisdiction column).
    ROLL_WINDOW    : unused (kept for compatibility; smoothing was removed).
    TRAIN_WEEK     : index passed to get_train_test_data() that separates the
                     training weeks from the evaluation weeks.
    jurs_selection : jurisdictions to forecast. The paper uses SanDiego only.
    max_p          : maximum autoregressive order (8).
    n_step_ahead   : forecast horizon in weeks (2).
    model_label    : text placed in the Model column of the output.
  
  Returns a list:
    per_jurisdiction : metrics for each jurisdiction (unweighted and
                       slope-weighted), with a Model column.
    overall          : the per-jurisdiction metrics averaged over
                       jurisdictions. With SanDiego only this equals the
                       SanDiego row.
    forecasts        : t+2 forecasts and observed counts for every origin.
  
  No coefficients are returned, because the ARIMA order changes at each
  origin.'
  
  data_raw <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  
  # raw weekly counts, no smoothing (ROLL_WINDOW kept only for compatibility)
  data_model <- data_raw
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  # ---- one ARIMA_model() call per jurisdiction --------------------------------
  fc_list <- list()
  for (jur in jurs_selection) {
    tr <- train_ts[,  c("date_week_start", "Week_Number", jur)]
    fu <- future_ts[, c("date_week_start", "Week_Number", jur)]
    fc_list[[jur]] <- ARIMA_model(train_data_ts_jur1  = tr,
                                  future_data_ts_jur1 = fu,
                                  future_real_ts_jur1 = fu,
                                  jur_name     = jur,
                                  n_step_ahead = n_step_ahead,
                                  max_p        = max_p)
  }
  forecasts_arima <- bind_rows(fc_list)
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  jur_metrics_all <- bind_rows(
    build_jur_metrics(forecasts_arima, model_label)
  ) %>% dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = forecasts_arima)  
}