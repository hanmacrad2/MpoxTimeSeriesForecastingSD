#*********************************
#* Sparse Auto-regression model
#* *******************************

#*****************************************
# AR-LASSO FORECASTING FUNCTIONS
#
# Purpose
#   Rolling two-week-ahead forecasts from a univariate lasso-penalised
#   autoregression (AR-Lasso) for San Diego County (SDC), used as a comparison
#   model for the VAR-Lasso. It uses only SDC's own lagged counts.
#
# Method (Methods Sections 4.5 and 4.7.2)
#   At each forecast origin, using an expanding window of SDC weekly counts up
#   to that week:
#     1. First-difference the series (no scaling, as for the VAR-Lasso).
#     2. Fit a lasso AR with BigVAR (basic penalty) using up to n_lags own lags
#        (8 in the paper, chosen from the autocorrelation of the 2023 training
#        data) at a single fixed penalty lambda. No penalty is chosen by
#        cross-validation (ownlambdas = TRUE with one value).
#     3. Forecast t+1, convert back to the count scale, append it to the
#        series, refit, and forecast t+2.
#     4. Keep only the t+2 forecast, truncated at zero.
#     5. Advance the origin one week and add the newly observed count.
#   Raw weekly counts are used (no smoothing). No 2024 data are used to choose
#   any setting.
#
# Functions
#   AR_model()         : the rolling two-step loop (BigVAR). USED in the paper.
#   run_AR_model()     : data preparation, forecasting and metrics
#                        (slope-weighted and unweighted). It calls AR_model().
#                        Returns $per_jurisdiction, $overall, $forecasts and
#                        $df_coeffs (non-zero coefficients per origin).
#
# Needs
#   Packages: BigVAR, dplyr (bigtime only for AR_model_bigtime)
#   Functions defined elsewhere: get_train_test_data(), GET_TS_DATA(),
#   EXTRACT_NONZERO_LASSO_COEFFS(), GET_DF_WEIGHTED_SLOPE(),
#   GET_METRICS_JURISDICTION(), GET_SLOPE_METRICS_JURISDICTION()
#*********************************************************************

AR_model <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                          jur_name, n_lags, lambda, n_step_ahead = 2) {
  
  print('****************')
  print('New AR model')
  print(paste0('n lags: ', n_lags))
  
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  future_real_ts_jur1  <- future_real_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)
  train_week_t         <- train_data_ts_jur1  %>% dplyr::select(-Week_Number, -date_week_start)
  
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  forecasts_ar_model  <- data.frame()
  df_coeffs   <- data.frame()
  
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    
    # 1. Difference 
    train_diff <- diff(as.matrix(train_week_t), differences = 1)
    
    ar_lasso_model <- constructModel(train_diff, p = n_lags, struct = "Basic",
                                     gran = c(lambda), ownlambdas = TRUE,
                                     T1 = floor(nrow(train_diff) * 0.65),
                                     verbose = FALSE)
    
    ar_lasso_model_cv <- cv.BigVAR(ar_lasso_model)
    #browser()
    sum(as.numeric(coef(ar_lasso_model_cv)) != 0)
    opt_lambda = ar_lasso_model_cv@OptimalLambda
    print(paste0('optimal lambda: ', opt_lambda))
    # 3. Save non-zero coefficients
    coefficients <- coef(ar_lasso_model_cv)
    df_coeffs_t  <- EXTRACT_NONZERO_LASSO_COEFFS(coefficients, jur_name)
    if (!is.null(df_coeffs_t) && nrow(df_coeffs_t) > 0) {
      df_coeffs_t$Week_Number <- rep(week_number_forecast[week_t], nrow(df_coeffs_t))
      df_coeffs <- rbind(df_coeffs, df_coeffs_t)
    }
    
    # 4. Step 1 forecast (t+1)
    last_obs     <- as.numeric(tail(train_week_t, 1))
    pred_diff_t1 <- predict(ar_lasso_model_cv, n.ahead = 1)
    level_t1     <- last_obs + as.numeric(pred_diff_t1)
    
    # 5. Step 2 forecast (t+2)
    extended_series <- rbind(train_week_t, level_t1)
    extended_diff   <- diff(as.matrix(extended_series), differences = 1)
    
    ar_lasso_model_2 <- constructModel(extended_diff, p = n_lags, struct = "Basic",
                                       gran = c(lambda), ownlambdas = TRUE,
                                       T1 = floor(nrow(extended_diff) * 0.65),
                                       verbose = FALSE)
    
    ar_lasso_model_2_cv <- cv.BigVAR(ar_lasso_model_2)
    
    pred_diff_t2 <- predict(ar_lasso_model_2_cv, n.ahead = 1)
    level_t2     <- level_t1 + as.numeric(pred_diff_t2)
    
    # 6. Actual values
    actual_values <- as.numeric(future_real_ts_jur1[week_t + 1, ])
    
    df_t <- data.frame(
      Week_Number     = rep(week_number_forecast[week_t + 1], 1),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), 1),
      Jurisdiction    = jur_name,
      Predicted       = pmax(level_t2, 0),
      Actual          = actual_values
    )
    forecasts_ar_model <- bind_rows(forecasts_ar_model, df_t)
    
    # Extend the training set forward by 1 week
    train_week_t <- rbind(train_week_t, future_data_ts_jur1[week_t, 3])
  }
  
  forecasts_ar_model <- forecasts_ar_model %>% mutate(error = Actual - Predicted)
  
  if (nrow(df_coeffs) > 0) df_coeffs$Jurisdiction <- jur_name
  
  return(list(forecasts_ar_model = forecasts_ar_model, df_coeffs = df_coeffs))
}

#Run ar model
run_AR_model <- function(data_mpox, TRAIN_WEEK,
                         jurs_selection, n_lags_ar = 8, lambda,
                         n_step_ahead = 2,
                         model_label = "Penalized AR (Lasso)") {
  
  'End-to-end driver for the AR-Lasso comparison model.
  
  Filters the data to the chosen jurisdictions, splits it into training (2023)
  and evaluation (2024) weeks, runs AR_model once per jurisdiction, and
  computes unweighted and slope-weighted error metrics.
  
  Args:
    data_mpox      : long data frame, one row per jurisdiction-week (needs a
                     Jurisdiction column).
    TRAIN_WEEK     : index passed to get_train_test_data() that separates the
                     training weeks from the evaluation weeks.
    jurs_selection : jurisdictions to forecast. The paper uses SanDiego only.
    n_lags_ar      : maximum number of own lags (8).
    lambda         : single fixed lasso penalty. The default is 0.05 but the
                     paper uses 0.1, so pass lambda = 0.1.
    n_step_ahead   : forecast horizon in weeks (2).
    model_label    : text placed in the Model column of the output.
  
  Raw weekly counts are used with no smoothing.
  
  The call used for the paper is
    run_AR_model(data_year, 0, TRAIN_WEEK, SanDiego, n_lags_ar = 8,
                 lambda = 0.1)
  
  Returns a list:
    per_jurisdiction : metrics for each jurisdiction (unweighted and
                       slope-weighted), with a Model column.
    overall          : the per-jurisdiction metrics averaged over
                       jurisdictions. With SanDiego only this equals the
                       SanDiego row.
    forecasts        : t+2 forecasts and observed counts for every origin.
    df_coeffs        : non-zero coefficients from the first fit at each
                       origin (see AR_model).'
  
  data_raw <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  
  # raw weekly counts, no smoothing
  data_model <- data_raw
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  # ---- one AR_model() call per jurisdiction -----------------------------------
  fc_list <- list(); coef_list <- list()
  for (jur in jurs_selection) {
    tr <- train_ts[,  c("date_week_start", "Week_Number", jur)]
    fu <- future_ts[, c("date_week_start", "Week_Number", jur)]
    out <- AR_model(train_data_ts_jur1  = tr,
                    future_data_ts_jur1 = fu,
                    future_real_ts_jur1 = fu,
                    jur_name     = jur,
                    n_lags       = n_lags_ar,
                    lambda       = lambda,
                    n_step_ahead = n_step_ahead)
    fc_list[[jur]]   <- out$forecasts_ar_model
    coef_list[[jur]] <- out$df_coeffs
  }
  forecasts_ar <- bind_rows(fc_list)
  df_coeffs    <- bind_rows(coef_list)
  
  # ---- metrics (unweighted and slope-weighted, per jurisdiction) --------------
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  jur_metrics_all <- bind_rows(
    build_jur_metrics(forecasts_ar, model_label)
  ) %>% dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = forecasts_ar,   # element name kept so existing plotting code still works
       df_coeffs = df_coeffs)
}

