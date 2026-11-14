#*******************************************
#* VAR-LASSO MODEL FUNCTIONS
#*****************************************

#*****************************************
# VAR-LASSO FORECASTING FUNCTIONS
#
# Purpose
#   Rolling two-week-ahead VAR-Lasso forecasts for San Diego County (SDC),
#   using SDC and the screened predictor jurisdictions as a multivariate
#   system, and the helpers that summarise and compare the results.
#
# Method (Methods Section 4.7.2, rolling forecast design)
#   At each forecast origin, using an expanding window of data up to that week:
#     1. First-difference the weekly counts of all series.
#     2. Fit a VAR-Lasso (BigVAR, basic lasso penalty, 25 lags) at a single
#        fixed penalty lambda. No penalty is chosen by cross-validation
#        (ownlambdas = TRUE with one value).
#     3. Forecast t+1, convert back to the count scale, append it to the
#        training series, refit, and forecast t+2.
#     4. Keep only the t+2 forecast, truncated at zero.
#     5. Advance the origin one week and add the newly observed data.
#   No 2024 data are used to choose any setting.
#
# Functions
#   VAR_LASSO_FORECAST_TWO_STEP() : the rolling two-step forecast loop
#   RUN_VAR()                     : data preparation, forecasting and metrics
#   run_var_sd()                  : wrapper returning the SDC results
#                                   ($overall, $forecasts, $coeffs, $result)
#   pct_improve_vs_var()          : % improvement of the VAR-Lasso over each
#                                   other model (positive = VAR-Lasso better)
#   EXTRACT_NONZERO_LASSO_COEFFS(): non-zero coefficients, with series names
#
# Needs
#   Packages: dplyr, tidyr, BigVAR, reshape2
#   Functions defined elsewhere: get_train_test_data(), GET_TS_DATA(),
#   GET_METRICS(), GET_DF_WEIGHTED_SLOPE(), GET_METRICS_JURISDICTION(),
#   GET_SLOPE_METRICS_JURISDICTION(), PLOT_FORECASTS_SINGLE_JURISDICITON()
#   list_jur must include SanDiego (the response) and the predictors.
#**************************************************************************************

#*********************
#* VAR LASSO

#MODEL
VAR_LASSO_FORECAST_TWO_STEP <- function(train_data_ts, future_data_ts, future_real_ts,
                                      lambda, n_lags = 25) {
  
  'Rolling, expanding-window, two-week-ahead VAR-Lasso forecasts.
  
  At each forecast origin: first-difference all series, fit the VAR-Lasso at
  the fixed penalty lambda, forecast t+1, convert to counts, append it to the
  series, refit, forecast t+2, convert to counts and clip negatives at zero.
  All jurisdictions are forecast; the caller keeps the target (SDC).
  
  Args:
    train_data_ts  : initial training weeks. Columns Week_Number,
                     date_week_start, then one count column per jurisdiction.
    future_data_ts : evaluation weeks, same columns. Observed weeks are added
                     to the training window one at a time.
    future_real_ts : observed counts used as Actual (RUN_VAR passes the same
                     object as future_data_ts).
    lambda         : single fixed lasso penalty.
    n_lags         : maximum lag p (25 weeks).
  
  Returns a list:
    df_pred_results : long data frame (Week_Number, date_week_start,
                      Jurisdiction, Predicted, Actual, bic_value, error).
                      Week_Number is the TARGET week (t+2).
    preds, true     : wide versions of Predicted and Actual.
    df_coeffs       : non-zero coefficients from the FIRST fit at each origin
                      (observed data only). Week_Number is the ORIGIN week.
    coeffs_final    : coefficient matrix from the last step-2 refit.
  
  Note: n_forecasts is the number of evaluation weeks minus 1 (45 in the paper).'
  
  print(paste0('N States: ', (ncol(train_data_ts) - 2)))
  print(paste0('N lags: ', n_lags))
  print(paste0('N step_ahead: ', 2))
  
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  train_data_ts <- train_data_ts %>% dplyr::select(-Week_Number, - date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, - date_week_start)
  future_real_ts = future_real_ts  %>% dplyr::select(-Week_Number, - date_week_start)
  
  juris_names <- colnames(train_data_ts)
  n_forecasts <- nrow(future_data_ts) - 1
  
  results_df <- data.frame()
  df_coeffs <- data.frame()
  train_week_t <- train_data_ts 
  
  for (week_t in 1:n_forecasts) { 
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    
    # 1. Difference training data
    train_diff <- diff(as.matrix(train_week_t), differences = 1)
    
    #2. Fit VAR Lasso model on differenced data
    print(paste0('LAMBDA: ', lambda))
    model_tmp <- constructModel(train_diff, p = n_lags, struct = 'Basic',
                                gran = c(lambda), ownlambdas = TRUE,
                                T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
    
    cv_model_tmp <- cv.BigVAR(model_tmp)
    
    # 3. Save non-zero coefficients
    coefficients <- coef(cv_model_tmp)
    df_coeffs_t <- EXTRACT_NONZERO_LASSO_COEFFS(coefficients, juris_names)
    df_coeffs_t$Week_Number <- rep(week_number_forecast[week_t], nrow(df_coeffs_t))
    df_coeffs <- rbind(df_coeffs, df_coeffs_t)
    
    # 4. Step 1 forecast (t+1)
    last_obs <- as.numeric(tail(train_week_t, 1))
    pred_diff_t1 <- predict(cv_model_tmp, n.ahead = 1)
    level_t1 <- last_obs + as.numeric(pred_diff_t1)
    
    # 5. Step 2 forecast (t+2)
    extended_series <- rbind(train_week_t, level_t1)
    extended_diff <- diff(as.matrix(extended_series), differences = 1)
    
    model_tmp2 <- constructModel(extended_diff, p = n_lags, struct = 'Basic',
                                 gran = c(lambda), ownlambdas = TRUE,
                                 T1 = floor(nrow(extended_diff) * 0.65), verbose = FALSE)
    
    cv_model_tmp_2 <- cv.BigVAR(model_tmp2)
    
    pred_diff_t2 <- predict(cv_model_tmp_2, n.ahead = 1)
    level_t2 <- level_t1 + as.numeric(pred_diff_t2)
    
    # 6. Get actual values
    actual_values <- as.numeric(future_real_ts[week_t + 1, ])
    
    # 7. Save results to long-format dataframe
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), length(juris_names)),
      Jurisdiction = juris_names,
      Predicted = pmax(level_t2, 0),  # clip negatives
      Actual = actual_values,
      bic_value = cv_model_tmp@BICMSFE  
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # 8. Extend training set
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  # Also create wide-format versions (optional)
  df_pred <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Predicted) %>%
    arrange(Week_Number)
  df_true <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Actual) %>%
    arrange(Week_Number)
  
  return(list(
    df_pred_results = results_df,
    preds = df_pred,
    true = df_true,
    df_coeffs = df_coeffs,
    coeffs_final = coef(cv_model_tmp_2)
  ))
}


#RUN VAR
RUN_VAR <- function(jurisdiction_interest, data_mpox, TRAIN_WEEK,
                    list_jur, lambda, n_lags = 25){
  
  'End-to-end driver for the VAR-Lasso forecasts.
  
  Filters the data to list_jur, splits it into training (2023) and evaluation
  (2024) weeks, runs VAR_LASSO_FORECAST_TWO_STEP, computes pooled,
  per-jurisdiction and slope-weighted error metrics, and plots the forecasts
  for jurisdiction_interest.
  
  Args:
    jurisdiction_interest : jurisdiction to plot (e.g. SanDiego).
    data_mpox             : long data frame, one row per jurisdiction-week
                            (needs a Jurisdiction column).
    TRAIN_WEEK            : index passed to get_train_test_data() that
                            separates training weeks from evaluation weeks.
    list_jur              : jurisdictions in the system (SDC + predictors).
    lambda                : single fixed lasso penalty.
    n_lags                : maximum lag p.
  
  Returns a list:
    results_var       : full output of VAR_LASSO_FORECAST_TWO_STEP.
    var_forecasts     : long forecast data frame, all jurisdictions.
    metrics           : pooled metrics across all jurisdictions.
    jur_level_metrics : per-jurisdiction metrics (unweighted and
                        slope-weighted), with Model = VAR-Lasso.
  
  Side effects: prints the pooled metrics and draws a forecast plot.'
  
  #-------------------------------------------------
  # PREPARE DATA
  #-------------------------------------------------
  data_mpox <- data_mpox %>%
    filter(Jurisdiction %in% list_jur)
  
  split <- get_train_test_data(data_mpox, TRAIN_WEEK)
  
  train_ts  <- GET_TS_DATA(split$train_data)
  future_ts <- GET_TS_DATA(split$future_data)
  data_model <- data_mpox
  
  #-------------------------------------------------
  # RUN
  #-------------------------------------------------
  ## VAR-Lasso
  results_var <- VAR_LASSO_FORECAST_TWO_STEP(
    train_ts, future_ts, future_ts, lambda, n_lags = n_lags
  )
  
  var_forecasts = results_var$df_pred_results
  
  #Performance metrics
  metrics_var <- GET_METRICS(var_forecasts)
  print(metrics_var)
  
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, list_jur)
  
  jur_metrics_var <- GET_METRICS_JURISDICTION(var_forecasts, list_jur)
  jur_slope_var   <- GET_SLOPE_METRICS_JURISDICTION(var_forecasts, df_slope, list_jur)
  
  jur_level_metrics <- left_join(jur_metrics_var, jur_slope_var, by = "Jurisdiction") %>%
    mutate(Model = "VAR-Lasso") %>%
    dplyr::select(Jurisdiction, Model, everything())
  #PLOT_CASES_FORECASTS(var_forecasts, var_forecasts, var_forecasts, var_forecasts, var_forecasts, list_jur, title_plot)
  
  #PLOT SAN DIEGO
  title_plot = paste0(jurisdiction_interest, ' VAR-Lasso forecasts, lambda: ', lambda)
  jur_var_forecasts = var_forecasts %>% filter(Jurisdiction == jurisdiction_interest)
  PLOT_FORECASTS_SINGLE_JURISDICITON(jur_var_forecasts, jur_var_forecasts, jur_var_forecasts,
                                     jur_var_forecasts,
                                     jur_var_forecasts, title_plot)
  
  return(list(
    results_var = results_var,             # original model output object ($df_pred_results etc.)
    var_forecasts = var_forecasts,         # unwrapped prediction dataframe
    metrics = metrics_var,                 # pooled metrics (all jurisdictions combined)
    jur_level_metrics = jur_level_metrics  # per-jurisdiction metrics -- NEW
  ))
}

#RUN
run_var_sd <- function(data_mpox, TRAIN_WEEK, list_jur, lambda,
                       jurisdiction_interest = "SanDiego",
                       n_lags = 25) {
  
  'Wrapper used for the paper results: runs RUN_VAR and returns only the
  target jurisdiction (San Diego County by default), in the same shape as
  the comparison-model wrappers so results can be bound together.
  
  Args:
    data_mpox, TRAIN_WEEK, list_jur, lambda, n_lags : as in RUN_VAR.
    jurisdiction_interest : the response jurisdiction (default SanDiego).
  
  Returns a list:
    overall   : one row of target metrics (unweighted and slope-weighted)
                with a Model column.
    forecasts : target rows of the forecast data frame.
    coeffs    : non-zero coefficients saved at each origin (first fit),
                restricted to the target equation when a Response column
                exists.
    result    : full RUN_VAR output.
  
  Stops with an error if the target is not in list_jur, or if list_jur has
  no predictors (a VAR needs at least two series).'
  
  if (!(jurisdiction_interest %in% list_jur)) stop(jurisdiction_interest, " is not in list_jur")
  if (length(list_jur) < 2) stop("list_jur has no predictors -- VAR-Lasso needs at least one.")
  
  result <- RUN_VAR(jurisdiction_interest, data_mpox,
                    TRAIN_WEEK, list_jur, lambda,
                    n_lags = n_lags)
  
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


# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

# % improvement of VAR-Lasso over each other model: (other - VAR) / other * 100.
# Absolute values, so bias is compared by size (as in the original paper).
# Positive = VAR-Lasso better.
pct_improve_vs_var <- function(comparison,
                               metric_cols = c("rmse", "mae", "slope_weighted_rmse",
                                               "slope_weighted_mae", "slope_weighted_bias")) {
  
  'Percentage improvement of the VAR-Lasso over each other model, per metric:
  (other - VAR) / other * 100, using absolute values so bias is compared by
  size. Positive means the VAR-Lasso is better.
  
  Args:
    comparison  : data frame with one row per model, a Model column that
                  includes VAR-Lasso, and one numeric column per metric.
    metric_cols : metric columns to compare (default RMSE, MAE and the
                  slope-weighted RMSE, MAE and bias).
  
  Returns one row per non-VAR model with columns pct_improve_<metric>.'
  
  var_row <- comparison %>% filter(Model == "VAR-Lasso")
  
  comparison %>%
    filter(Model != "VAR-Lasso") %>%
    transmute(Model,
              across(all_of(metric_cols),
                     ~ (abs(.x) - abs(var_row[[cur_column()]])) / abs(.x) * 100,
                     .names = "pct_improve_{.col}"))
}

#EXTRACT COEFFICIENTS
EXTRACT_NONZERO_LASSO_COEFFS <- function(coef_matrix, list_ordered_jur = NULL) {
  
  'Convert the coefficient matrix from coef(<BigVAR fit>) into a long data
  frame of non-zero coefficients, with readable jurisdiction names.
  
  BigVAR labels series Y1, Y2, ... and predictors such as Y3L2 (series 3,
  lag 2). If list_ordered_jur is supplied, Y3L2 becomes <name>_L2 (for
  example Illinois_L24) and Response becomes the jurisdiction name.
  
  Args:
    coef_matrix      : coefficient matrix (rows = responses, columns =
                       intercept and lagged predictors).
    list_ordered_jur : jurisdiction names in the same order as the columns of
                       the data given to BigVAR (NULL leaves Y1, Y2, ...).
  
  Returns a data frame with Response, Predictor, Coefficient, sorted by
  response then by absolute coefficient size. The intercept is kept under its
  original name; filter on _L<number> to keep lag terms only.'
  
  # Convert to data.frame if it's a matrix
  if (is.matrix(coef_matrix)) {
    coef_df <- as.data.frame(coef_matrix)
  } else {
    coef_df <- coef_matrix
  }
  
  # Add response as a column before melting
  coef_df$Response <- rownames(coef_df)
  
  # Reshape to long format
  coef_long <- reshape2::melt(coef_df, id.vars = "Response", 
                              variable.name = "Predictor", value.name = "Coefficient")
  
  # Filter to non-zero coefficients
  nonzero_coefs <- coef_long[coef_long$Coefficient != 0, ]
  
  # Rename Y1, Y2, etc. if list_ordered_jur is provided
  if (!is.null(list_ordered_jur)) {
    # Replace responses
    response_map <- setNames(list_ordered_jur, paste0("Y", seq_along(list_ordered_jur)))
    nonzero_coefs$Response <- response_map[nonzero_coefs$Response]
    
    # Replace predictors: detect Y[1-9]+ at start of string, e.g., Y3L2
    nonzero_coefs$Predictor <- sapply(nonzero_coefs$Predictor, function(pred) {
      if (grepl("^Y\\d+", pred)) {
        match <- regmatches(pred, regexpr("^Y\\d+", pred))
        index <- as.numeric(sub("Y", "", match))
        new_jur <- list_ordered_jur[index]
        sub("^Y\\d+", paste0(new_jur, "_"), pred)
      } else {
        pred
      }
    })
  }
  
  # Sort for readability
  nonzero_coefs <- nonzero_coefs[order(nonzero_coefs$Response, -abs(nonzero_coefs$Coefficient)), ]
  #nonzero_coefs <- nonzero_coefs %>% select(Predictor, Response, Coefficient)
  
  return(nonzero_coefs)
}