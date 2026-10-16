#*********************************
#* Sparse Auto-regression model
#* *******************************
#*********************************
#* Sparse Auto-regression model
#* *******************************

AR_model <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                          jur_name, n_lags, lambda = 0.05, n_step_ahead = 2) {
  
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
    
    # 1. Difference (no scaling, same as VAR lasso)
    train_diff <- diff(as.matrix(train_week_t), differences = 1)
    
    # 2. Fit AR lasso with BigVAR
    #lambda_grid <- sort(c(0.01, 0.05, 0.1, 0.25, 0.5, 1, 2), decreasing = TRUE)
    
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


AR_model_bigtime <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                             jur_name, n_lags, lambda = 0.05, n_step_ahead = 2) {
  
  print(paste0('n lags: ', n_lags))
  'Univariate penalized AR (bigtime::sparseVAR), differenced and standardized counts, recursive two-step-ahead forecast.'
  # Remove non-jurisdictional columns
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  future_real_ts_jur1 <- future_real_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)
  
  # Remove Week_Number and date_week_start from train
  train_week_t <- train_data_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)
  
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  
  forecasts_ar_model  <- data.frame()
  df_coeffs   <- data.frame()
  
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    train_diff <- diff(as.matrix(train_week_t), differences = 1)
    train_diff_scaled = scale(train_diff)
    
    mu <- attr(train_diff_scaled, "scaled:center")
    sd_ <- attr(train_diff_scaled, "scaled:scale")
    train_diff_scaled = as.matrix(train_diff_scaled)
    
    #****************
    #MODEL
    ar_lasso_model <- sparseVAR(Y= train_diff_scaled, p = n_lags, #VARpen = "L1",
                                VARlseq = c(lambda), selection = "none")
    
    # 3. Save non-zero coefficients (same format as the other AR models:
    #    one row per non-zero lag per forecast origin; standardized scale)
    if (week_t == 1) print(names(ar_lasso_model))   # check once that `Phihat` exists, then delete
    phi <- as.numeric(ar_lasso_model$Phihat)
    idx <- which(phi != 0)
    if (length(idx) > 0) {
      df_coeffs <- rbind(df_coeffs, data.frame(
        Response    = jur_name,
        Predictor   = paste0(jur_name, "_L", idx),
        Coefficient = phi[idx],
        Week_Number = week_number_forecast[week_t]))
    }
    
    # 4. Step 1 forecast (t+1)
    last_obs <- as.numeric(tail(train_week_t, 1))
    pred_diff_t1_scaled <- directforecast(ar_lasso_model, h = 1)
    pred_diff_t1_unscaled = as.numeric(pred_diff_t1_scaled)*sd_ + mu
    
    level_t1 <- last_obs + as.numeric(pred_diff_t1_unscaled)
    
    # 5. Step 2 forecast (t+2)
    extended_series <- rbind(train_week_t, level_t1)
    
    extended_diff <- diff(as.matrix(extended_series), differences = 1)
    extended_diff_scaled = scale(extended_diff)
    mu <- attr(extended_diff_scaled, "scaled:center")
    sd_ <- attr(extended_diff_scaled, "scaled:scale")
    
    ar_lasso_model_2 <- sparseVAR(Y = extended_diff_scaled, p = n_lags,
                                  VARlseq = c(lambda), selection = "none")
    
    pred_diff_t2_scaled <- directforecast(ar_lasso_model_2, h = 1)
    pred_diff_t2_unscaled = as.numeric(pred_diff_t2_scaled)*sd_ + mu
    
    level_t2 <- level_t1 + as.numeric(pred_diff_t2_unscaled)
    
    #STEP AHEAD: Actual values at t + n
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


# =============================================================================
# run_AR_model(): calls AR_model_bigtime() directly, one call per jurisdiction.
# AR_model_jurs() is no longer needed: delete it.
# Needs AR_model_bigtime() from your AR_model file, plus your existing
# get_train_test_data(), GET_TS_DATA(), GET_DF_WEIGHTED_SLOPE(),
# GET_METRICS_JURISDICTION(), GET_SLOPE_METRICS_JURISDICTION().
# Untested (no R here).
# =============================================================================

run_AR_model <- function(data_mpox, ROLL_WINDOW = 0, TRAIN_WEEK,
                         jurs_selection, n_lags_ar = 8, lambda = 0.05,
                         n_step_ahead = 2,
                         model_label = "Penalized AR (Lasso)") {
  
  data_raw <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  
  # raw weekly counts, no smoothing (ROLL_WINDOW kept only for compatibility)
  data_model <- data_raw
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  # ---- one AR_model_bigtime() call per jurisdiction ---------------------------
  fc_list <- list(); coef_list <- list()
  for (jur in jurs_selection) {
    tr <- train_ts[,  c("date_week_start", "Week_Number", jur)]
    fu <- future_ts[, c("date_week_start", "Week_Number", jur)]
    # NAMED arguments: lambda sits before n_step_ahead in the signature, so a
    # positional call would silently pass n_step_ahead (2) as the penalty
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
  
  # ---- metrics (same pattern as your other wrappers) --------------------------
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  jur_metrics_all <- bind_rows(
    build_jur_metrics(forecasts_ar, model_label)
    #, build_jur_metrics(forecasts_arima, "ARIMA")
    #, build_jur_metrics(forecasts_naive, "Naive")
  ) %>% dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = list(AR_Lasso = forecasts_ar),   # element name kept so existing plotting code still works
       df_coeffs = df_coeffs)
}

