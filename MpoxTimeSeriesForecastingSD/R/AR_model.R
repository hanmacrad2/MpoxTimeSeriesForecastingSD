#*********************************
#* Sparse Auto-regression model
#* *******************************

AR_model_jurs <- function(train_data_ts, future_data_ts, future_real_ts, list_jur,
                          n_lags, n_step_ahead = 2) {
  
  'Simple Naive estimate. Calculates 4 week average of weekly count data and projects n_step_ahead in weeks (2 weeks)'
  print(paste0('n lags: ', n_lags))
  forecasts_ar_temp <- list()
  coeffs_list  <- list()
  
  for (jur_name in list_jur) {
    
    print(jur_name)
    train_data_ts_jur1 = train_data_ts[, c('date_week_start', 'Week_Number', jur_name)]
    future_data_ts_jur1 = future_data_ts[, c('date_week_start', 'Week_Number', jur_name)]
    future_real_ts_jur1 = future_real_ts[, c('date_week_start', 'Week_Number', jur_name)] # %>% filter(Jurisdiction == jur_name)
    #out = AR_model(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
    #                         jur_name, n_lags)
    #df_results_ar = rbind(df_results_ar, df_result_jur)
    out <- AR_model(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                    jur_name, n_lags, n_step_ahead)
    forecasts_ar_temp[[jur_name]] <- out$forecasts_ar_model
    coeffs_list[[jur_name]]  <- out$df_coeffs
    
  }
  
  results_list = list(forecasts_ar = dplyr::bind_rows(forecasts_ar_temp),
       df_coeffs = dplyr::bind_rows(coeffs_list))
  
  return(results_list)
}


AR_model <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                     jur_name, n_lags, n_step_ahead = 2) {
  
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
    
    # ar_lasso_model <- constructModel(train_diff, p = n_lags, struct = "Basic",
    #                                  gran = c(0.05), ownlambdas = TRUE,
    #                                  T1 = floor(nrow(train_diff) * 0.65),
    #                                  verbose = FALSE)
    
    lambda_grid <- c(75, 50, 25, 10)
    ar_lasso_model <- constructModel(train_diff, p = n_lags, struct = "Basic",
                                     gran = lambda_grid, #gran = c(50, 10), 
                                     ownlambdas = TRUE,
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
    
    # ar_lasso_model_2 <- constructModel(extended_diff, p = n_lags, struct = "Basic",
    #                                    gran = c(0.05), ownlambdas = TRUE,
    #                                    T1 = floor(nrow(extended_diff) * 0.65),
    #                                    verbose = FALSE)
    
    ar_lasso_model_2 <- constructModel(extended_diff, p = n_lags, struct = "Basic",
                                       gran = lambda_grid, #gran = c(50, 10), 
                                     ownlambdas = TRUE,
                                     T1 = floor(nrow(extended_diff) * 0.65), verbose = FALSE)
    
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
                     jur_name, n_lags, n_step_ahead = 2) {
  
  print(paste0('n lags: ', n_lags))
  'Simple Naive estimate. Calculates 4 week average of weekly count data and projects n_step_ahead in weeks (2 weeks)'
  # Remove non-jurisdictional columns
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  future_real_ts_jur1 <- future_real_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)
  
  # Remove Week_Number and date_week_start from train
  train_week_t <- train_data_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)
  
  #jur_names <- colnames(train_data_ts_jur1)
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  
  results_df <- data.frame()
  
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    train_diff <- diff(as.matrix(train_week_t), differences = 1)
    train_diff_scaled = scale(train_diff)
    
    #train_diff_scaled = matrix(as.numeric(train_diff_scaled), nrow = nrow(train_diff_scaled))
    mu <- attr(train_diff_scaled, "scaled:center")
    sd_ <- attr(train_diff_scaled, "scaled:scale")
    train_diff_scaled = as.matrix(train_diff_scaled)
    
    #****************
    #MODEL
    mod <- constructModel(train_diff_scaled, p = 25, struct = "Basic",
                          gran = c(0.05), ownlambdas = TRUE,   # same style as your VAR call
                          T1 = floor(nrow(train_diff_scaled) * 0.65),         # 33, must be > p
                          verbose = FALSE)
    res <- cv.BigVAR(mod)
    res
    
    ar_lasso_model <- sparseVAR(Y= train_diff_scaled, p = n_lags, VARpen = "L1", 
                                VARlseq = c(0.01, 0.05), selection = "cv")
    #ar_lasso_model <- sparseVAR(Y= train_diff_scaled, p = n_lags, selection = "cv")
    
    #Try
    # ar_lasso_model <- sparseVAR(
    #   Y = train_diff_scaled,
    #   p = n_lags,
    #   VARlseq = c(0.001, 0.005, 0.01, 0.05),
    #   selection = "cv"
    # )
    # 
    #Yin <- matrix(as.numeric(train_diff_scaled), ncol = ncol(train_diff_scaled))
    #sparseVAR(Y = Yin, p = n_lags, VARpen = "L1", VARlseq = c(0.001), selection = "none")
    # ar_lasso_model <- sparseVAR(Y = train_diff_scaled, p = n_lags, VARpen = "L1",
    #                             VARlseq = c(0.001), selection = "none")

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
    
    #OLD
    #ar_lasso_model_2 = sparseVAR(Y= extended_diff_scaled, p = n_lags, selection = "cv") # VARpen = "L1",  
    ar_lasso_model_2 <- sparseVAR(Y = extended_diff_scaled, p = n_lags,
                                VARpen = "L1", VARlseq = 0.001, selection = "none")
    
    pred_diff_t2_scaled <- directforecast(ar_lasso_model_2, h = 1)
    pred_diff_t2_unscaled = as.numeric(pred_diff_t2_scaled)*sd_ + mu
    
    level_t2 <- level_t1 + as.numeric(pred_diff_t2_unscaled)
    
    # Get last observed value at time t
    #last_obs <- as.numeric(tail(train_data_ts_jur1, 1))
    
    #STEP AHEAD: Actual values at t + n
    actual_values <- as.numeric(future_real_ts_jur1[week_t + 1, ])
    
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + 1], 1),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), 1),
      #Week_Number = rep(week_number_forecast[week_t + (n_step_ahead - 1)], 1),
      #date_week_start = rep(date_week_start_list[week_t + (n_step_ahead - 1)], 1),
      Jurisdiction = jur_name,
      Predicted = pmax(level_t2, 0),
      Actual = actual_values
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # Extend the training set forward by 1 week
    #train_data_ts_jur1 <- rbind(train_data_ts_jur1, future_data_ts_jur1[week_t, 1])
    #browser()
    train_week_t <- rbind(train_week_t, future_data_ts_jur1[week_t, 3])
  }
  
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  
  # Reshape
  # df_pred <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Predicted) %>%
  #   arrange(Week_Number)
  # df_true <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Actual) %>%
  #   arrange(Week_Number)
  
  return(results_df)
  
}


AR_model_CI <- function(train_data_ts, 
                        future_data_ts, 
                        future_real_ts,
                        list_jur,
                        n_step_ahead = 2,
                        alpha = 0.05) {
  
  # ---- DATA PREPARATION ----
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  
  # Remove non-jurisdictional columns
  train_week_t <- train_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_clean <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_clean <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  # Number of forecasts
  n_forecasts <- nrow(future_data_ts) - (n_step_ahead - 1)
  
  # Results storage
  results_list <- vector("list", n_forecasts)
  
  # ---- LOOP OVER WEEKS ----
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    
    # Force matrix
    Y <- as.matrix(train_week_t)
    
    # Ensure numeric
    if (!is.numeric(Y)) {
      Y <- apply(Y, 2, as.numeric)
    }
    
    if (is.null(colnames(Y))) {
      colnames(Y) <- list_jur
    }
    
    J <- ncol(Y)
    out_week <- vector("list", J)
    
    # ---- LOOP OVER JURISDICTIONS ----
    for (i in seq_len(J)) {
      
      jur <- colnames(Y)[i]
      
      # Extract column
      y <- Y[, i, drop = TRUE]
      
      # Check for NAs
      if (any(is.na(y))) {
        warning(paste("Week", week_t, "- Jurisdiction", jur, "contains NAs. Removing them."))
        y <- y[!is.na(y)]
      }
      
      # Check sufficient data
      if (length(y) < 3) {
        warning(paste("Week", week_t, "- Jurisdiction", jur, "has insufficient data. Skipping."))
        next
      }
      
      # ---- STEP 1: Forecast t+1 WITH CI ----
      last_obs <- tail(y, 1)
      dy <- diff(y)
      
      if (length(dy) < 2) {
        warning(paste("Week", week_t, "- Jurisdiction", jur, "has insufficient differenced data. Skipping."))
        next
      }
      
      # Fit AR(1) on differences
      fit1 <- tryCatch({
        arima(dy, order = c(1, 0, 0))
      }, error = function(e) {
        warning(paste("Week", week_t, "- ARIMA failed for", jur, ":", e$message))
        return(NULL)
      })
      
      if (is.null(fit1)) next
      
      # Forecast difference for t+1 with uncertainty
      fc1 <- predict(fit1, n.ahead = 1)
      z <- qnorm(1 - alpha / 2)
      
      diff_t1_mean <- fc1$pred[1]
      diff_t1_sd <- fc1$se[1]
      
      diff_t1_lower <- diff_t1_mean - z * diff_t1_sd
      diff_t1_upper <- diff_t1_mean + z * diff_t1_sd
      
      # Convert to levels
      level_t1_mean <- last_obs + diff_t1_mean
      level_t1_lower <- last_obs + diff_t1_lower
      level_t1_upper <- last_obs + diff_t1_upper
      
      # ---- STEP 2: Forecast t+2 WITH PROPAGATED UNCERTAINTY ----
      # Extend series with t+1 prediction (use mean)
      y_extended <- c(y, level_t1_mean)
      dy_extended <- diff(y_extended)
      
      # Fit AR(1) on extended differences
      fit2 <- tryCatch({
        arima(dy_extended, order = c(1, 0, 0))
      }, error = function(e) {
        warning(paste("Week", week_t, "- ARIMA step 2 failed for", jur, ":", e$message))
        return(NULL)
      })
      
      if (is.null(fit2)) next
      
      # Forecast difference for t+2
      # PROPER APPROACH: Use direct h=2 forecast from original model
      # This accounts for AR correlation structure
      fc_direct <- predict(fit1, n.ahead = 2)
      
      # The 2-step ahead forecast variance from ARIMA already accounts
      # for the correlation between forecast errors at different horizons
      diff_t1_mean <- fc_direct$pred[1]
      diff_t2_cumulative_mean <- sum(fc_direct$pred[1:2])
      diff_t2_cumulative_sd <- fc_direct$se[2]  # This is the 2-step SE
      
      # Alternative manual calculation (for AR(1) specifically):
      # If φ is the AR coefficient, then:
      # Var(ε_t+1 + φ*ε_t+2) = σ^2(1 + φ^2) for AR(1)
      # But predict() already does this correctly
      
      diff_t2_lower <- diff_t2_cumulative_mean - z * diff_t2_cumulative_sd
      diff_t2_upper <- diff_t2_cumulative_mean + z * diff_t2_cumulative_sd
      
      # Back to levels
      level_mean <- last_obs + diff_t2_cumulative_mean
      level_lower <- last_obs + diff_t2_lower
      level_upper <- last_obs + diff_t2_upper
      
      # Get actual value
      actual_value <- as.numeric(future_real_clean[week_t + 1, i])
      
      # Store results
      out_week[[i]] <- data.frame(
        Week_Number = week_number_forecast[week_t + 1],
        date_week_start = list_date_week_start[week_t + 1],
        Jurisdiction = jur,
        Predicted = pmax(as.numeric(level_mean), 0),
        Lower_CI = pmax(as.numeric(level_lower), 0),
        Upper_CI = pmax(as.numeric(level_upper), 0),
        Actual = actual_value,
        stringsAsFactors = FALSE
      )
    }
    
    # Combine jurisdictions for this week
    out_week <- out_week[!sapply(out_week, is.null)]
    
    if (length(out_week) > 0) {
      results_list[[week_t]] <- do.call(rbind, out_week)
    }
    
    # ---- EXTEND TRAINING SET ----
    train_week_t <- rbind(train_week_t, future_data_clean[week_t, ])
  }
  
  # ---- COMBINE ALL RESULTS ----
  results_list <- results_list[!sapply(results_list, is.null)]
  
  if (length(results_list) == 0) {
    stop("No valid forecasts produced. Check your data for NAs or non-numeric values.")
  }
  
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL
  
  # Add error column
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  
  return(results_df)
}
