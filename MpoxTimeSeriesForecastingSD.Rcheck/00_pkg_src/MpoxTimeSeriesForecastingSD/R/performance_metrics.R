#PERFORMANCE METRICS

GET_DF_WEIGHTED_SLOPE <- function(df_mpox_smooth, list_jur){
  
  df_slope = data.frame()
  
  for (jur in list_jur){
    
    df_smooth_jur = df_mpox_smooth %>% filter(Jurisdiction == jur) %>%
      arrange(date_week_start)
    
    #Slope
    delta_raw <- diff(df_smooth_jur$Cases)
    delta_yt = pmax(delta_raw, 0) #NB: pmax
    
    #Dated dataframe
    date_week_start <- df_smooth_jur$date_week_start[-1]
    
    df_slope_jur <- data.frame(
      Jurisdiction = jur,
      date_week_start = as.Date(date_week_start),
      delta_yt = delta_yt
    )
    
    #Combine
    df_slope = rbind(df_slope, df_slope_jur)
  }
  
  
  return(df_slope)
}

GET_SLOPE_WEIGHTED_METRICS <- function(df_slope, df_preds) {
  
  df_preds_slope <- df_preds %>%
    left_join(df_slope, by = c('Jurisdiction', 'date_week_start')) %>%
    dplyr::select(Jurisdiction, date_week_start, Actual, Predicted, delta_yt)
  
  weights <- df_preds_slope$delta_yt
  
  # RMSE
  slope_weighted_rmse <- sqrt(sum(weights * (df_preds_slope$Actual - df_preds_slope$Predicted)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  slope_weighted_rmse <- round(slope_weighted_rmse, 10)
  print(paste0('Slope weighted RMSE: ', round(slope_weighted_rmse, 3)))
  
  # MAE
  slope_weighted_mae <- sum(weights * abs(df_preds_slope$Actual - df_preds_slope$Predicted), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_mae <- round(slope_weighted_mae, 10)
  print(paste0('Slope weighted MAE: ', round(slope_weighted_mae, 3)))
  
  # BIAS
  slope_weighted_bias <- sum(weights * (df_preds_slope$Predicted - df_preds_slope$Actual), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_bias <- round(slope_weighted_bias, 10)
  print(paste0('Slope weighted BIAS: ', slope_weighted_bias))
  print(paste0('Slope weighted BIAS: ', round(slope_weighted_bias, 3)))
  
  # MAPE
  df_preds_slope_mape <- df_preds_slope %>% filter(Actual > 0)
  
  weights_mape <- df_preds_slope_mape$delta_yt
  
  slope_weighted_mape <- sum(weights_mape * abs((df_preds_slope_mape$Predicted - df_preds_slope_mape$Actual) / df_preds_slope_mape$Actual), na.rm = TRUE) / sum(weights_mape, na.rm = TRUE) * 100
  print(paste0('Slope weighted MAPE (%): ', slope_weighted_mape))
  
  return(list(
    slope_weighted_rmse = slope_weighted_rmse,
    slope_weighted_mae = slope_weighted_mae,
    slope_weighted_bias = slope_weighted_bias,
    slope_weighted_mape = slope_weighted_mape
  ))
}

#GET_METRICS
GET_METRICS <- function(df_preds) {
  
  df_metrics <- df_preds %>%
    dplyr::select(Jurisdiction, date_week_start, Actual, Predicted)
  
  # RMSE
  rmse <- sqrt(mean((df_metrics$Actual - df_metrics$Predicted)^2, na.rm = TRUE))
  rmse <- round(rmse, 10)
  print(paste0('RMSE: ', round(rmse, 3)))
  
  # MAE
  mae <- mean(abs(df_metrics$Actual - df_metrics$Predicted), na.rm = TRUE)
  mae <- round(mae, 10)
  print(paste0('MAE: ', round(mae, 3)))
  
  # Bias
  bias <- mean(df_metrics$Predicted - df_metrics$Actual, na.rm = TRUE)
  bias <- round(bias, 10)
  print(paste0('BIAS: ', round(bias, 3)))
  
  # MAPE
  df_mape <- df_metrics %>%
    filter(Actual > 0)
  
  mape <- mean(abs((df_mape$Predicted - df_mape$Actual) /
                     df_mape$Actual), na.rm = TRUE) * 100
  
  print(paste0('MAPE (%): ', round(mape, 3)))
  
  return(list(
    rmse = rmse,
    mae = mae,
    bias = bias,
    mape = mape
  ))
}

GET_SLOPE_WEIGHTED_METRICS_I <- function(df_slope, df_preds) {
  
  df_preds_slope <- df_preds %>%
    left_join(df_slope, by = c('Jurisdiction', 'date_week_start')) %>%
    dplyr::select(Jurisdiction, date_week_start, Actual, Predicted, delta_yt)
  
  weights <- df_preds_slope$delta_yt
  
  #RMSE
  slope_weighted_rmse <- sqrt(sum(weights * (df_preds_slope$Actual - df_preds_slope$Predicted)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  slope_weighted_rmse = round(slope_weighted_rmse, 10) #3
  print(paste0('Slope weighted RMSE:', slope_weighted_rmse))
  
  #MAE
  slope_weighted_mae =  sum(weights * abs(df_preds_slope$Actual - df_preds_slope$Predicted), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_mae = round(slope_weighted_mae, 10) #3
  print(paste0('Slope weighted MAE:', slope_weighted_mae))
  
  #BIAS
  slope_weighted_bias = sum(weights * (df_preds_slope$Predicted - df_preds_slope$Actual), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_bias = round(slope_weighted_bias, 10) #3
  print(paste0('Slope weighted BIAS:', slope_weighted_bias))
  
  
  return(list(slope_weighted_rmse = slope_weighted_rmse, slope_weighted_mae = slope_weighted_mae, slope_weighted_bias = slope_weighted_bias))
}

GET_SLOPE_WEIGHTED_IMPROVEMENT <- function(df_slope, df_preds1, df_preds2) {
  
  # Get slope-weighted metrics for each prediction set
  list_metrics1 <- GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds1)
  list_metrics2 <- GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds2)
  
  # Extract values
  var_w_slope_rmse <- list_metrics1$slope_weighted_rmse
  naive_w_slope_rmse <- list_metrics2$slope_weighted_rmse
  var_w_slope_mae <- list_metrics1$slope_weighted_mae
  naive_w_slope_mae <- list_metrics2$slope_weighted_mae
  var_w_slope_bias <- list_metrics1$slope_weighted_bias
  naive_w_slope_bias <- list_metrics2$slope_weighted_bias
  #
  var_w_slope_mape <- list_metrics1$slope_weighted_mape
  naive_w_slope_mape <- list_metrics2$slope_weighted_mape
  
  # Calculate percentage improvements
  var_rmse_pcent_improve <- 100 * (naive_w_slope_rmse - var_w_slope_rmse) / naive_w_slope_rmse
  print(paste0('var_rmse_pcent_improve %; ', round(var_rmse_pcent_improve,2)))
  var_mae_pcent_improve <- 100 * (naive_w_slope_mae - var_w_slope_mae) / naive_w_slope_mae
  print(paste0('var_mae_pcent_improve %; ', round(var_mae_pcent_improve,2)))
  var_bias_pcent_improve <- 100 * (naive_w_slope_bias - var_w_slope_bias) / naive_w_slope_bias
  print(paste0('var_bias_pcent_improve %; ', round(var_bias_pcent_improve,2)))
  #MAPE IMPROVE
  var_mape_pcent_improve <- 100*(naive_w_slope_mape - var_w_slope_mape) / naive_w_slope_mape
  print(paste0('var_mape_pcent_improve %; ', round(var_mape_pcent_improve,2)))
  
  # Return as one-row dataframe
  var_w_slope_rmse = round(var_w_slope_rmse, 3)
  naive_w_slope_rmse = round(naive_w_slope_rmse, 3)
  var_rmse_pcent_improve = round(var_rmse_pcent_improve, 1)
  var_w_slope_mae = round(var_w_slope_mae, 3)
  naive_w_slope_mae = round(naive_w_slope_mae, 3)
  var_mae_pcent_improve = round(var_mae_pcent_improve, 1)
  var_w_slope_bias = round(var_w_slope_bias, 3)
  naive_w_slope_bias = round(naive_w_slope_bias, 3)
  var_bias_pcent_improve = round(var_bias_pcent_improve, 1)
  
  df_out <- data.frame(
    Evaluation_Metric = c('Slope weighted RMSE', 'Slope weighted MAE', 'Slope weighted Bias', 'Slope weighted MAPE'),
    VAR_result = c(var_w_slope_rmse, var_w_slope_mae, var_w_slope_bias, var_w_slope_mape),
    Model_2_result = c(naive_w_slope_rmse, naive_w_slope_mae, naive_w_slope_bias, naive_w_slope_mape),
    Percent_improve_var = c(var_rmse_pcent_improve, var_mae_pcent_improve, var_bias_pcent_improve, var_mape_pcent_improve)
  )
  
  return(df_out)
}

GET_SLOPE_METRICS_JURISDICTION <- function(df_preds, df_slope, list_order) {
  
  df_metrics <- data.frame()
  
  for (jur in list_order) {
    
    # Filter predictions and slope for the jurisdiction
    df_preds_jur <- df_preds %>% filter(Jurisdiction == jur)
    df_slope_jur <- df_slope %>% filter(Jurisdiction == jur)
    
    # Get slope-weighted metrics as a named list
    slope_metrics <- GET_SLOPE_WEIGHTED_METRICS(df_slope_jur, df_preds_jur)
    
    # Add row to summary dataframe
    df_metrics <- bind_rows(df_metrics, tibble(
      Jurisdiction = jur,
      slope_weighted_rmse = slope_metrics$slope_weighted_rmse,
      slope_weighted_mae  = slope_metrics$slope_weighted_mae,
      slope_weighted_bias = slope_metrics$slope_weighted_bias,
      slope_weighted_mape = slope_metrics$slope_weighted_mape
    ))
  }
  
  
  return(df_metrics)
}

GET_JUR_METRICS_MODELS_COMPARED <- function(df1, df2, df_slope, list_order) {
  
  res1 <- GET_SLOPE_METRICS_JURISDICTION(df1, df_slope, list_order)
  res2 <- GET_SLOPE_METRICS_JURISDICTION(df2, df_slope, list_order)
  
  df_combined <- res1 %>%
    rename(
      var_w_slope_rmse = slope_weighted_rmse,
      var_w_slope_mae  = slope_weighted_mae,
      var_w_slope_bias = slope_weighted_bias,
      var_w_slope_mape = slope_weighted_mape
    ) %>%
    left_join(
      res2 %>% rename(
        naive_w_slope_rmse = slope_weighted_rmse,
        naive_w_slope_mae  = slope_weighted_mae,
        naive_w_slope_bias = slope_weighted_bias,
        naive_w_slope_mape = slope_weighted_mape
      ),
      by = "Jurisdiction"
    ) %>%
    mutate(
      var_rmse_pcent_improve = round(100 * (naive_w_slope_rmse - var_w_slope_rmse) / naive_w_slope_rmse, 10),
      var_mae_pcent_improve  = round(100 * (naive_w_slope_mae  - var_w_slope_mae)  / naive_w_slope_mae, 10),
      var_bias_pcent_improve = round(100 * (naive_w_slope_bias - var_w_slope_bias) / naive_w_slope_bias, 10),
      var_mape_pcent_improve = round(100 * (naive_w_slope_mape - var_w_slope_mape) / naive_w_slope_mape, 10)
    ) %>%
    dplyr::select(
      Jurisdiction,
      var_w_slope_rmse, naive_w_slope_rmse, var_rmse_pcent_improve,
      var_w_slope_mae,  naive_w_slope_mae,  var_mae_pcent_improve,
      var_w_slope_bias, naive_w_slope_bias, var_bias_pcent_improve,
      var_w_slope_mape, naive_w_slope_mape, var_mape_pcent_improve
    ) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_order)) %>%
    arrange(Jurisdiction)
  
  # Create the average row
  avg_row <- df_combined %>%
    dplyr::select(-Jurisdiction) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(Jurisdiction = "Average") %>%
    dplyr::select(Jurisdiction, everything())
  
  # Bind to bottom of full table
  df_final <- bind_rows(df_combined, avg_row)
  
  return(df_final)
}


#ADDITIONAL FUNCTION
MATRIX_LASSO_RENAME_COLS <- function(mat, juris_names) {
  # Rename rows: Y1, Y2, ..., Yn → juris_names
  row_mapping <- setNames(juris_names, paste0("Y", seq_along(juris_names)))
  rownames(mat) <- row_mapping[rownames(mat)]
  
  # Rename columns: e.g., Y3L1 → Colorado_L1
  colnames(mat) <- sapply(colnames(mat), function(colname) {
    if (grepl("^Y\\d+", colname)) {
      match <- regmatches(colname, regexpr("^Y\\d+", colname))  # e.g., Y3
      index <- as.numeric(sub("Y", "", match))                  # e.g., 3
      new_name <- juris_names[index]                            # e.g., Colorado
      sub("^Y\\d+", paste0(new_name, "_"), colname)             # replace Y3 with Colorado_
    } else {
      colname  # leave "intercept" or any non-lag columns as-is
    }
  })
  
  return(mat)
}

#GET_JUR_METRICS_MODELS_COMPARED

GET_METRICS_JURISDICTION <- function(df_preds, list_order) {
  
  df_metrics <- data.frame()
  
  for (jur in list_order) {
    
    # Filter predictions and slope for the jurisdiction
    df_preds_jur <- df_preds %>% filter(Jurisdiction == jur)
    
    # Get slope-weighted metrics as a named list
    jur_metrics <- GET_METRICS(df_preds_jur)
    
    # Add row to summary dataframe
    df_metrics <- bind_rows(df_metrics, tibble(
      Jurisdiction = jur,
      rmse = jur_metrics$rmse,
      mae  = jur_metrics$mae,
      bias = jur_metrics$bias,
    mape = jur_metrics$mape
    ))
  }
  
  
  return(df_metrics)
}

GET_JUR_BASE_METRICS_MODELS_COMPARED <- function(df1, df2, list_order) {
  
  res1 <- GET_METRICS_JURISDICTION(df1, list_order)
  res2 <- GET_METRICS_JURISDICTION(df2, list_order)
  
  df_combined <- res1 %>%
    rename(
      var_rmse = rmse,
      var_mae  = mae,
      var_bias = bias,
      var_mape = mape
    ) %>%
    left_join(
      res2 %>% rename(
        naive_rmse = rmse,
        naive_mae  = mae,
        naive_bias = bias,
        naive_mape = mape
      ),
      by = "Jurisdiction"
    ) %>%
    mutate(
      var_rmse_pcent_improve = round(100 * (naive_rmse - var_rmse) / naive_rmse, 10),
      var_mae_pcent_improve  = round(100 * (naive_mae  - var_mae)  / naive_mae, 10),
      var_bias_pcent_improve = round(100 * (naive_bias - var_bias) / naive_bias, 10),
      var_mape_pcent_improve = round(100 * (naive_mape - var_mape) / naive_mape, 10)
    ) %>%
    dplyr::select(
      Jurisdiction,
      var_rmse, naive_rmse, var_rmse_pcent_improve,
      var_mae,  naive_mae,  var_mae_pcent_improve,
      var_bias, naive_bias, var_bias_pcent_improve,
      var_mape, naive_mape, var_mape_pcent_improve
    ) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_order)) %>%
    arrange(Jurisdiction)
  
  # Create the average row
  avg_row <- df_combined %>%
    dplyr::select(-Jurisdiction) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(Jurisdiction = "Average") %>%
    dplyr::select(Jurisdiction, everything())
  
  # Bind to bottom of full table
  df_final <- bind_rows(df_combined, avg_row)
  
  return(df_final)
}