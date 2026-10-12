#DATA PROCESSING

GET_SMOOTH_DATA <- function(data_mpox, ROLL_WINDOW){
  
  print(paste0('Roll window size: ', ROLL_WINDOW))
  df_smooth <- data_mpox %>%
    group_by(Jurisdiction) %>%
    mutate(
      Cases = rollmean(Cases, k = ROLL_WINDOW, fill = NA, align = "right") 
    ) %>%
    ungroup() %>%
    filter(!is.na(Cases))
  
  return(df_smooth)
}

#*******************
#TIME SERIES FORMATTING
GET_TS_DATA <- function(data_mpox) {
  
  'GET DATA IN TIME-SERIES FORMAT; TIME X JURISDICTION'
  var_data <- data_mpox %>%
    dplyr::select(Jurisdiction, date_week_start, Week_Number, Cases) %>%
    pivot_wider(id_cols = c(Week_Number, date_week_start), names_from = Jurisdiction, values_from = Cases) #id_cols = c(date_week_start, Week_Number)
  #%>% dplyr::select(-Week_Number)
  
  print(paste0('nrow data_ts: ', nrow(var_data)))
  print(paste0('ncol data_ts: ', ncol(var_data)))
  
  return(var_data)
}

GET_TS_DATA_DIFF <- function(data_mpox) {
  
  'GET DATA IN TIME-SERIES FORMAT; TIME X JURISDICTION'
  data_diff_long <- data_mpox %>%
    group_by(Jurisdiction) %>%
    mutate(Cases_Diff = c(NA, diff(Cases))) %>%
    ungroup() %>%
    filter(!is.na(Cases_Diff)) # Remove the first row of each jurisdiction
  
  data_diff <- data_diff_long %>%
    pivot_wider(id_cols = c(Week_Number), names_from = Jurisdiction, values_from = Cases_Diff) %>%
    dplyr::select(-Week_Number, - date_week_start)
  
  #data_diff = data_diff[, correct_col_order]
  
  print(paste0('nrow data_ts: ', nrow(data_diff)))
  print(paste0('ncol data_ts: ', ncol(data_diff)))
  
  return(data_diff)
}

#MODEL FITTING - SPLIT DATA

get_train_test_data <- function (data_mpox_model, TRAIN_WEEK){
  
  #TRAIN VS TEST SPLIT
  train_data <- data_mpox_model %>% filter(Week_Number <= TRAIN_WEEK)
  print(paste0('n weeks train_data: ', nrow(train_data)/(length(unique(data_mpox_model$Jurisdiction)))))
  future_data  <- data_mpox_model %>% filter(Week_Number > TRAIN_WEEK)
  print(paste0('nrow weeks future_data: ', nrow(future_data)/(length(unique(data_mpox_model$Jurisdiction)))))
  
  print(paste0('Number unique jurisdictions: ', length(unique(train_data$Jurisdiction))))
  
  list_data = list(train_data = train_data, future_data = future_data)
  
  return(list_data)
}