
#*******************************************************************
#* RUN VAR
#*******************
FOLDER_SAVE = "C:/Users/h2cra/OneDrive/Documents/GitHub/MpoxTimeSeriesForecastingSD/results/"

#* DATA
TRAIN_WEEK= 84
YEAR_INTEREST = 2023
data_year = data_mpox  %>% filter(Year >= YEAR_INTEREST)

#RUN
var_sd <- run_var_sd(data_year, TRAIN_WEEK, list_jur, correlation_threshold = 0.40)

#COEFFS
df_coeffs = var_sd$result$results_var$df_coeffs
df_coeffs_sd = df_coeffs %>% filter(Response == 'SanDiego')

#SAVE
file_coeffs = 'df_coeffs_sd.rds'
saveRDS(df_coeffs_sd, file = paste0(FOLDER_SAVE, file_coeffs))

#SAVE CSV
file_coeffs <- 'df_coeffs_sd.csv'
write.csv(df_coeffs_sd, file = paste0(FOLDER_SAVE, file_coeffs), row.names = FALSE)


#PLOT
title_plot = 'San diego, corr > 0.40'
sd_var_forecasts = var_sd %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(sd_var_forecasts, sd_var_forecasts, sd_var_forecasts,
                                   sd_var_forecasts,
                                   sd_var_forecasts, title_plot)

#********************************************************************************* QED
# QED






#***************************
#* check inner function

#RUN VAR ORIGINAL FUNCTION
var_sd_orig <- RUN_VAR('SanDiego', data_year, TRAIN_WEEK, list_jur, 0.40)

#Plot
title_plot = 'San diego, corr > 0.40'
var_sd_orig_forecasts = var_sd_orig$var_forecasts
sd_var_forecasts_orig = var_sd_orig_forecasts %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(sd_var_forecasts, sd_var_forecasts, sd_var_forecasts,
                                   sd_var_forecasts,
                                   sd_var_forecasts, title_plot)

df_coeffs_orig = var_sd_orig$results_var$df_coeffs










#*******************************************************************
#* RUN - Correlation of one lag > 0.50
#********************************************************************
correlation_threshold = 0.50
list_jur = c("NewYorkCity", "LA", "Illinois", 'Colorado', "SanDiego",
             "Philadelphia", "Ohio", "Georgia")

data_model = data_year %>% filter(Jurisdiction %in% list_jur)

#RUN VAR
jurisdiction_interest = 'SanDiego'
var_results_sd_05 = RUN_VAR(jurisdiction_interest, data_model,
                            TRAIN_WEEK, list_jur, correlation_threshold) 

jur_level_metrics_sd_05 = var_results_sd_05$jur_level_metrics

#Forecasts
var_forecasts_05 = var_results_sd_05$var_forecasts

#Plot
title_plot = 'San diego, corr > 0.50'
sd_var_forecasts = var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(sd_var_forecasts, sd_var_forecasts, sd_var_forecasts,
                                   sd_var_forecasts,
                                   sd_var_forecasts, title_plot)

# #Coeffs
df_coeffs_05 = var_results_sd_05$results_var$df_coeffs 
df_sd = df_coeffs_05 %>% filter(Response == 'SanDiego')

#*******************************************************************
#* RUN - Correlation of one lag > 0.40
#********************************************************************
correlation_threshold = 0.40

#DATA MODEL
list_jur = c("NewYorkCity", "LA", "Illinois", 'Colorado', "SanDiego",
             "Philadelphia", "Ohio", "Georgia", "NewJersey", "NewYork", "Massachusetts")

data_model = data_year %>% filter(Jurisdiction %in% list_jur)

#***************
#RUN VAR
var_results_sd_04 = RUN_VAR(jurisdiction_interest, data_model,
                               TRAIN_WEEK, list_jur, correlation_threshold) 

jur_level_metrics_04 = var_results_sd_04$jur_level_metrics

#Coeffs
df_coeffs_04 = var_results_sd_04$results_var$df_coeffs
df_sd_04 = df_coeffs_04 %>% filter(Response == 'SanDiego')

#Forecasts
var_forecasts_04 = var_results_sd_04$var_forecasts

#Plot
title_plot = 'San diego, corr > 0.40'
sd_var_forecasts = var_forecasts_04 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(sd_var_forecasts, sd_var_forecasts, sd_var_forecasts,
                                   sd_var_forecasts,
                                   sd_var_forecasts, title_plot)

#Coeffs
df_coeffs_04 = var_results$forecasts$df_coeffs
df_sd = df_coeffs_04 %>% filter(Response == 'SanDiego')
  


#EXTRACT COEFFS

