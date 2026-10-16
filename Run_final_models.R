#RUN ALL MODELS

#TRAIN WEEK
TRAIN_WEEK = 84
data_year  = data_mpox %>% filter(Year >= 2023)

#1. RUN VAR MODEL
lambda = 0.05
var_sd <- run_var_sd(data_year, TRAIN_WEEK, list_jur, lambda)

#2. RUN NEG BIN MODEL
nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur, lambda_value = lambda,
                        predictor_type = "level", refit_step2 = TRUE)

#3.AR MODEL
ar_model <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 8, lambda = lambda)

#4. ARIMA MODEL
arima_model <- run_ARIMA_model(data_year, 0, TRAIN_WEEK, "SanDiego", max_p = 8)

#5. Naive Persistance model
naive_model <- run_naive_model(data_year, TRAIN_WEEK, "SanDiego", window = 1,
                        model_label = "Naive (last week)")

#**************************
#COMPARISON
comparison_best = bind_rows(var_sd$overall, nb_glm_out$overall,
                            ar_model$overall, arima_model$overall, naive_model$overall)

comparison_final = comparison_best

#*******************************

#comparison %
var_rmse <- comparison_final$slope_weighted_rmse[comparison_final$Model == "VAR-Lasso"]
var_mae  <- comparison_final$slope_weighted_mae[comparison_final$Model == "VAR-Lasso"]

comparison_final %>%
  mutate(pct_rmse = round(100 * (slope_weighted_rmse - var_rmse) / slope_weighted_rmse, 2),
         pct_mae  = round(100 * (slope_weighted_mae  - var_mae)  / slope_weighted_mae,  2))

#PLOT
title_plot = 'San Diego County Forecasts, 2024'
Plot_final_models(var_sd, nb_glm_out, ar_model, arima_model, naive_model, title_plot)




















#VERSION OLD
TRAIN_WEEK = 84
data_year  = data_mpox %>% filter(Year >= 2023)

#1. RUN VAR MODEL
var_sd <- run_var_sd(data_year, TRAIN_WEEK, list_jur, correlation_threshold = 0.40)

#2. RUN NEG BIN MODEL
nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur, lambda_value = 0.1,
                        predictor_type = "level", refit_step2 = TRUE)

#3. RUN UNI-VARIATE MODELS
univariate_results <- run_univariate_models(data_year, 0, TRAIN_WEEK, "SanDiego")

#COMPARISON
comparison_best = bind_rows(var_sd$overall, nb_glm_out$overall, univariate_results$overall)

#comparison %
var_rmse <- comparison_final$slope_weighted_rmse[comparison_final$Model == "VAR-Lasso"]
var_mae  <- comparison_final$slope_weighted_mae[comparison_final$Model == "VAR-Lasso"]

comparison_final %>%
  mutate(pct_rmse = round(100 * (slope_weighted_rmse - var_rmse) / slope_weighted_rmse, 2),
         pct_mae  = round(100 * (slope_weighted_mae  - var_mae)  / slope_weighted_mae,  2))

#PLOT
title_plot = 'San Diego County Forecasts, 2024'
plot_sd_five_models(var_sd, nb_glm_out2, univariate_results, title_plot)


#**************
#* COEFFICIENT TESTS
range(nb_glm_out$forecasts$Week_Number)   # want 86 130
nrow(nb_glm_out$forecasts)                # want 45
nb_glm_out$overall


#COEFFS
nb_coeffs = nb_glm_out$coeffs
nb_coeffs_level = nb_glm_out$coeffs
