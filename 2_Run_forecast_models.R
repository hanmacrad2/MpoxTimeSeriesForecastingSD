#RUN ALL MODELS

# =============================================================================
# Rolling two-week-ahead forecasts for San Diego County (SDC), 2024:
# runs all five models and compares their forecast accuracy
#
# Models
#   1. VAR-Lasso                 (SDC plus screened predictor jurisdictions)
#   2. Negative binomial AR-Lasso (SDC own lags only, up to 8)
#   3. AR-Lasso                  (SDC own lags only, up to 8)
#   4. ARIMA                     (SDC only, max AR order 8)
#   5. Naive persistence         (last observed weekly count)
#
# Design
#   Expanding-window rolling forecasts: at each origin the model is refitted to
#   all data available up to that week and a two-week-ahead forecast is kept.
#   Penalty lambda = 0.1 for all three penalised models; no model setting is
#   chosen using the 2024 forecasts.
#
# Outputs
#   comparison_fitted_models : slope-weighted RMSE, MAE and bias per model
#                              (main results table)
#   forecast plot            : "San Diego County Forecasts, 2024"
#   percentage differences   : error of each model relative to the VAR-Lasso
#
# Needs: data_mpox, list_jur, and the model functions run_var_sd(),
#   run_nb_sd(), run_AR_model(), run_ARIMA_model(), run_naive_model(),
#   Plot_final_models()
# =============================================================================

#Model training settings
TRAIN_WEEK = 84
lambda = 0.1
jurisdiction = "SanDiego"

#1. RUN VAR MODEL
var_lasso_forecasts <- run_var_sd(data_model, TRAIN_WEEK, list_jur, lambda)

#2. RUN NEG BIN MODEL
nb_ar_lasso_forecasts <- run_nb_sd(data_model, TRAIN_WEEK, n_lags = 8, lambda)

#3.AR MODEL
ar_lasso_forecasts <- run_AR_model(data_model, TRAIN_WEEK, jurisdiction, n_lags_ar = 8,
                         lambda)

#4. ARIMA MODEL
arima_forecasts <- run_ARIMA_model(data_model, 0, TRAIN_WEEK, jurisdiction, max_p = 8)

#5. Naive Persistance model
naive_forecasts <- run_naive_model(data_model, TRAIN_WEEK, jurisdiction, window = 1,
                        model_label = "Naive (persistence)")


#**************************
#COMPARISON
comparison_fitted_models = bind_rows(var_lasso_forecasts$overall, nb_ar_lasso_forecasts$overall,
                                     ar_lasso_forecasts$overall, arima_forecasts$overall, naive_forecasts$overall)

#PLOT
title_plot = 'San Diego County Forecasts, 2024'
Plot_final_models(var_lasso_forecasts, nb_ar_lasso_forecasts, ar_lasso_forecasts, arima_forecasts,
                  naive_forecasts, title_plot)


#*******************************
#Comparison
var_rmse <- comparison_fitted_models$slope_weighted_rmse[comparison_fitted_models$Model == "VAR-Lasso"]
var_mae  <- comparison_fitted_models$slope_weighted_mae[comparison_fitted_models$Model == "VAR-Lasso"]

comparison_final %>%
  mutate(pct_rmse = round(100 * (slope_weighted_rmse - var_rmse) / slope_weighted_rmse, 2),
         pct_mae  = round(100 * (slope_weighted_mae  - var_mae)  / slope_weighted_mae,  2))


