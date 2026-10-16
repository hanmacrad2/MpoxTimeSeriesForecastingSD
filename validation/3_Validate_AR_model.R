#RUN AR MODEL

#RUN
ar_8 <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 8, lambda = 0.05)
ar_8$overall %>% dplyr::select(Model, slope_weighted_rmse, slope_weighted_mae)


#*******************
#PLOT

title_plot = 'N lags = 8. AR model. San diego'
ar_forecasts = ar_8$forecasts$AR_Lasso
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(ar_forecasts, ar_forecasts, ar_forecasts,
                                   ar_forecasts,
                                   ar_forecasts, title_plot)




#RUN
ar_hl <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 25, lambda = 0.05)
ar_hl$overall %>% dplyr::select(Model, slope_weighted_rmse, slope_weighted_mae)


#*******************
#PLOT

title_plot = 'AR model. San diego'
ar_forecasts = ar_hl$forecasts$AR_Lasso
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(ar_forecasts, ar_forecasts, ar_forecasts,
                                   ar_forecasts,
                                   ar_forecasts, title_plot)

















ar_05 <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 25, lambda = 0.05)

ar05$overall %>% dplyr::select(Model, slope_weighted_rmse, slope_weighted_mae)

#COEFFS
coeffs_ar = ar_05$df_coeffs 

#*******************
#PLOT

title_plot = 'AR model. San diego'
ar_forecasts_05 = ar_05$forecasts$AR_Lasso
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(ar_forecasts_05, ar_forecasts_05, ar_forecasts_05,
                                   ar_forecasts_05,
                                   ar_forecasts_05, title_plot)

#*************
#COEFFICIENTS

df_ar_coeff <- coeffs_ar %>%
  mutate(Predictor = ifelse(Predictor %in% c("1", "intercept"), "Intercept", Predictor))
