#VALIDATE ARIMA MODEL

#RUN AR MODEL

#RUN
arima <- run_ARIMA_model(data_year, 0, TRAIN_WEEK, "SanDiego", max_p = 8)

arima$overall %>% dplyr::select(Model, slope_weighted_rmse, slope_weighted_mae)


#*******************
#PLOT

title_plot = 'N lags = 8. ARIMA model. San diego'
arima_forecasts = arima$forecasts$ARIMA
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(arima_forecasts, arima_forecasts, arima_forecasts,
                                   arima_forecasts,
                                   arima_forecasts, title_plot)
