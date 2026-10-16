#5. VALIDATE NAIVE MOVING AVERAGE

# ---- usage ---------------------------------------------------------------------
ma4  <- run_naive_model(data_year, TRAIN_WEEK, "SanDiego", window = 4)
last <- run_naive_model(data_year, TRAIN_WEEK, "SanDiego", window = 1,
                        model_label = "Naive (last week)")
bind_rows(ma4$overall, last$overall) %>%
  dplyr::select(Model, slope_weighted_rmse, slope_weighted_mae)


#*******************
#PLOT

title_plot = 'N lags = 8. NAIVE model. San diego'
naive_forecasts = ma4$forecasts$Naive
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(naive_forecasts, naive_forecasts, naive_forecasts,
                                   naive_forecasts,
                                   naive_forecasts, title_plot)

ma4$overall

#*******************
#PLOT - LAST

title_plot = 'Persist NAIVE model. San diego'
persist_forecasts = last$forecasts$Naive
#var_forecasts_05 %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(persist_forecasts, persist_forecasts, persist_forecasts,
                                   persist_forecasts,
                                   persist_forecasts, title_plot)

last$overall
