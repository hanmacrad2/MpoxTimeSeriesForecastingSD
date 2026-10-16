#* **************************************************************************************************

#RUN 3
nb_glm_out_005 <- run_nb_sd(data_mpox, 84, list_jur,
                            lambda_value = 0.05,
                            predictor_type = "level")


#PLOT
title_plot = 'Neg Bin lambda = 0.05;'
nb_sd_forecasts_01 = nb_sd_forecasts
nb_sd_forecasts = nb_glm_out_005$forecasts %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(nb_sd_forecasts, nb_sd_forecasts, nb_sd_forecasts,
                                   nb_sd_forecasts,
                                   nb_sd_forecasts, title_plot)

#nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur, lambda_value = 0.05)   # no max_forecasts
range(nb_glm_out$forecasts$Week_Number)   # want 86 130
nrow(nb_glm_out$forecasts)                # want 45
nb_glm_out$overall


#RUN 2 - BEST!!! 
nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur,
                        lambda_value = 0.1,
                        predictor_type = "level")

#PLOT
title_plot = 'Neg Bin lambda = 0.05;'
nb_sd_forecasts = nb_glm_out$forecasts %>% filter(Jurisdiction == 'SanDiego')
PLOT_FORECASTS_SINGLE_JURISDICITON(nb_sd_forecasts, nb_sd_forecasts, nb_sd_forecasts,
                                   nb_sd_forecasts,
                                   nb_sd_forecasts, title_plot)


#nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur, lambda_value = 0.05)   # no max_forecasts
range(nb_glm_out$forecasts$Week_Number)   # want 86 130
nrow(nb_glm_out$forecasts)                # want 45
nb_glm_out$overall

