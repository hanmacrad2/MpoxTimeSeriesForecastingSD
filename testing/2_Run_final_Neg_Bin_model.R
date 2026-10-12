#FINAL NB MODEL


#RUN 2 - BEST!!! 
nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur,
                        lambda_value = 0.1,
                        predictor_type = "level")

#nb_glm_out <- run_nb_sd(data_mpox, 84, list_jur, lambda_value = 0.05)   # no max_forecasts
range(nb_glm_out$forecasts$Week_Number)   # want 86 130
nrow(nb_glm_out$forecasts)                # want 45
nb_glm_out$overall


#COEFFS
nb_coeffs = nb_glm_out$coeffs
nb_coeffs_level = nb_glm_out$coeffs
