#### RUN VAR LASSO LAMBDA TEST 

#RUN VAR WITH CROSS VAL
lambda_value = 0.1
var_cross_val <- run_var_sd(data_year, TRAIN_WEEK, list_jur_040, lambda_value)


#RUN VAR WITH CROSS VAL
lambda_value = 0.05
var_cross_val <- run_var_sd(data_year, TRAIN_WEEK, list_jur_040, lambda_value)



var_lambdas <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)


#RE-RUN
tune_var <- lapply(var_lambdas, function(lambda_value)
  run_var_sd(data_mpox, 75, list_jur, lambda_value, max_forecasts = 8))

#RERUN
data.frame(
  lambda   = var_lambdas,
  MAE_SD   = sapply(tune_var, function(o) mean(abs(o$forecasts$Predicted - o$forecasts$Actual))),
  RMSE_SD  = sapply(tune_var, function(o) sqrt(mean((o$forecasts$Predicted - o$forecasts$Actual)^2))),
  MAE_all  = sapply(tune_var, function(o) with(o$result$var_forecasts, mean(abs(Predicted - Actual)))),
  max_pred = sapply(tune_var, function(o) max(o$result$var_forecasts$Predicted)),
  nonzero  = sapply(tune_var, function(o) nrow(o$coeffs) / length(unique(o$coeffs$Week_Number))),
  il_L25   = sapply(tune_var, function(o) sum(o$coeffs$Predictor == "Illinois" & o$coeffs$Lag == 25))
)

#HIGH
var_lambdas_hi <- c(20, 50, 100, 200)
tune_hi <- lapply(var_lambdas_hi, function(lambda_value)
  run_var_sd(data_mpox, 75, list_jur, lambda_value, max_forecasts = 8))

#COMBINE
library(ggplot2)

all_lambdas <- c(var_lambdas, var_lambdas_hi)
tune_all    <- c(tune_var, tune_hi)

df_tune <- data.frame(
  lambda   = all_lambdas,
  MAE_SD   = sapply(tune_all, function(o) mean(abs(o$forecasts$Predicted - o$forecasts$Actual))),
  RMSE_SD  = sapply(tune_all, function(o) sqrt(mean((o$forecasts$Predicted - o$forecasts$Actual)^2))),
  MAE_all  = sapply(tune_all, function(o) with(o$result$var_forecasts, mean(abs(Predicted - Actual)))),
  nonzero  = sapply(tune_all, function(o) nrow(o$coeffs) / length(unique(o$coeffs$Week_Number))),
  il_L25   = sapply(tune_all, function(o) sum(o$coeffs$Predictor == "Illinois" & o$coeffs$Lag == 25))
)

ggplot(df_tune, aes(lambda, MAE_SD)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(x = expression(lambda~"(log scale)"), y = "San Diego validation MAE",
       title = "VAR-Lasso penalty sensitivity, weeks 77-84") +
  theme_minimal()

sapply(tune_var, function(o) nrow(o$forecasts))                 # want 8
sapply(tune_var, function(o) range(o$forecasts$Week_Number))    # want 77, 84
identical(tune_var[[1]]$forecasts$Predicted, tune_var[[5]]$forecasts$Predicted) 

#OUT
names(tune_var[[1]]$coeffs)   # run this first, see note below

data.frame(
  lambda   = var_lambdas,
  MAE_SD   = sapply(tune_var, function(o) mean(abs(o$forecasts$Predicted - o$forecasts$Actual))),
  RMSE_SD  = sapply(tune_var, function(o) sqrt(mean((o$forecasts$Predicted - o$forecasts$Actual)^2))),
  MAE_all  = sapply(tune_var, function(o) with(o$result$var_forecasts, mean(abs(Predicted - Actual)))),
  max_pred = sapply(tune_var, function(o) max(o$result$var_forecasts$Predicted)),
  nonzero  = sapply(tune_var, function(o) nrow(o$coeffs) / length(unique(o$coeffs$Week_Number))),
  il_L25   = sapply(tune_var, function(o) sum(o$coeffs$Predictor == "Illinois" & o$coeffs$Lag == 25))
)


#VALIDATE LAMBDA
Y <- as.matrix(GET_TS_DATA(get_train_test_data(
  data_mpox %>% filter(Jurisdiction %in% list_jur), 75)$train_data) %>%
    dplyr::select(-Week_Number, -date_week_start))
train_diff <- diff(Y, differences = 1)

for (lam in c(0.05, 0.2, 1, 5, 20)) {
  m  <- constructModel(train_diff, p = 25, struct = 'Basic',
                       gran = c(lam), ownlambdas = TRUE,
                       T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
  cv <- cv.BigVAR(m)
  cat('lambda', lam, '-> OptimalLambda', cv@OptimalLambda,
      '| nonzero', sum(coef(cv) != 0), '\n')
}

#VALIDATE
jur <- colnames(Y)
i_sd <- which(jur == "SanDiego")
i_il <- which(jur == "Illinois")
col_il25 <- 1 + (25 - 1) * length(jur) + i_il    # +1 for the intercept column

for (lam in c(0.05, 0.2, 1, 5, 20, 50, 100)) {
  m  <- constructModel(train_diff, p = 25, struct = 'Basic',
                       gran = c(lam), ownlambdas = TRUE,
                       T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
  cv <- cv.BigVAR(m)
  B  <- coef(cv)
  cat('lambda', lam,
      '| nonzero', sum(B != 0),
      '| pct', round(100 * mean(B != 0), 1),
      '| IL_L25->SD coef', signif(B[i_sd, col_il25], 4), '\n')
}

#CHECK
dim(coef(cv))                 # want 11 x (1 + 11*25) = 11 x 276
colnames(coef(cv))[1:13]      # if named, confirms intercept-then-lag-1 layout



#********************
#* RUN VAR 


