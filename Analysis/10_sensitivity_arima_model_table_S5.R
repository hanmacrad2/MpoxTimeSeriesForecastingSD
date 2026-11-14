library(dplyr)
library(tidyr)
library(forecast)

# Needs your existing objects/functions already loaded:
#   data_mpox (columns incl. Jurisdiction, Cases), TRAIN_WEEK, pop_df
#   get_train_test_data(), GET_TS_DATA(), GET_DF_WEIGHTED_SLOPE(),
#   GET_METRICS_JURISDICTION(), GET_SLOPE_METRICS_JURISDICTION()

#*******************************************
#* 1. ARIMA MODEL (records the selected orders at every origin)
#*******************************************
ARIMA_model <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                        jur_name, n_step_ahead = 2, max_p = 8) {
  
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  
  train_week_t  <- train_data_ts_jur1[[jur_name]]
  future_values <- future_data_ts_jur1[[jur_name]]
  future_real   <- future_real_ts_jur1[[jur_name]]
  
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  
  results_df <- data.frame()
  orders_df  <- data.frame()
  
  fit_arima <- function(x) {
    auto.arima(x, seasonal = FALSE, stepwise = TRUE, approximation = FALSE,
               max.p = max_p, max.order = max_p + 2)
  }
  
  for (week_t in seq_len(n_forecasts)) {
    
    print(paste0("week_t: ", week_t))
    
    # STEP 1: fit on observed data, predict t+1
    fit   <- fit_arima(train_week_t)
    pred1 <- as.numeric(forecast(fit, h = 1)$mean)
    
    # STEP 2: append t+1 prediction, refit (orders re-selected), predict t+2
    extended_series <- c(train_week_t, pred1)
    fit2  <- fit_arima(extended_series)
    pred2 <- as.numeric(forecast(fit2, h = 1)$mean)
    
    # record selected orders for both fits
    o1 <- unname(forecast::arimaorder(fit))[1:3]
    o2 <- unname(forecast::arimaorder(fit2))[1:3]
    orders_df <- dplyr::bind_rows(
      orders_df,
      data.frame(Week_Number = week_number_forecast[week_t], Fit = "observed",
                 p = o1[1], d = o1[2], q = o1[3]),
      data.frame(Week_Number = week_number_forecast[week_t], Fit = "extended",
                 p = o2[1], d = o2[2], q = o2[3]))
    
    df_t <- data.frame(
      Week_Number     = week_number_forecast[week_t + 1],
      date_week_start = list_date_week_start[week_t + 1],
      Jurisdiction    = jur_name,
      Predicted       = max(pred2, 0),
      Actual          = future_real[week_t + 1]
    )
    results_df <- dplyr::bind_rows(results_df, df_t)
    
    # expand training set with the observed week
    train_week_t <- c(train_week_t, future_values[week_t])
  }
  
  results_df$error <- results_df$Actual - results_df$Predicted
  
  list(forecasts = results_df, orders = orders_df)   # NOTE: now a list
}

run_ARIMA_model <- function(data_mpox, ROLL_WINDOW = 0, TRAIN_WEEK,
                            jurs_selection, max_p = 8, n_step_ahead = 2,
                            model_label = "ARIMA", slope_data = NULL) {
  
  data_model <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  fc_list <- list(); ord_list <- list()
  for (jur in jurs_selection) {
    tr <- train_ts[,  c("date_week_start", "Week_Number", jur)]
    fu <- future_ts[, c("date_week_start", "Week_Number", jur)]
    res <- ARIMA_model(train_data_ts_jur1 = tr, future_data_ts_jur1 = fu,
                       future_real_ts_jur1 = fu, jur_name = jur,
                       n_step_ahead = n_step_ahead, max_p = max_p)
    fc_list[[jur]]  <- res$forecasts
    ord_list[[jur]] <- res$orders %>% mutate(Jurisdiction = jur)
  }
  forecasts_arima <- bind_rows(fc_list)
  orders_arima    <- bind_rows(ord_list)
  
  list(forecasts = forecasts_arima, orders = orders_arima,
       data_model = data_model, jurs = jurs_selection, label = model_label)
}

# metrics on whatever forecast data frame you pass (counts scale)
arima_metrics <- function(forecasts, data_for_slope, jurs, label = "ARIMA") {
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_for_slope, jurs)
  jm <- GET_METRICS_JURISDICTION(forecasts, jurs)
  js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs)
  left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = label)
}

#*******************************************
#* 2. MAIN ARIMA RUN + ORDER STABILITY
#*******************************************
arima_out <- run_ARIMA_model(data_year, 0, TRAIN_WEEK, "SanDiego", max_p = 8)

arima_main_metrics <- arima_metrics(arima_out$forecasts, arima_out$data_model, "SanDiego")
print(arima_main_metrics)       # must match your reported ARIMA numbers

n_orig <- length(unique(arima_out$orders$Week_Number))   # should be 45
cat("Forecast origins:", n_orig, "\n")

order_table <- function(orders, fit_label) {
  orders %>%
    filter(Fit == fit_label) %>%
    count(p, d, q, name = "n_origins") %>%
    mutate(Pct = round(100 * n_origins / n_orig, 1),
           Order = paste0("(", p, ",", d, ",", q, ")")) %>%
    arrange(desc(n_origins)) %>%
    select(Order, n_origins, Pct)
}

cat("\nSelected orders, fit on observed data (step 1):\n")
print(as.data.frame(order_table(arima_out$orders, "observed")))

cat("\nSelected orders, refit on extended series (step 2):\n")
print(as.data.frame(order_table(arima_out$orders, "extended")))

print(as.data.frame(
  bind_rows(arima_main_metrics, arima_rate_metrics) %>%
    select(Model, everything(), -Jurisdiction)))

# marginal summaries
for (v in c("p", "d", "q")) {
  cat("\nDistribution of", v, "(observed fit):\n")
  print(table(arima_out$orders %>% filter(Fit == "observed") %>% pull(v)))
}

#write.csv(order_table(arima_out$orders, "observed"), "stability_ARIMA_orders_observed.csv", row.names = FALSE)
#write.csv(order_table(arima_out$orders, "extended"), "stability_ARIMA_orders_extended.csv", row.names = FALSE)

