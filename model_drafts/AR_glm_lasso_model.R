library(dplyr)
library(glmnet)

# =============================================================================
# AR-LASSO (glmnet) + run_AR_model() wrapper: everything in one file
# Univariate (one series per jurisdiction), differenced counts, p lags,
# recursive two-step forecast, expanding window, refit at every origin and
# again at the second step.
#   lambda = NULL  -> penalty chosen by time-ordered (blocked) cross-validation
#                     inside each training window (only data available at the origin)
#   lambda = value -> fixed penalty. NOTE glmnet's lambda scale is NOT BigVAR's:
#                     with standardized lags of a differenced count series,
#                     lambda above roughly 1-2 removes every lag.
# Needs your existing: get_train_test_data(), GET_TS_DATA(), GET_DF_WEIGHTED_SLOPE(),
#   GET_METRICS_JURISDICTION(), GET_SLOPE_METRICS_JURISDICTION()
# Untested (no R here).
# =============================================================================

# row t holds y_{t-1}, ..., y_{t-p}; response is y_t
make_ar_design <- function(y, p) {
  n <- length(y)
  if (n <= p + 5) stop("Series too short for ", p, " lags (", n, " observations).")
  X <- embed(y, p + 1)[, -1, drop = FALSE]
  colnames(X) <- paste0("L", 1:p)
  list(X = X, y = y[(p + 1):n])
}

fit_ar_glmnet <- function(y, p, lambda = NULL, nfolds = 5, rule = "lambda.min") {
  d <- make_ar_design(y, p)
  if (is.null(lambda)) {
    # contiguous (time-ordered) folds, not random folds
    foldid <- as.integer(cut(seq_len(nrow(d$X)), breaks = nfolds, labels = FALSE))
    cv  <- cv.glmnet(d$X, d$y, alpha = 1, standardize = TRUE, foldid = foldid)
    lam <- cv[[rule]]               # "lambda.min" or "lambda.1se": decide in advance
  } else {
    lam <- lambda
  }
  fit <- glmnet(d$X, d$y, alpha = 1, lambda = lam, standardize = TRUE)
  list(fit = fit, lambda = lam, p = p)
}

predict_next_diff <- function(m, y) {
  x_next <- matrix(rev(tail(y, m$p)), nrow = 1)    # lag 1 = most recent difference
  colnames(x_next) <- paste0("L", 1:m$p)
  as.numeric(predict(m$fit, newx = x_next))
}

AR_glmnet <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                      jur_name, n_lags = 25, lambda = NULL, nfolds = 5,
                      rule = "lambda.min", n_step_ahead = 2) {
  
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  train_vec   <- train_data_ts_jur1[[jur_name]]
  future_vec  <- future_data_ts_jur1[[jur_name]]
  future_real <- future_real_ts_jur1[[jur_name]]
  
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  out <- data.frame(); df_coeffs <- data.frame()
  
  for (week_t in seq_len(n_forecasts)) {
    # step 1: fit on differences, forecast t+1
    m1 <- fit_ar_glmnet(diff(train_vec), n_lags, lambda, nfolds, rule)
    cf <- as.matrix(coef(m1$fit))[-1, 1]          # drop intercept
    nz <- cf[cf != 0]
    if (length(nz) > 0) {
      df_coeffs <- bind_rows(df_coeffs, data.frame(
        Response = jur_name, Predictor = paste0(jur_name, "_", names(nz)),
        Coefficient = as.numeric(nz), Week_Number = week_number_forecast[week_t],
        lambda_used = m1$lambda))
    }
    level_t1 <- tail(train_vec, 1) + predict_next_diff(m1, diff(train_vec))
    
    # step 2: append the t+1 forecast, refit, forecast t+2
    ext      <- c(train_vec, level_t1)
    m2       <- fit_ar_glmnet(diff(ext), n_lags, lambda, nfolds, rule)
    level_t2 <- level_t1 + predict_next_diff(m2, diff(ext))
    
    out <- bind_rows(out, data.frame(
      Week_Number     = week_number_forecast[week_t + 1],
      date_week_start = list_date_week_start[week_t + 1],
      Jurisdiction    = jur_name,
      Predicted       = max(level_t2, 0),
      Actual          = future_real[week_t + 1]))
    
    # extend the training set with the OBSERVED week
    train_vec <- c(train_vec, future_vec[week_t])
  }
  
  out$error <- out$Actual - out$Predicted
  list(forecasts_ar_model = out, df_coeffs = df_coeffs)
}

# =============================================================================
# run_AR_model(): same structure as your other run_* wrappers
# =============================================================================
run_AR_model <- function(data_mpox, ROLL_WINDOW = 0, TRAIN_WEEK,
                         jurs_selection, n_lags_ar = 25,
                         lambda = NULL,            # NULL = blocked CV; number = fixed penalty
                         nfolds = 5, rule = "lambda.min") {
  
  data_raw <- data_mpox %>% filter(Jurisdiction %in% jurs_selection)
  
  # raw weekly counts, no smoothing (ROLL_WINDOW kept only for compatibility)
  data_model <- data_raw
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  # ---- AR-Lasso, one univariate model per selected jurisdiction ---------------
  fc_list <- list(); coef_list <- list()
  for (jur in jurs_selection) {
    tr <- train_ts[,  c("date_week_start", "Week_Number", jur)]
    fu <- future_ts[, c("date_week_start", "Week_Number", jur)]
    out <- AR_glmnet(tr, fu, fu, jur_name = jur, n_lags = n_lags_ar,
                     lambda = lambda, nfolds = nfolds, rule = rule)
    fc_list[[jur]]   <- out$forecasts_ar_model
    coef_list[[jur]] <- out$df_coeffs
  }
  forecasts_ar <- bind_rows(fc_list)
  results_AR   <- list(df_coeffs = bind_rows(coef_list))
  
  # ---- metrics (same pattern as your other wrappers) --------------------------
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, jurs_selection)
  
  build_jur_metrics <- function(forecasts, model_name) {
    jm <- GET_METRICS_JURISDICTION(forecasts, jurs_selection)
    js <- GET_SLOPE_METRICS_JURISDICTION(forecasts, df_slope, jurs_selection)
    left_join(jm, js, by = "Jurisdiction") %>% mutate(Model = model_name)
  }
  
  jur_metrics_all <- bind_rows(
    build_jur_metrics(forecasts_ar, "AR-Lasso")
    #, build_jur_metrics(forecasts_arima, "ARIMA")
    #, build_jur_metrics(forecasts_naive, "Naive")
  ) %>% dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_metrics_all %>%
    group_by(Model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  list(per_jurisdiction = jur_metrics_all, overall = overall,
       forecasts = list(AR_Lasso = forecasts_ar),
       df_coeffs = results_AR$df_coeffs)
}

# ---- usage ---------------------------------------------------------------------
# ar05 <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 25, lambda = 0.05)
# ar05$overall %>% dplyr::select(Model, slope_weighted_rmse, slope_weighted_mae)
# ar_cv <- run_AR_model(data_year, 0, TRAIN_WEEK, "SanDiego", n_lags_ar = 25)   # blocked CV
#
# Non-zero lags per origin (intercept already excluded):
# n_orig <- nrow(ar05$forecasts$AR_Lasso)
# per <- ar05$df_coeffs %>% count(Week_Number)
# c(mean = mean(c(per$n, rep(0, n_orig - nrow(per)))))