library(MASS)
library(mpath)
library(dplyr)
library(tidyr)

# ============================================================
# CHANGES FROM THE PREVIOUS VERSION
# ============================================================
# 1. REMOVED every fallback to naive persistence (there were three:
#    lambda selection failing, the final fit failing, and predict()
#    failing). A failed fit now STOPS with the jurisdiction, week, step,
#    stage and the underlying error message. Nothing is silently replaced
#    by "last observed value" any more.
# 2. REMOVED the silent "pick the most-regularised lambda if every CV fold
#    failed" path -- that produced a near-intercept-only model without
#    telling you. It now stops with an error instead.
# 3. newx is now passed to predict() as a matrix (glmregNB's predict
#    documents newx as a matrix; the old code passed a data.frame, which is
#    one candidate for the predict errors that were being swallowed).
#
# 4. FIXED the response column: it was being named after the jurisdiction (e.g.
#    "Colorado") instead of "y" when the input was a tibble, so glmregNB could
#    not find 'y'. This was the failure the old naive fallback was hiding.
# 5. parallel = FALSE on all three glmregNB calls (avoids starting parallel
#    workers on every one of the thousands of small fits, and the trace warning).
#
# Everything else (design matrix, expanding-window CV, protected predictors,
# two-step rolling forecast, return shape) is unchanged.

# ============================================================
# 1. LAGGED DESIGN MATRIX
# ============================================================
create_lagged_matrix <- function(data, n_lags) {
  data <- as.matrix(data)
  n <- nrow(data)
  X_list <- list()
  for (lag in 1:n_lags) {
    X_lag <- data[(n_lags + 1 - lag):(n - lag), , drop = FALSE]
    colnames(X_lag) <- paste0(colnames(data), "_lag", lag)
    X_list[[lag]] <- X_lag
  }
  do.call(cbind, X_list)
}

# ============================================================
# 2. EXPANDING-WINDOW CV FOR LAMBDA
# ============================================================
select_lambda_expanding_nb <- function(lag_df, n_folds = 5, min_train_frac = 0.5,
                                       alpha = 1, theta_fixed = FALSE,
                                       penalty_factor = NULL) {
  n <- nrow(lag_df)
  min_train <- floor(n * min_train_frac)
  if (is.null(penalty_factor)) penalty_factor <- rep(1, ncol(lag_df) - 1)
  
  # Full-window fit only supplies the lambda sequence. If THIS fails the
  # model cannot be fitted at all, so stop with the reason.
  full_fit <- tryCatch(
    glmregNB(y ~ ., parallel = FALSE, data = lag_df, alpha = alpha, theta.fixed = theta_fixed,
             penalty.factor = penalty_factor),
    error = function(e) {
      # Save exactly what was passed in so the failure can be reproduced at the console
      assign("debug_lag_df", lag_df, envir = .GlobalEnv)
      assign("debug_penalty_factor", penalty_factor, envir = .GlobalEnv)
      stop("glmregNB failed on the full window: ", conditionMessage(e), call. = FALSE)
    }
  )
  lambda_seq <- full_fit$lambda
  
  cut_points <- unique(floor(seq(min_train, n - 1, length.out = n_folds + 1)))
  n_folds_actual <- max(length(cut_points) - 1, 0)
  
  loglik_mat <- matrix(NA_real_, nrow = length(lambda_seq),
                       ncol = max(n_folds_actual, 1))
  
  if (n_folds_actual >= 1) {
    for (k in seq_len(n_folds_actual)) {
      train_rows <- 1:cut_points[k]
      valid_rows <- (cut_points[k] + 1):cut_points[k + 1]
      
      # A single failed CV fold is tolerated (it just contributes no
      # log-likelihood); if ALL folds fail we stop below.
      fit_k <- tryCatch(
        glmregNB(y ~ ., parallel = FALSE, data = lag_df[train_rows, ], alpha = alpha,
                 theta.fixed = theta_fixed, lambda = lambda_seq,
                 penalty.factor = penalty_factor),
        error = function(e) NULL
      )
      if (is.null(fit_k) || length(fit_k$theta) != length(lambda_seq)) next
      
      preds <- tryCatch(
        predict(fit_k, newx = as.matrix(lag_df[valid_rows, -1, drop = FALSE]),
                which = seq_along(lambda_seq), type = "response"),
        error = function(e) NULL
      )
      if (is.null(preds)) next
      
      for (l in seq_along(lambda_seq)) {
        mu_l <- if (is.matrix(preds)) preds[, l] else preds
        loglik_mat[l, k] <- sum(dnbinom(
          lag_df$y[valid_rows], mu = mu_l, size = fit_k$theta[l], log = TRUE
        ))
      }
    }
  }
  
  mean_loglik <- rowMeans(loglik_mat, na.rm = TRUE)
  
  if (all(is.nan(mean_loglik))) {
    stop("all ", max(n_folds_actual, 1), " expanding-window CV folds failed (",
         n, " rows, ", ncol(lag_df) - 1, " predictors); cannot select lambda.",
         call. = FALSE)
  }
  best_l <- which.max(mean_loglik)
  
  list(lambda = lambda_seq[best_l], which = best_l,
       mean_loglik = mean_loglik, lambda_seq = lambda_seq,
       loglik_mat = loglik_mat)
}

# ============================================================
# 3. ONE FORECAST STEP, ALL JURISDICTIONS
# ============================================================
fit_and_forecast_step <- function(data_window, juris_names, n_lags,
                                  protected_predictors = list(),
                                  n_folds = 5, min_train_frac = 0.5,
                                  week_label, step_label) {
  
  X <- create_lagged_matrix(data_window, n_lags)
  
  dummy_row <- matrix(0, nrow = 1, ncol = ncol(data_window),
                      dimnames = list(NULL, colnames(data_window)))
  data_window_ext <- rbind(as.matrix(data_window), dummy_row)
  X_ext <- create_lagged_matrix(data_window_ext, n_lags)
  X_new <- X_ext[nrow(X_ext), , drop = FALSE]   # kept as a MATRIX for predict()
  
  preds <- setNames(numeric(length(juris_names)), juris_names)
  coeffs_out <- data.frame()
  
  # Any failure below stops the run and says exactly where it happened.
  fail <- function(stage, e, j) {
    stop(sprintf("NB model failed [%s] for %s, week %s, step %s: %s",
                 stage, j, week_label, step_label, conditionMessage(e)),
         call. = FALSE)
  }
  
  for (j in juris_names) {
    
    # Plain numeric vector. (data_window[rows, j] returns a one-column tibble when
    # data_window is a tibble, and data.frame(y = <that>, X) then names the response
    # column after the jurisdiction instead of "y" -- glmregNB then cannot find 'y'.)
    y_j <- as.numeric(data_window[[j]])[(n_lags + 1):nrow(data_window)]
    lag_df <- data.frame(y = y_j, X)
    if (!("y" %in% names(lag_df))) stop("response column 'y' missing from lag_df for ", j)
    
    penalty_factor <- setNames(rep(1, ncol(X)), colnames(X))
    if (!is.null(protected_predictors[[j]])) {
      penalty_factor[protected_predictors[[j]]] <- 0
    }
    
    cv_result <- tryCatch(
      select_lambda_expanding_nb(
        lag_df, n_folds = n_folds, min_train_frac = min_train_frac,
        penalty_factor = unname(penalty_factor)
      ),
      error = function(e) fail("lambda selection", e, j)
    )
    
    final_fit <- tryCatch(
      glmregNB(y ~ ., parallel = FALSE, data = lag_df, alpha = 1, theta.fixed = FALSE,
               lambda = cv_result$lambda, penalty.factor = unname(penalty_factor)),
      error = function(e) fail("final fit", e, j)
    )
    
    preds[j] <- tryCatch(
      as.numeric(predict(final_fit, newx = X_new, which = 1, type = "response")),
      error = function(e) fail("predict", e, j)
    )
    
    beta_j <- as.numeric(final_fit$beta[, 1])
    nz <- beta_j != 0
    if (any(nz)) {
      coeffs_out <- rbind(coeffs_out, data.frame(
        Jurisdiction = j,
        predictor    = rownames(final_fit$beta)[nz],
        coefficient  = beta_j[nz],
        lambda       = cv_result$lambda,
        step         = step_label,
        Week_Number  = week_label
      ))
    }
  }
  
  list(preds = preds, coeffs = coeffs_out)
}

# ============================================================
# 4. MAIN ENTRY POINT (unchanged apart from the above)
# ============================================================
NEG_BIN_FORECAST <- function(train_data_ts, future_data_ts, future_real_ts, ROLL_WINDOW,
                             n_lags = 25, protected_predictors = list(),
                             n_folds = 5, min_train_frac = 0.5) {
  
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  train_data_ts  <- train_data_ts  %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_ts <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  juris_names <- colnames(train_data_ts)
  n_forecasts <- nrow(future_data_ts) - 1
  
  results_df <- data.frame()
  df_coeffs  <- data.frame()
  train_week_t <- train_data_ts
  
  for (week_t in 1:n_forecasts) {
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    
    step1 <- fit_and_forecast_step(
      train_week_t, juris_names, n_lags, protected_predictors,
      n_folds, min_train_frac, week_number_forecast[week_t], step_label = 1
    )
    level_t1 <- step1$preds
    
    extended_series <- rbind(train_week_t, level_t1)
    step2 <- fit_and_forecast_step(
      extended_series, juris_names, n_lags, protected_predictors,
      n_folds, min_train_frac, week_number_forecast[week_t], step_label = 2
    )
    level_t2 <- step2$preds
    
    df_coeffs <- rbind(df_coeffs, step1$coeffs, step2$coeffs)
    
    actual_values <- as.numeric(future_real_ts[week_t + 1, ])
    
    df_t <- data.frame(
      Week_Number     = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), length(juris_names)),
      Jurisdiction    = juris_names,
      Predicted       = pmax(level_t2, 0),
      Actual          = actual_values
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>% mutate(error = Actual - Predicted)
  
  return(list(results_df = results_df, df_coeffs = df_coeffs))
}


#RUN NEG BIN
RUN_NEG_BIN <- function(jurisdiction_interest, data_mpox, ROLL_WINDOW, TRAIN_WEEK,
                        list_jur, correlation_threshold = NA, n_lags = 25,
                        protected_predictors = list()){
  
  if (!(jurisdiction_interest %in% list_jur)) {
    stop(jurisdiction_interest, " is not in list_jur -- add it, or nothing will be forecast for it.")
  }
  
  #-------------------------------------------------
  # PREPARE DATA (same as RUN_VAR)
  #-------------------------------------------------
  print(paste0('Roll window: ', ROLL_WINDOW))
  data_mpox <- data_mpox %>%
    filter(Jurisdiction %in% list_jur)
  
  split <- get_train_test_data(data_mpox, TRAIN_WEEK)
  
  train_ts  <- GET_TS_DATA(split$train_data)
  future_ts <- GET_TS_DATA(split$future_data)
  
  #SMOOTH
  if(ROLL_WINDOW != 0){
    data_model <- GET_SMOOTH_DATA(data_mpox, ROLL_WINDOW)
  } else {
    data_model <- data_mpox
  }
  
  split_smooth <- get_train_test_data(data_model, TRAIN_WEEK)
  
  train_ts_smooth  <- GET_TS_DATA(split_smooth$train_data)
  future_ts_smooth <- GET_TS_DATA(split_smooth$future_data)
  
  #-------------------------------------------------
  # RUN
  #-------------------------------------------------
  ## Negative binomial (uses lags of every jurisdiction in list_jur as predictors)
  results_nb <- NEG_BIN_FORECAST(
    train_ts_smooth, future_ts_smooth, future_ts, ROLL_WINDOW,
    n_lags = n_lags, protected_predictors = protected_predictors
  )
  
  nb_forecasts = results_nb$results_df
  
  metrics_nb <- GET_METRICS(nb_forecasts)
  
  print(metrics_nb)
  
  #-------------------------------------------------
  # JURISDICTION-LEVEL METRICS
  #-------------------------------------------------
  df_slope <- GET_DF_WEIGHTED_SLOPE(data_model, list_jur)
  
  jur_metrics_nb <- GET_METRICS_JURISDICTION(nb_forecasts, list_jur)
  jur_slope_nb   <- GET_SLOPE_METRICS_JURISDICTION(nb_forecasts, df_slope, list_jur)
  
  jur_level_metrics <- left_join(jur_metrics_nb, jur_slope_nb, by = "Jurisdiction") %>%
    mutate(Model = "Negative-Binomial-AR") %>%
    dplyr::select(Jurisdiction, Model, everything())
  
  #PLOT ONE JURISDICTION
  title_plot = paste0(jurisdiction_interest, ' Negative binomial forecasts. Correlation threshold: ', correlation_threshold)
  jur_nb_forecasts = nb_forecasts %>% filter(Jurisdiction == jurisdiction_interest)
  PLOT_FORECASTS_SINGLE_JURISDICITON(jur_nb_forecasts, jur_nb_forecasts, jur_nb_forecasts,
                                     jur_nb_forecasts,
                                     jur_nb_forecasts, title_plot)
  
  return(list(
    results_nb = results_nb,               # original model output ($results_df, $df_coeffs)
    nb_forecasts = nb_forecasts,           # prediction dataframe
    metrics = metrics_nb,                  # pooled metrics (all jurisdictions in list_jur)
    jur_level_metrics = jur_level_metrics  # per-jurisdiction metrics
  ))
}

# Quick use -- San Diego with the same information set its VAR-Lasso gets
# (itself + its r >= 0.40 predictors), so the two models are like for like:
sd_predictors <- predictor_selection_040 %>%
  filter(jurisdiction_1 == "SanDiego", Selected) %>% pull(jurisdiction_2)

nb_sd <- RUN_NEG_BIN("SanDiego", data_mpox, ROLL_WINDOW = 0, TRAIN_WEEK = 120, #84,
                     list_jur = c("SanDiego", sd_predictors), correlation_threshold = 0.40)
nb_sd$jur_level_metrics