library(mpath)
library(dplyr)
library(tidyr)

library(mpath)
library(dplyr)
library(tidyr)

# ============================================================
# NEGATIVE BINOMIAL LASSO FUNCTIONS
# Mirrors VAR_LASSO_FORECAST_TWO_STEP / run_var_sd.
#
# Differences from the VAR-Lasso (state these in the methods):
#  - Fitted on LEVELS (counts), not first differences: the NB
#    likelihood needs non-negative counts.
#  - One NB-lasso equation per jurisdiction, all 25 lags of all
#    jurisdictions as candidates, L1 penalty (alpha = 1).
#  - lambda is ONE fixed value you supply (lambda_value), the
#    same in every rolling window. Choose it before looking at
#    forecast errors.
#  - Step 2 reuses the step-1 fits and feeds the t+1 forecasts in
#    as lag-1 inputs (refit_step2 = FALSE). With TRUE it refits
#    with the rounded t+1 forecast appended as a count.
#  - protected_predictors = list(SanDiego = "Illinois_lag25") exempts that
#    term from the penalty in San Diego's equation only (penalty.factor = 0).
#    It is then always estimated, so say in the methods that it was chosen
#    from the VAR-Lasso finding. alpha = 1 is the lasso.
#  - NO fallbacks: any failed fit or prediction stops with an
#    error naming the forecast, step and jurisdiction.
#
# NOT TESTED: written without access to R. Run the smoke test first.
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

make_lagged_data <- function(data, target, n_lags = 25) {
  data <- as.matrix(data)
  X <- create_lagged_matrix(data, n_lags)
  data.frame(y = data[(n_lags + 1):nrow(data), target], X)
}

# Adds "where we were" to any error raised inside expr
with_context <- function(expr, context) {
  tryCatch(expr, error = function(e) stop(context, " -> ", conditionMessage(e), call. = FALSE))
}

fit_nb_lasso <- function(hist_mat, target, lambda_value, n_lags,
                         alpha = 1, protected = character(0)) {
  d  <- make_lagged_data(hist_mat, target, n_lags)
  pf <- setNames(rep(1, ncol(d) - 1), colnames(d)[-1])
  if (length(protected) > 0) {
    not_found <- setdiff(protected, names(pf))
    if (length(not_found) > 0) {
      stop("protected predictor(s) not found for ", target, ": ",
           paste(not_found, collapse = ", "), call. = FALSE)
    }
    pf[protected] <- 0     # penalty 0 = always in the model, never shrunk
  }
  tryCatch(
    glmregNB(y ~ ., data = d, alpha = alpha, theta.fixed = FALSE,
             lambda = lambda_value, penalty.factor = unname(pf),
             parallel = FALSE),
    error = function(e) stop("glmregNB failed for ", target, ": ",
                             conditionMessage(e), call. = FALSE)
  )
}

predict_nb_next <- function(fit_obj, hist_mat, n_lags, target) {
  hist_mat <- as.matrix(hist_mat)
  dummy <- matrix(0, 1, ncol(hist_mat), dimnames = list(NULL, colnames(hist_mat)))
  X_ext <- create_lagged_matrix(rbind(hist_mat, dummy), n_lags)
  X_new <- as.data.frame(X_ext[nrow(X_ext), , drop = FALSE])
  p <- tryCatch(
    as.numeric(predict(fit_obj, newx = X_new, which = 1, type = "response")),
    error = function(e) stop("predict failed for ", target, ": ",
                             conditionMessage(e), call. = FALSE)
  )
  if (length(p) != 1 || !is.finite(p)) {
    stop("non-finite prediction for ", target, call. = FALSE)
  }
  p
}

nonzero_coefs <- function(fit_obj, response, week) {
  B <- as.matrix(fit_obj$beta)
  b <- as.numeric(B[, 1]); names(b) <- rownames(B)
  nz <- b != 0
  if (!any(nz)) return(NULL)
  data.frame(Response = response, Predictor = names(b)[nz],
             Coefficient = b[nz], Week_Number = week, row.names = NULL)
}

NB_LASSO_FORECAST_TWO_STEP <- function(train_data_ts, future_data_ts, future_real_ts,
                                       ROLL_WINDOW, n_lags = 25, lambda_value,
                                       alpha = 1, protected_predictors = list(),
                                       refit_step2 = FALSE, max_forecasts = NULL) {
  'Recursive 2-step-ahead negative binomial lasso forecasts - saving only the t+2 prediction as a dataframe'
  
  if (missing(lambda_value) || !is.numeric(lambda_value) || length(lambda_value) != 1) {
    stop("Supply one fixed lambda_value, chosen before looking at forecast errors.")
  }
  if (ROLL_WINDOW != 0) {
    warning("Smoothed series are non-integer; the NB likelihood expects counts. Use ROLL_WINDOW = 0.")
  }
  
  print(paste0('N States: ', (ncol(train_data_ts) - 2)))
  print(paste0('Roll window: ', ROLL_WINDOW))
  print(paste0('N lags: ', n_lags))
  print(paste0('N step_ahead: ', 2))
  print(paste0('Fixed lambda: ', lambda_value))
  print(paste0('Alpha: ', alpha))
  print(paste0('Protected predictors: ', if (length(protected_predictors) == 0) 'none' else
    paste(names(protected_predictors), unlist(protected_predictors), sep = ' <- ', collapse = '; ')))
  
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  train_data_ts  <- train_data_ts  %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_ts <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  juris_names <- colnames(train_data_ts)
  n_forecasts <- nrow(future_data_ts) - 1
  if (!is.null(max_forecasts)) n_forecasts <- min(n_forecasts, max_forecasts)
  
  results_df   <- data.frame()
  df_coeffs    <- data.frame()
  train_week_t <- train_data_ts
  
  for (week_t in 1:n_forecasts) {
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    ctx <- paste0("forecast ", week_t, " (week ", week_number_forecast[week_t + 1], ")")
    
    H <- as.matrix(train_week_t)
    
    # 1. Fit one NB-lasso equation per jurisdiction on the levels
    fits1 <- with_context(
      setNames(lapply(juris_names, function(j) fit_nb_lasso(H, j, lambda_value, n_lags, alpha, protected_predictors[[j]])),
               juris_names),
      paste0(ctx, ", step 1 fit"))
    
    # 2. Save non-zero coefficients
    df_coeffs <- bind_rows(
      df_coeffs,
      bind_rows(lapply(juris_names, function(j)
        nonzero_coefs(fits1[[j]], j, week_number_forecast[week_t])))
    )
    
    # 3. Step 1 forecast (t+1), all jurisdictions
    level_t1 <- with_context(
      sapply(juris_names, function(j) predict_nb_next(fits1[[j]], H, n_lags, j)),
      paste0(ctx, ", step 1 prediction"))
    
    # 4. Step 2 forecast (t+2)
    H2 <- rbind(H, level_t1[colnames(H)])
    if (refit_step2) {
      H2fit <- rbind(H, round(level_t1[colnames(H)]))
      fits2 <- with_context(
        setNames(lapply(juris_names, function(j) fit_nb_lasso(H2fit, j, lambda_value, n_lags, alpha, protected_predictors[[j]])),
                 juris_names),
        paste0(ctx, ", step 2 refit"))
    } else {
      fits2 <- fits1
    }
    level_t2 <- with_context(
      sapply(juris_names, function(j) predict_nb_next(fits2[[j]], H2, n_lags, j)),
      paste0(ctx, ", step 2 prediction"))
    
    # 5. Get actual values
    actual_values <- as.numeric(future_real_ts[week_t + 1, juris_names])
    
    # 6. Save results to long-format dataframe
    df_t <- data.frame(
      Week_Number     = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(list_date_week_start[week_t + 1], length(juris_names)),
      Jurisdiction    = juris_names,
      Predicted       = as.numeric(pmax(level_t2[juris_names], 0)),
      Actual          = actual_values
    )
    results_df <- bind_rows(results_df, df_t)
    
    # 7. Extend training set
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>% mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  df_pred <- pivot_wider(
    results_df %>% dplyr::select(Week_Number, date_week_start, Jurisdiction, Predicted),
    names_from = Jurisdiction, values_from = Predicted) %>% arrange(Week_Number)
  df_true <- pivot_wider(
    results_df %>% dplyr::select(Week_Number, date_week_start, Jurisdiction, Actual),
    names_from = Jurisdiction, values_from = Actual) %>% arrange(Week_Number)
  
  return(list(
    df_pred_results = results_df,
    results_df      = results_df,
    preds           = df_pred,
    true            = df_true,
    df_coeffs       = df_coeffs
  ))
}

# ------------------------------------------------------------
# San Diego wrapper, mirrors run_var_sd.
# list_jur = all 11 jurisdictions  -> NB-lasso (VAR-Lasso counterpart)
# list_jur = "SanDiego"            -> NB autoregression-lasso (own lags only)
# ------------------------------------------------------------
run_nb_sd <- function(data_mpox, TRAIN_WEEK, list_jur, lambda_value,
                      jurisdiction_interest = "SanDiego", n_lags = 25,
                      alpha = 1, protected_predictors = list(),
                      model_label = "Negative-Binomial-Lasso",
                      refit_step2 = FALSE, max_forecasts = NULL) {
  
  if (!(jurisdiction_interest %in% list_jur)) stop(jurisdiction_interest, " is not in list_jur")
  
  data_model <- data_mpox %>% filter(Jurisdiction %in% list_jur)
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  results_nb <- NB_LASSO_FORECAST_TWO_STEP(
    train_ts, future_ts, future_ts, ROLL_WINDOW = 0, n_lags = n_lags,
    lambda_value = lambda_value, alpha = alpha,
    protected_predictors = protected_predictors,
    refit_step2 = refit_step2, max_forecasts = max_forecasts
  )
  nb_forecasts <- results_nb$results_df
  
  df_slope    <- GET_DF_WEIGHTED_SLOPE(data_model, list_jur)
  jur_metrics <- GET_METRICS_JURISDICTION(nb_forecasts, list_jur)
  jur_slope   <- GET_SLOPE_METRICS_JURISDICTION(nb_forecasts, df_slope, list_jur)
  
  jur_level_metrics <- left_join(jur_metrics, jur_slope, by = "Jurisdiction") %>%
    mutate(Model = model_label) %>%
    dplyr::select(Jurisdiction, Model, everything())
  
  overall <- jur_level_metrics %>%
    filter(Jurisdiction == jurisdiction_interest) %>%
    dplyr::select(-Jurisdiction)
  
  coeffs <- results_nb$df_coeffs
  if (nrow(coeffs) > 0) coeffs <- coeffs %>% filter(Response == jurisdiction_interest)
  
  list(overall   = overall,
       forecasts = nb_forecasts %>% filter(Jurisdiction == jurisdiction_interest),
       coeffs    = coeffs,
       result    = results_nb)
}

#CHECK
d0       <- data_mpox %>% filter(Jurisdiction %in% list_jur)
sp0      <- get_train_test_data(d0, 84)
train_ts <- GET_TS_DATA(sp0$train_data) %>% dplyr::select(-Week_Number, -date_week_start)
lag_df   <- make_lagged_data(train_ts, "SanDiego", 25)
dim(lag_df)      # expect 59 x 276

fit_en <- glmregNB(y ~ ., data = lag_df, alpha = 0.01, theta.fixed = FALSE, parallel = FALSE)
lam_il <- min(fit_en$lambda)       # 2.158875, the setting that gave Illinois lag 25

# smoke test first: add max_forecasts = 3
nb_il <- run_nb_sd(data_mpox, TRAIN_WEEK = 84, list_jur = list_jur,
                   lambda_value = lam_il, alpha = 0.01,
                   model_label = "NB near-ridge (alpha 0.01)")
nb_il$overall

# check Illinois lag 25 has a value in every rolling window
il <- nb_il$coeffs %>% filter(Predictor == "Illinois_lag25")
c(windows = n_distinct(nb_il$result$results_df$Week_Number),
  with_illinois_lag25 = n_distinct(il$Week_Number))
il %>% dplyr::select(Week_Number, Coefficient)
