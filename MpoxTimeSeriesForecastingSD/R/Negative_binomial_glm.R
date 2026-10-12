## NEG BINOMIAL MODEL

#NEGATIVE BINOMIAL MODEL

#NEGATIVE BINOMIAL MODEL

library(mpath); library(dplyr); library(tidyr)

# ---- helpers -------------------------------------------------------------
# lag-major design matrix, rows (p+1):n, columns like "Illinois_L25"
nb_design <- function(Z, p, tag = "_L") {
  n <- nrow(Z)
  do.call(cbind, lapply(1:p, function(l) {
    m <- Z[(p + 1 - l):(n - l), , drop = FALSE]
    colnames(m) <- paste0(colnames(Z), tag, l)
    m
  }))
}

# predictor row for the NEXT (unobserved) week, same column order as nb_design
nb_next_row <- function(Z, p, tag = "_L") {
  n <- nrow(Z)
  x <- unlist(lapply(1:p, function(l) Z[n + 1 - l, ]))
  names(x) <- unlist(lapply(1:p, function(l) paste0(colnames(Z), tag, l)))
  x
}

# log-then-difference: Dfull[t, ] = log1p(Y[t, ]) - log1p(Y[t-1, ]); row 1 is NA
nb_log_diff <- function(Y) {
  Z <- log1p(Y)
  D <- rbind(rep(NA_real_, ncol(Z)), diff(Z, differences = 1))
  colnames(D) <- colnames(Z)
  D
}

# build predictors for the training rows; drops the first usable row so that
# the differenced columns are complete (lag p of a difference needs week t-p-1)
nb_build_X <- function(Y, p, predictor_type = "both") {
  Z <- log1p(Y); D <- nb_log_diff(Y)
  XL <- nb_design(Z, p, "_L")[-1, , drop = FALSE]
  XD <- nb_design(D, p, "_dL")[-1, , drop = FALSE]
  switch(predictor_type,
         level = XL,
         diff  = XD,
         both  = cbind(XL, XD))
}

# matching predictor row for the next (unobserved) week
nb_build_x_next <- function(Y, p, predictor_type = "both") {
  Z <- log1p(Y); D <- nb_log_diff(Y)
  xL <- nb_next_row(Z, p, "_L")
  xD <- nb_next_row(D, p, "_dL")
  switch(predictor_type,
         level = xL,
         diff  = xD,
         both  = c(xL, xD))
}

#Neg bin
nb_fit_one <- function(X, y, lambda, alpha, pf_names = character(0)) {
  
  ok <- apply(X, 2, function(x) all(is.finite(x)) && sd(x) > 0)
  X  <- X[, ok, drop = FALSE]
  
  dat <- data.frame(y = round(y), X, check.names = FALSE)
  
  pf <- rep(1, ncol(X))
  pf[colnames(X) %in% pf_names] <- 0
  
  fit <- tryCatch(
    suppressWarnings(
      glmregNB(
        y ~ ., data = dat,
        lambda = lambda,
        penalty = "enet",
        alpha = alpha,
        standardize = TRUE,
        parallel = FALSE,
        penalty.factor = pf
      )
    ),
    error = function(e) {
      stop("glmregNB failed: ", e$message)
    }
  )
  
  cf <- coef(fit, which = 1)
  nm <- names(cf)
  cf <- as.numeric(cf)
  
  list(
    b0 = cf[nm == "(Intercept)"],
    beta = setNames(
      cf[nm != "(Intercept)"],
      nm[nm != "(Intercept)"]
    ),
    failed = FALSE
  )
}

nb_predict <- function(f, x) exp(min(f$b0 + sum(f$beta * x[names(f$beta)]), 20))

nb_fit_all <- function(Y, n_lags, lambda, alpha, protected, predictor_type = "both") {
  n <- nrow(Y)
  X <- nb_build_X(Y, n_lags, predictor_type)
  rows <- (n_lags + 2):n                      # one row later than before (differencing)
  setNames(lapply(colnames(Y), function(j)
    nb_fit_one(X, Y[rows, j], lambda, alpha, protected[[j]])), colnames(Y))
}


run_nb_sd <- function(data_mpox, TRAIN_WEEK, list_jur, lambda_value,
                      jurisdiction_interest = "SanDiego", n_lags = 25,
                      alpha = 1, protected_predictors = list(),
                      model_label = "Negative-Binomial-Lasso",
                      refit_step2 = FALSE, max_forecasts = NULL,
                      predictor_type = "both") {
  
  if (!(jurisdiction_interest %in% list_jur)) stop(jurisdiction_interest, " is not in list_jur")
  
  data_model <- data_mpox %>% filter(Jurisdiction %in% list_jur)
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  results_nb <- NB_LASSO_FORECAST_TWO_STEP(
    train_ts, future_ts, future_ts, ROLL_WINDOW = 0, n_lags = n_lags,
    lambda_value = lambda_value, alpha = alpha,
    protected_predictors = protected_predictors,
    refit_step2 = refit_step2, max_forecasts = max_forecasts,
    predictor_type = predictor_type
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

# ---- main forecaster (same signature/outputs as VAR_LASSO_FORECAST_TWO_STEP) ----
NB_LASSO_FORECAST_TWO_STEP <- function(train_data_ts, future_data_ts, future_real_ts,
                                       ROLL_WINDOW, n_lags = 25, lambda_value = 0.1,
                                       alpha = 1, protected_predictors = list(),
                                       refit_step2 = FALSE, max_forecasts = NULL,
                                       predictor_type = "both") {
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  train_data_ts  <- train_data_ts  %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_ts <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  juris_names <- colnames(train_data_ts)
  n_forecasts <- nrow(future_data_ts) - 1
  if (!is.null(max_forecasts)) n_forecasts <- min(n_forecasts, max_forecasts)
  
  results_df <- data.frame(); df_coeffs <- data.frame()
  train_week_t <- train_data_ts
  
  for (week_t in 1:n_forecasts) {
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    Ytr <- as.matrix(train_week_t)
    
    # 1. Fit 11 NB-lasso equations: counts as response, log-differenced lags as predictors
    fits1 <- nb_fit_all(Ytr, n_lags, lambda_value, alpha, protected_predictors, predictor_type)
    
    # 2. Save non-zero coefficients
    df_coeffs_t <- do.call(rbind, lapply(names(fits1), function(r) {
      b <- fits1[[r]]$beta; b <- b[b != 0]
      if (length(b) == 0) return(NULL)
      data.frame(Response = r, Predictor = sub("_d?L[0-9]+$", "", names(b)),
                 Lag = as.integer(sub(".*L", "", names(b))),
                 Type = ifelse(grepl("_dL[0-9]+$", names(b)), "growth", "level"),
                 Coefficient = as.numeric(b), Week_Number = week_number_forecast[week_t])
    }))
    df_coeffs <- bind_rows(df_coeffs, df_coeffs_t)
    
    # 3. Step 1 forecast (t+1) for ALL jurisdictions
    x1  <- nb_build_x_next(Ytr, n_lags, predictor_type)
    mu1 <- vapply(juris_names, function(j) nb_predict(fits1[[j]], x1), numeric(1))
    
    # 4. Step 2 forecast (t+2): append t+1 forecasts, lag 1 is now the predicted week
    Yext  <- rbind(Ytr, mu1)
    x2    <- nb_build_x_next(Yext, n_lags, predictor_type)
    fits2 <- if (refit_step2) nb_fit_all(Yext, n_lags, lambda_value, alpha,
                                         protected_predictors, predictor_type) else fits1
    mu2   <- vapply(juris_names, function(j) nb_predict(fits2[[j]], x2), numeric(1))
    
    # 5. Save long-format results
    actual_values <- as.numeric(future_real_ts[week_t + 1, ])
    results_df <- bind_rows(results_df, data.frame(
      Week_Number     = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(list_date_week_start[week_t + 1], length(juris_names)),
      Jurisdiction    = juris_names,
      Predicted       = pmax(mu2, 0),
      Actual          = actual_values))
    
    # 6. Extend training set
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>% mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  list(results_df = results_df, df_coeffs = df_coeffs,
       preds = pivot_wider(results_df, id_cols = c(Week_Number, date_week_start),
                           names_from = Jurisdiction, values_from = Predicted),
       true  = pivot_wider(results_df, id_cols = c(Week_Number, date_week_start),
                           names_from = Jurisdiction, values_from = Actual))
}