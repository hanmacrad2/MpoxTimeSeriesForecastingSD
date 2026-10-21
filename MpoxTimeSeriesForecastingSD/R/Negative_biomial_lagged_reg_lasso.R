#*******************************************
#* NEGATIVE BINOMIAL LASSO MODEL
#*
#* Count-based comparison model for the VAR-Lasso.
#*
#*   Response   : raw weekly case counts (undifferenced - the negative
#*                binomial likelihood is defined only on the non-negative
#*                integers, so the response cannot be differenced as in
#*                the VAR-Lasso).
#*   Predictors : log(1 + cases) at lags 1..p for ALL jurisdictions.
#*                With 11 jurisdictions and p = 25 this gives 275 candidates.
#*   Penalty    : L1 (lasso) via mpath::glmregNB, at a single fixed lambda.
#*   Forecast   : recursive two-step-ahead, matching the VAR-Lasso pipeline.
#*
#* One equation is fitted per jurisdiction. All eleven are needed because the
#* two-step forecast requires a one-step prediction for every series (each
#* appears as a lagged predictor of the others), but only the jurisdiction of
#* interest is evaluated.
#*******************************************

library(mpath); library(dplyr); library(tidyr)


#-------------------------------------------------------------------
# DESIGN MATRIX HELPERS
#-------------------------------------------------------------------

#' Build a lag-major design matrix.
#'
#' Columns are ordered lag-by-lag: all jurisdictions at lag 1, then all at
#' lag 2, and so on, named like "Illinois_L25". Rows correspond to response
#' weeks (p + 1):n, since earlier weeks have no complete set of lags.
nb_design <- function(Z, p) {
  n <- nrow(Z)
  
  do.call(cbind, lapply(1:p, function(l) {
    # rows (p + 1 - l):(n - l) line lag l up against response weeks (p+1):n
    lag_block <- Z[(p + 1 - l):(n - l), , drop = FALSE]
    colnames(lag_block) <- paste0(colnames(Z), "_L", l)
    lag_block
  }))
}


#' Predictor row for the next (as yet unobserved) week.
#'
#' Takes the most recent p rows of Z and lays them out in the same column
#' order as nb_design(), so the two can be used with the same coefficients.
nb_next_row <- function(Z, p) {
  n <- nrow(Z)
  
  x <- unlist(lapply(1:p, function(l) Z[n + 1 - l, ]))
  names(x) <- unlist(lapply(1:p, function(l) paste0(colnames(Z), "_L", l)))
  
  x
}


#' Training design matrix: log(1 + y) at lags 1..p.
#'
#' Note the dropped first row. It costs one observation but keeps the usable
#' training window identical to earlier versions of this model, so results
#' remain comparable across runs.
nb_build_X <- function(Y, p) {
  Z <- log1p(Y)
  nb_design(Z, p)[-1, , drop = FALSE]
}


#' Matching predictor row for the next week.
nb_build_x_next <- function(Y, p) {
  Z <- log1p(Y)
  nb_next_row(Z, p)
}


#-------------------------------------------------------------------
# MODEL FITTING
#-------------------------------------------------------------------

#' Fit a single penalised negative binomial equation.
#'
#' @param X        design matrix (training rows x candidate predictors)
#' @param y        response counts for one jurisdiction
#' @param lambda   single penalty value (no path, no cross-validation)
#' @param alpha    1 = lasso, 0 = ridge
#' @param pf_names predictors to exempt from the penalty (empty by default,
#'                 so nothing is forced into the model)
nb_fit_one <- function(X, y, lambda, alpha, pf_names = character(0)) {
  
  # Drop any predictor that is constant or non-finite within this training
  # window; a zero-variance column cannot be standardised.
  ok <- apply(X, 2, function(x) all(is.finite(x)) && sd(x) > 0)
  X  <- X[, ok, drop = FALSE]
  
  dat <- data.frame(y = round(y), X, check.names = FALSE)
  
  # Penalty factor of 0 exempts a predictor from shrinkage; 1 is the default.
  pf <- rep(1, ncol(X))
  pf[colnames(X) %in% pf_names] <- 0
  
  fit <- tryCatch(
    suppressWarnings(
      glmregNB(
        y ~ ., data = dat,
        lambda         = lambda,
        penalty        = "enet",
        alpha          = alpha,
        standardize    = TRUE,
        parallel       = FALSE,   # avoids mpath's parallel backend warnings
        penalty.factor = pf
      )
    ),
    error = function(e) {
      stop("glmregNB failed: ", e$message)
    }
  )
  
  # which = 1 selects the single fitted lambda.
  cf <- coef(fit, which = 1)
  nm <- names(cf)
  cf <- as.numeric(cf)
  
  # Dispersion (theta), estimated jointly with the coefficients. glmregNB
  # returns one value per lambda, so with a single lambda this is length 1.
  # Returns NA rather than failing if the slot is absent in this mpath version.
  theta <- tryCatch(as.numeric(fit$theta)[1], error = function(e) NA_real_)
  
  list(
    b0     = cf[nm == "(Intercept)"],
    beta   = setNames(cf[nm != "(Intercept)"], nm[nm != "(Intercept)"]),
    theta  = theta,
    failed = FALSE
  )
}


#' Predicted count from a fitted equation: exp(b0 + x'beta).
#'
#' The linear predictor is capped at 20 before exponentiation purely to guard
#' against numerical overflow; e^20 is far above any plausible weekly count,
#' so the cap should never bind on a well-behaved fit.
nb_predict <- function(f, x) {
  exp(min(f$b0 + sum(f$beta * x[names(f$beta)]), 20))
}


#' Fit one equation per jurisdiction on a common design matrix.
nb_fit_all <- function(Y, n_lags, lambda, alpha, protected) {
  n <- nrow(Y)
  X <- nb_build_X(Y, n_lags)
  
  # Response rows start at n_lags + 2: n_lags weeks are consumed by the lag
  # structure, plus the single row dropped in nb_build_X().
  rows <- (n_lags + 2):n
  
  setNames(
    lapply(colnames(Y), function(j) {
      nb_fit_one(X, Y[rows, j], lambda, alpha, protected[[j]])
    }),
    colnames(Y)
  )
}


#-------------------------------------------------------------------
# MAIN FORECASTER
# Same signature and outputs as VAR_LASSO_FORECAST_TWO_STEP.
#-------------------------------------------------------------------

NB_LASSO_FORECAST_TWO_STEP <- function(train_data_ts, future_data_ts, future_real_ts,
                                       ROLL_WINDOW, n_lags = 25, lambda_value = 0.1,
                                       alpha = 1, protected_predictors = list(),
                                       refit_step2 = FALSE, max_forecasts = NULL) {
  
  # Keep the week labels before stripping them from the data matrices.
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  
  train_data_ts  <- train_data_ts  %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_ts <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  juris_names <- colnames(train_data_ts)
  
  # One forecast per future week, less one: the first future week is consumed
  # as the unobserved t+1 step of the first two-step forecast.
  n_forecasts <- nrow(future_data_ts) - 1
  if (!is.null(max_forecasts)) n_forecasts <- min(n_forecasts, max_forecasts)
  
  results_df   <- data.frame()
  df_coeffs    <- data.frame()
  df_theta     <- data.frame()
  train_week_t <- train_data_ts
  
  for (week_t in 1:n_forecasts) {
    
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    Ytr <- as.matrix(train_week_t)
    
    #-- 1. Fit one equation per jurisdiction on the data available now -------
    fits1 <- nb_fit_all(Ytr, n_lags, lambda_value, alpha, protected_predictors)
    
    #-- 2. Record the non-zero coefficients for this origin ------------------
    df_coeffs_t <- do.call(rbind, lapply(names(fits1), function(r) {
      
      b <- fits1[[r]]$beta
      b <- b[b != 0]
      if (length(b) == 0) return(NULL)
      
      data.frame(
        Response    = r,
        Predictor   = sub("_L[0-9]+$", "", names(b)),   # jurisdiction name
        Lag         = as.integer(sub(".*_L", "", names(b))),
        Coefficient = as.numeric(b),
        Week_Number = week_number_forecast[week_t]
      )
    }))
    
    df_coeffs <- bind_rows(df_coeffs, df_coeffs_t)
    
    #-- 2b. Record the fitted dispersion for each equation -------------------
    # Theta is poorly identified when the training window is short, so it is
    # worth monitoring across refits rather than assuming it is stable.
    df_theta_t <- data.frame(
      Response    = names(fits1),
      Theta       = vapply(fits1, function(f) f$theta, numeric(1)),
      Week_Number = week_number_forecast[week_t],
      row.names   = NULL
    )
    
    df_theta <- bind_rows(df_theta, df_theta_t)
    
    #-- 3. Step 1: one-week-ahead forecast for every jurisdiction ------------
    # Needed for all eleven series, since each one feeds the others' lags.
    x1  <- nb_build_x_next(Ytr, n_lags)
    mu1 <- vapply(juris_names, function(j) nb_predict(fits1[[j]], x1), numeric(1))
    
    #-- 4. Step 2: append the t+1 forecasts, then forecast t+2 ---------------
    # After appending, lag 1 is the forecast week rather than an observed one.
    Yext <- rbind(Ytr, mu1)
    x2   <- nb_build_x_next(Yext, n_lags)
    
    # refit_step2 = TRUE refits on the augmented series, matching the
    # VAR-Lasso; FALSE reuses the step-1 coefficients and is roughly twice
    # as fast.
    fits2 <- if (refit_step2) {
      nb_fit_all(Yext, n_lags, lambda_value, alpha, protected_predictors)
    } else {
      fits1
    }
    
    mu2 <- vapply(juris_names, function(j) nb_predict(fits2[[j]], x2), numeric(1))
    
    #-- 5. Store the t+2 forecast against its actual -------------------------
    actual_values <- as.numeric(future_real_ts[week_t + 1, ])
    
    results_df <- bind_rows(results_df, data.frame(
      Week_Number     = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(list_date_week_start[week_t + 1], length(juris_names)),
      Jurisdiction    = juris_names,
      Predicted       = pmax(mu2, 0),   # counts cannot be negative
      Actual          = actual_values
    ))
    
    #-- 6. Roll the window forward by one observed week ----------------------
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>% mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  list(
    results_df = results_df,
    df_coeffs  = df_coeffs,
    df_theta   = df_theta,
    preds      = pivot_wider(results_df, id_cols = c(Week_Number, date_week_start),
                             names_from = Jurisdiction, values_from = Predicted),
    true       = pivot_wider(results_df, id_cols = c(Week_Number, date_week_start),
                             names_from = Jurisdiction, values_from = Actual)
  )
}


#-------------------------------------------------------------------
# WRAPPER
# Splits the data, runs the forecaster, and returns metrics for the
# jurisdiction of interest alongside the full output.
#-------------------------------------------------------------------

run_nb_sd <- function(data_mpox, TRAIN_WEEK, list_jur, lambda_value,
                      jurisdiction_interest = "SanDiego", n_lags = 25,
                      alpha = 1, protected_predictors = list(),
                      model_label = "Negative-Binomial-Lasso",
                      refit_step2 = TRUE, max_forecasts = NULL) {
  
  if (!(jurisdiction_interest %in% list_jur)) {
    stop(jurisdiction_interest, " is not in list_jur")
  }
  
  #-- Prepare the train / future split ------------------------------------
  data_model <- data_mpox %>% filter(Jurisdiction %in% list_jur)
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  
  train_ts  <- GET_TS_DATA(split$train_data)
  future_ts <- GET_TS_DATA(split$future_data)
  
  #-- Run the rolling two-step forecast -----------------------------------
  results_nb <- NB_LASSO_FORECAST_TWO_STEP(
    train_ts, future_ts, future_ts,
    ROLL_WINDOW          = 0,
    n_lags               = n_lags,
    lambda_value         = lambda_value,
    alpha                = alpha,
    protected_predictors = protected_predictors,
    refit_step2          = refit_step2,
    max_forecasts        = max_forecasts
  )
  
  nb_forecasts <- results_nb$results_df
  
  #-- Metrics, per jurisdiction -------------------------------------------
  df_slope    <- GET_DF_WEIGHTED_SLOPE(data_model, list_jur)
  jur_metrics <- GET_METRICS_JURISDICTION(nb_forecasts, list_jur)
  jur_slope   <- GET_SLOPE_METRICS_JURISDICTION(nb_forecasts, df_slope, list_jur)
  
  jur_level_metrics <- left_join(jur_metrics, jur_slope, by = "Jurisdiction") %>%
    mutate(Model = model_label) %>%
    dplyr::select(Jurisdiction, Model, everything())
  
  #-- Narrow to the jurisdiction of interest ------------------------------
  overall <- jur_level_metrics %>%
    filter(Jurisdiction == jurisdiction_interest) %>%
    dplyr::select(-Jurisdiction)
  
  coeffs <- results_nb$df_coeffs
  if (nrow(coeffs) > 0) {
    coeffs <- coeffs %>% filter(Response == jurisdiction_interest)
  }
  
  theta <- results_nb$df_theta
  if (nrow(theta) > 0) {
    theta <- theta %>% filter(Response == jurisdiction_interest)
  }
  
  list(
    overall   = overall,
    forecasts = nb_forecasts %>% filter(Jurisdiction == jurisdiction_interest),
    coeffs    = coeffs,
    theta     = theta,
    result    = results_nb
  )
}