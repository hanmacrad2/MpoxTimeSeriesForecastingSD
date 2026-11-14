#*****************************************
# NEGATIVE BINOMIAL AUTOREGRESSION WITH LASSO PENALTY: FORECASTING FUNCTIONS
#
# Purpose
#   Rolling two-week-ahead forecasts from a univariate, lasso-penalised
#   negative binomial autoregression for San Diego County (SDC). It is a
#   count-based comparison model for the VAR-Lasso that accommodates
#   over-dispersion (conditional variance can exceed the conditional mean).
#   It uses only SDC's own lagged counts.
#
# Method (Methods Sections 4.6.3, 4.7.1 and 4.7.2)
#   Response    : raw weekly counts, not differenced (a negative binomial
#                 cannot be fitted to differenced counts, which can be negative)
#   Predictors  : log(1 + count) of SDC at lags 1 to n_lags (8 in the paper,
#                 chosen from the autocorrelation of the 2023 training data)
#   Estimation  : mpath::glmregNB with an L1 (lasso) penalty (alpha = 1) at a
#                 single fixed penalty lambda; predictors are standardised
#   At each forecast origin, using an expanding window of data up to that week:
#     1. Fit the model to the observed data.
#     2. Forecast t+1, append it to the series, refit, and forecast t+2.
#     3. Keep only the t+2 forecast, truncated at zero.
#     4. Advance the origin one week and add the newly observed count.
#   No 2024 data are used to choose any setting.
#
# Outputs of run_nb_sd()
#   $overall   : slope-weighted and unweighted error metrics
#   $forecasts : t+2 forecasts and observed counts, one row per origin
#   $coeffs    : non-zero lag coefficients and the dispersion estimate (Theta)
#                from the first fit at each origin (the fit to observed data)
#
# Functions
#   fit_nb(), NB_LAGS(), NB_LAGS_NEXT(), NB_COEFS(), NB_PREDICT(),
#   NB_FIT_SERIES()              : helpers (fitting, lag matrix, prediction)
#   NB_FORECAST_TWO_STEP_SD()    : the rolling two-step forecast loop
#   run_nb_sd()                  : data preparation, forecasting and metrics
#
# Needs
#   Packages: mpath, dplyr
#   Functions defined elsewhere: get_train_test_data(), GET_TS_DATA(),
#   GET_DF_WEIGHTED_SLOPE(), GET_METRICS_JURISDICTION(),
#   GET_SLOPE_METRICS_JURISDICTION()
#*****************************************



#*****************************
# HELPERS

#MAIN NEGATIVE BINOMIAL WITH PENALIZED ELASTIC-NET LASSO (alpha = 1) MODEL 

fit_nb <- function(dat, lambda = 0.05, alpha = 1, m = 10000) {
  'Fit one penalised negative binomial regression with mpath::glmregNB.
  
  Args:
    dat    : data frame with response column y and one column per lagged
             predictor (log(1 + count)).
    lambda : single fixed penalty. The paper uses 0.1.
    alpha  : elastic-net mixing. 1 = lasso (used in the paper), 0 = ridge.
    m      : maximum iterations for the dispersion (theta) updates.
  
  Predictors are standardised and the intercept is not penalised. The
  dispersion parameter theta is estimated alongside the coefficients.
  
  Returns the fitted glmregNB object. Use NB_COEFS to extract coefficients.'
  
  glmregNB(y ~ ., data = dat, lambda = lambda, penalty = "enet",
           alpha = alpha, standardize = TRUE, parallel = FALSE,
           maxit.theta = m)
}


NB_LAGS <- function(y, p) {
  'Build the lag matrix of log(1 + y) used to fit the model.
  
  Args:
    y : numeric vector of weekly counts, oldest first.
    p : number of lags (8 in the paper).
  
  Returns a matrix with length(y) - p rows and p columns named SanDiego_L1 to
  SanDiego_L<p>. Row i holds lags 1 to p for the response in week p + i, so
  column SanDiego_L1 is the previous week. Zero counts are handled by log1p.'
  
  z <- log1p(y)
  n <- length(z)
  
  X <- sapply(1:p, function(l) z[(p + 1 - l):(n - l)])
  colnames(X) <- paste0("SanDiego_L", 1:p)
  X
}


NB_LAGS_NEXT <- function(y, p) {
  'Build the single predictor row used to forecast the next, unobserved week.
  
  Args:
    y : numeric vector of weekly counts (observed, or observed plus an
        appended forecast), oldest first.
    p : number of lags.
  
  Returns a named numeric vector of log(1 + count) at lags 1 to p, where
  SanDiego_L1 is the most recent value in y. The names match the columns
  produced by NB_LAGS so they line up with the fitted coefficients.'
  
  z <- log1p(y)
  n <- length(z)
  
  x <- z[n:(n - p + 1)]
  names(x) <- paste0("SanDiego_L", 1:p)
  x
}


NB_COEFS <- function(fit) {
  'Extract the intercept, lag coefficients and dispersion from a glmregNB fit.
  
  Args:
    fit : fitted object returned by fit_nb.
  
  Returns a list:
    b0    : intercept (numeric).
    beta  : named numeric vector of lag coefficients. Coefficients shrunk to
            zero by the penalty are included as zeros.
    theta : dispersion parameter in the size parametrisation, where
            Var = mu + mu^2 / theta. Larger theta means less over-dispersion.'
  
  cf <- coef(fit, which = 1)
  nm <- names(cf)
  cf <- as.numeric(cf)
  
  list(b0    = cf[nm == "(Intercept)"],
       beta  = setNames(cf[nm != "(Intercept)"], nm[nm != "(Intercept)"]),
       theta = as.numeric(fit$theta)[1])
}

NB_PREDICT <- function(f, x) {
  'Point forecast for one week: the estimated conditional mean.
  
  Args:
    f : coefficient list returned by NB_COEFS (or NB_FIT_SERIES).
    x : named predictor row from NB_LAGS_NEXT.
  
  Returns exp(b0 + sum(beta * x)), a single number on the count scale. It is
  the mean of the fitted negative binomial, not a draw, median or mode, and it
  is not rounded. It is always positive.'
  
  exp(f$b0 + sum(f$beta * x[names(f$beta)]))
}


# NEW: fit one NB equation to a series and return the coefficient list
NB_FIT_SERIES <- function(y, n_lags, lambda, alpha) {
  'Build the lag matrix from a series, fit the penalised model and return the
  coefficients.
  
  Args:
    y      : numeric vector of weekly counts, oldest first.
    n_lags : number of lags.
    lambda : single fixed penalty.
    alpha  : elastic-net mixing (1 = lasso).
  
  Any lag column with zero variance is dropped before fitting. Warnings from
  the fit are suppressed.
  
  Returns the list from NB_COEFS (b0, beta, theta). Used for both the first
  fit at each origin and the refit after the t+1 forecast is appended.'
  
  X    <- NB_LAGS(y, n_lags)
  rows <- (n_lags + 1):length(y)
  dat  <- data.frame(y = y[rows], X[, apply(X, 2, sd) > 0, drop = FALSE],
                     check.names = FALSE)
  
  NB_COEFS(suppressWarnings(fit_nb(dat, lambda, alpha)))
}

#*****************************
# FORECASTER

NB_FORECAST_TWO_STEP_SD <- function(train_ts, future_ts, n_lags = 5,
                                    lambda = 0.05, alpha = 1,
                                    max_forecasts = NULL) {
  'Rolling, expanding-window, two-week-ahead negative binomial lasso forecasts
  for San Diego County.
  
  At each forecast origin: fit to the observed data, forecast t+1, append that
  forecast to the series, refit, forecast t+2 and keep it. The training
  window then grows by one observed week.
  
  Args:
    train_ts      : initial training weeks (from GET_TS_DATA) with a SanDiego
                    column of weekly counts.
    future_ts     : evaluation weeks, same structure, with Week_Number and
                    date_week_start columns.
    n_lags        : number of lags. The default is 5 but the paper uses 8, so
                    pass n_lags = 8.
    lambda        : single fixed penalty. The default is 0.05 but the paper
                    uses 0.1, so pass lambda = 0.1.
    alpha         : elastic-net mixing (1 = lasso).
    max_forecasts : optional cap on the number of origins, for quick tests.
  
  Returns a list:
    results_df : one row per origin with Week_Number and date_week_start of
                 the TARGET week (t+2), Jurisdiction, Predicted (clipped at
                 zero), Actual and error (Actual - Predicted).
    df_coeffs  : non-zero lag coefficients and Theta from the FIRST fit at
                 each origin (observed data only). Week_Number is the ORIGIN
                 week. Origins with no non-zero lag have no rows.
  
  Note: the number of origins is the number of evaluation weeks minus 1,
  which gives the 45 forecasts in the paper.'
  
  print(paste0('N lags: ', n_lags))
  print(paste0('Lambda: ', lambda))
  
  week_number_forecast <- future_ts$Week_Number
  list_date_week_start <- as.Date(future_ts$date_week_start)
  
  y_train  <- train_ts$SanDiego
  y_future <- future_ts$SanDiego
  
  # less one: the first future week is used as the unobserved t+1 step
  n_forecasts <- length(y_future) - 1
  if (!is.null(max_forecasts)) n_forecasts <- min(n_forecasts, max_forecasts)
  
  results_df <- data.frame()
  df_coeffs  <- data.frame()
  
  for (week_t in 1:n_forecasts) {
    
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    
    # data available at this origin
    y_now <- c(y_train, head(y_future, week_t - 1))
    
    # 1. Fit on observed data
    f <- NB_FIT_SERIES(y_now, n_lags, lambda, alpha)
    
    # 2. Save non-zero coefficients (from the fit on observed data)
    b <- f$beta[f$beta != 0]
    if (length(b) > 0) {
      df_coeffs <- bind_rows(df_coeffs, data.frame(
        Predictor   = "SanDiego",
        Lag         = as.integer(sub(".*_L", "", names(b))),
        Coefficient = as.numeric(b),
        Theta       = f$theta,
        Week_Number = week_number_forecast[week_t]))
    }
    
    # 3. Step 1 forecast (t+1)
    mu1 <- NB_PREDICT(f, NB_LAGS_NEXT(y_now, n_lags))
    
    # 4. Step 2 forecast (t+2): append t+1 prediction, REFIT, then predict
    y_ext <- c(y_now, mu1)
    f2    <- NB_FIT_SERIES(y_ext, n_lags, lambda, alpha)
    mu2   <- NB_PREDICT(f2, NB_LAGS_NEXT(y_ext, n_lags))
    
    # 5. Save
    results_df <- bind_rows(results_df, data.frame(
      Week_Number     = week_number_forecast[week_t + 1],
      date_week_start = as.Date(list_date_week_start[week_t + 1]),
      Jurisdiction    = "SanDiego",
      Predicted       = max(mu2, 0),
      Actual          = y_future[week_t + 1]))
  }
  
  results_df <- results_df %>% mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  list(results_df = results_df, df_coeffs = df_coeffs)
}


#*****************************
# RUN

run_nb_sd <- function(data_mpox, TRAIN_WEEK, n_lags = 5, lambda = 0.05,
                      alpha = 1, model_label = "Negative-Binomial-Lasso",
                      max_forecasts = NULL) {
  'End-to-end driver for the negative binomial lasso model, used for the paper
  results.
  
  Filters the data to San Diego County, splits it into training (2023) and
  evaluation (2024) weeks, runs NB_FORECAST_TWO_STEP_SD, and computes
  unweighted and slope-weighted error metrics.
  
  Args:
    data_mpox     : long data frame, one row per jurisdiction-week (needs a
                    Jurisdiction column containing SanDiego).
    TRAIN_WEEK    : index passed to get_train_test_data() that separates the
                    training weeks from the evaluation weeks.
    n_lags        : number of lags. The default is 5 but the paper uses 8.
    lambda        : single fixed penalty. The default is 0.05 but the paper
                    uses 0.1.
    alpha         : elastic-net mixing (1 = lasso).
    model_label   : text placed in the Model column of the output.
    max_forecasts : optional cap on the number of origins, for quick tests.
  
  The call used for the paper is
    run_nb_sd(data_year, 84, n_lags = 8, lambda = 0.1)
  
  Returns a list:
    overall   : one row of metrics (unweighted and slope-weighted) with a
                Model column.
    forecasts : t+2 forecasts and observed counts, one row per origin.
    coeffs    : non-zero lag coefficients and Theta from the first fit at each
                origin.
    result    : full output of NB_FORECAST_TWO_STEP_SD.'
  
  data_model <- data_mpox %>% filter(Jurisdiction == "SanDiego")
  split      <- get_train_test_data(data_model, TRAIN_WEEK)
  train_ts   <- GET_TS_DATA(split$train_data)
  future_ts  <- GET_TS_DATA(split$future_data)
  
  results_nb <- NB_FORECAST_TWO_STEP_SD(train_ts, future_ts, n_lags = n_lags,
                                        lambda = lambda, alpha = alpha,
                                        max_forecasts = max_forecasts)
  
  nb_forecasts <- results_nb$results_df
  
  df_slope    <- GET_DF_WEIGHTED_SLOPE(data_model, "SanDiego")
  jur_metrics <- GET_METRICS_JURISDICTION(nb_forecasts, "SanDiego")
  jur_slope   <- GET_SLOPE_METRICS_JURISDICTION(nb_forecasts, df_slope, "SanDiego")
  
  overall <- left_join(jur_metrics, jur_slope, by = "Jurisdiction") %>%
    mutate(Model = model_label) %>%
    dplyr::select(-Jurisdiction)
  
  list(overall   = overall,
       forecasts = nb_forecasts,
       coeffs    = results_nb$df_coeffs,
       result    = results_nb)
}


#*****************************
# USAGE

# nb_out <- run_nb_sd(data_year, 84, n_lags = 8, lambda = 0.1)
# nb_out$overall
# summary(nb_out$coeffs$Theta)