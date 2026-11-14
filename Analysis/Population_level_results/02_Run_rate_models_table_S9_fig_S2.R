#*****************************************************************
# POPULATION-ADJUSTED (INCIDENCE-RATE) SENSITIVITY ANALYSIS
#
# Refits the VAR-Lasso and AR-Lasso to weekly incidence rates (cases per
# 100,000) instead of counts, converts the errors back to the count scale,
# and compares them with the count-based fits (Supplementary Section S6).
#
# Needs (already in the session):
#   data_rate   : data_mpox with Cases converted to rates (Cases_count holds
#                 the original counts)
#   pop_df      : fixed population denominator per jurisdiction
#   list_jur    : SanDiego plus the ten predictor jurisdictions
#   Count-based results: var_sd, nb_glm_out, ar_model, arima_model, naive_model
#   Functions: run_var_sd(), run_AR_model(), PLOT_FORECASTS_SUBSET()
#*****************************************************************

#*****************************************************************
# SETTINGS
#*****************************************************************
TRAIN_WEEK   <- 84          # training weeks (should end at the end of 2023)
lambda       <- 0.05        # same penalty as the main analysis
n_lags_ar       <- 8           # AR-Lasso maximum lag
jurisdiction <- "SanDiego"

POP_SD <- pop_df$Population[pop_df$Jurisdiction == jurisdiction]   # 3,298,634

#*****************************************************************
# FUNCTION: convert rate-scale errors back to counts
#*****************************************************************
RATE_OVERALL_TO_COUNTS <- function(m, pop, per = 1e5) {
  'Convert error metrics fitted on the rate scale (cases per 100,000) back to
  the case-count scale by multiplying by pop / per.
  
  Args:
    m   : data frame of metrics (as returned in $overall).
    pop : population denominator of the jurisdiction.
    per : rate denominator (100,000).
  
  Only the error metrics that scale linearly are converted: rmse, mae, bias
  and their slope-weighted versions. MAPE columns are dropped because they are
  scale-free and are not reported.'
  
  m %>%
    dplyr::select(-dplyr::any_of(c("mape", "slope_weighted_mape"))) %>%
    mutate(across(c(rmse, mae, bias,
                    slope_weighted_rmse, slope_weighted_mae, slope_weighted_bias),
                  ~ .x * pop / per))
}

#*****************************************************************
# 1. RUN THE RATE-BASED MODELS
#*****************************************************************
var_rate <- run_var_sd(data_rate, TRAIN_WEEK, list_jur, lambda)

ar_rate  <- run_AR_model(data_rate, TRAIN_WEEK, jurisdiction, n_lags_ar, lambda)

#*****************************************************************
# 2. PLOT (rate scale)
#*****************************************************************
title_plot <- "Population-Adjusted Rates - San Diego County Two-Week-Ahead Forecasts"

PLOT_FORECASTS_SUBSET_POPULATION(var_rate$forecasts, ar_rate$forecasts, title_plot,
                      jurisdiction_interest = jurisdiction,
                      point_size = 3.8, line_size = 1.45,
                      y_label = "Weekly incidence rate (cases per 100,000 population)")

#*****************************************************************
# 3. COMPARE RATE-BASED AND COUNT-BASED FITS (all errors in cases)
#*****************************************************************
rate_metrics <- bind_rows(RATE_OVERALL_TO_COUNTS(var_rate$overall, POP_SD),
                          RATE_OVERALL_TO_COUNTS(ar_rate$overall,  POP_SD))

count_metrics <- bind_rows(var_lasso_forecasts$overall, ar_lasso_forecasts$overall, nb_ar_lasso_forecasts$overall,
                           arima_forecasts$overall, naive_forecasts$overall) %>%
  dplyr::select(-dplyr::any_of(c("mape", "slope_weighted_mape")))

comparison_rate <- bind_rows(Rates = rate_metrics, Counts = count_metrics,
                             .id = "Fit") %>%
  dplyr::select(Fit, Model, everything()) %>%
  arrange(Model, Fit)

options(width = 200)
print(comparison_rate %>% mutate(across(where(is.numeric), ~ round(.x, 2))))