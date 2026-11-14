# MpoxTimeSeriesForecastingSD

An R package for short-term forecasting of weekly Mpox cases in San Diego County (SDC), California. It was written for the study below, and bundles the models, error metrics and plotting functions in one installable package, so the analysis can be reused on other weekly case series.

> Early warning of Mpox outbreaks in San Diego County using Lasso Vector Autoregression models with cross-jurisdictional lags. *In revision at Epidemics.*

The package fits a sparse vector autoregression with lasso penalty (VAR-Lasso) that uses lagged case counts from other jurisdictions, fits four comparison models, and evaluates them with unweighted and slope-weighted error metrics. It was developed with the County of San Diego Health and Human Services Agency (CSDHHSA) through Resilient Shield, part of the CDC-funded Insight Net network.

The scripts that reproduce each table and figure in the paper, the data dictionary, and the reproducibility details are in the README of the repository root.

## What the package provides

- **Five forecasting models** with a common interface: VAR-Lasso, AR-Lasso, negative binomial AR-Lasso, ARIMA and naive persistence.
- **Rolling two-week-ahead forecasts** with an expanding window and no use of the test period for tuning.
- **Error metrics**, including slope-weighted RMSE, MAE and bias, which weight errors by the size of week-to-week increases.
- **Plotting functions** for the weekly cases and the forecast figures.

## Installation

The package sits in a subfolder of the repository, so give `subdir`:

```r
# install.packages("remotes")
remotes::install_github("hanmacrad2/MpoxTimeSeriesForecastingSD",
                        subdir = "MpoxTimeSeriesForecastingSD")
```

From a local clone, run from the repository root:

```r
devtools::install("MpoxTimeSeriesForecastingSD")
```

Or from the command line, in the repository root:

```
R CMD INSTALL MpoxTimeSeriesForecastingSD
```

Then load it with `library(MpoxTimeSeriesForecastingSD)`. Requires R >= [version]. The package attaches its dependencies (`dplyr`, `tidyr`, `tibble`, `ggplot2`, `ggh4x`, `scales`, `reshape2`, `BigVAR`, `mpath`, `forecast`), which are installed with it. 

## Input data

The package does not include the case data. Supply a data frame with one row per jurisdiction and week:

| Column | Description |
|---|---|
| `Jurisdiction` | Jurisdiction code (for example `SanDiego`, `Illinois`, `LA`) |
| `Year` | Calendar year |
| `Week_Number` | Week index. The train and test split is made on this column. |
| `date_week_start` | Start date of the reporting week |
| `Cases` | Reported weekly case count (raw, not smoothed) |

Definitions of the case, date and week are in the Data Dictionary in the repository README. 

## Quick start

```r
library(MpoxTimeSeriesForecastingSD)
library(dplyr)

data_year <- data_mpox %>% filter(Year >= 2023)    # data_mpox: your data in the format above

list_jur <- c("NewYorkCity", "LA", "Georgia", "Illinois", "NewJersey",
              "SanDiego", "Philadelphia", "Colorado", "Massachusetts",
              "Ohio", "NewYork")

TRAIN_WEEK <- 84     # last training week (end of 2023); later weeks are the 2024 test period
lambda     <- 0.1    # lasso penalty, fixed in advance

print_split_check(data_year, TRAIN_WEEK, list_jur)   # prints the train and test windows

var_out   <- run_var_sd(data_year, TRAIN_WEEK, list_jur, lambda)
nb_out    <- run_nb_sd(data_year, TRAIN_WEEK, n_lags = 8, lambda = lambda)
ar_out    <- run_AR_model(data_year, TRAIN_WEEK = TRAIN_WEEK, jurs_selection = "SanDiego",
                          n_lags_ar = 8, lambda = lambda)
arima_out <- run_ARIMA_model(data_year, TRAIN_WEEK = TRAIN_WEEK, jurs_selection = "SanDiego",
                             max_p = 8)
naive_out <- run_naive_model(data_year, TRAIN_WEEK, "SanDiego", window = 1)

# Accuracy comparison (slope-weighted and unweighted RMSE, MAE, bias)
comparison <- dplyr::bind_rows(var_out$overall, nb_out$overall, ar_out$overall,
                               arima_out$overall, naive_out$overall)

# Forecast figure
Plot_final_models(var_out, nb_out, ar_out, arima_out, naive_out,
                  "San Diego County Forecasts, 2024")
```

Pass `lambda` and the lag arguments explicitly. Some function defaults (`lambda = 0.05`, `n_lags = 5`) are not the settings used in the paper.

## Models

All models forecast two weeks ahead with an expanding window. At each forecast origin, the model is fitted to all data up to that week, the one-week-ahead forecast is appended to the series, the model is refitted, and the two-week-ahead forecast is kept.

| Model | Uses | Function |
|---|---|---|
| VAR-Lasso | SDC plus ten predictor jurisdictions, up to 25 weekly lags, first-differenced counts | `run_var_sd()` |
| AR-Lasso | SDC's own lags only, up to 8, first-differenced counts | `run_AR_model()` |
| Negative binomial AR-Lasso | SDC's own lags only, up to 8, raw counts with log(1 + count) predictors | `run_nb_sd()` |
| ARIMA | SDC only, order chosen by `auto.arima` (maximum AR order 8) | `run_ARIMA_model()` |
| Naive persistence | Last observed weekly count carried forward | `run_naive_model()` |

Each `run_*` function returns the forecasts (`$forecasts`), the error metrics (`$overall`), and, for the penalized models, the non-zero coefficients at each forecast origin.

## Function reference

Every function has a docstring at the top of its body that describes its arguments and return value. Most users need only the `run_*` functions and the plotting functions.

| File in `R/` | Main functions |
|---|---|
| `data_processing.R` | `get_train_test_data()`, `GET_TS_DATA()`: reshaping and the train and test split |
| `VAR_lasso_model.R` | `run_var_sd()`, `pct_improve_vs_var()`: VAR-Lasso (`BigVAR`) |
| `AR_Lasso_model.R` | `run_AR_model()`: AR-Lasso (`BigVAR`) |
| `Negative_binomial_AR_lasso_model.R` | `run_nb_sd()`: negative binomial lasso autoregression (`mpath`) |
| `Arima_model.R` | `run_ARIMA_model()`: ARIMA (`forecast`) |
| `Naive_persistence_model.R` | `run_naive_model()`: persistence benchmark |
| `performance_metrics.R` | RMSE, MAE, bias and slope-weighted versions |
| `plotting_functions.R` | `PLOT_WEEKLY_CASES()`, `Plot_final_models()`, `PLOT_FORECASTS_SUBSET()`, `print_split_check()` |

## Design choices

- **Training period:** January to December 2023. The 2022 outbreak is excluded because of its unprecedented scale. Out-of-sample performance is assessed on 45 rolling forecasts from January to November 2024.
- **No test-period tuning:** the penalty and the lag windows and orders are fixed from the 2023 data and do not depend on the 2024 forecasts. The VAR-Lasso 25-week window was informed by phylogeographic evidence of introductions from Illinois and by the cross-correlation analysis. The univariate lag order of 8 was set from the autocorrelation of the 2023 SDC data.
- **Raw counts:** no smoothing is applied to the weekly counts.
- **Slope-weighted metrics:** errors are weighted by the size of week-to-week increases in observed cases, so weeks of rising incidence count for more.
- **Penalty:** lambda is on each package's own scale, so values are not comparable across models.

## What the package does not include

- The case data. Supply your own in the format above.
- The phylogeographic analysis (TreeTime, BEAST X), which was run separately.
- The scripts that reproduce the paper's tables and figures, which are in the repository root.

## Limitations

- The VAR-Lasso lag window reflects phylogeographic evidence specific to SDC. Applying the approach to other jurisdictions would need comparable evidence to set the lag window.
- One test year and one forecast horizon (two weeks ahead).

## Citation

If you use this package, please cite the paper above and, once released, the package