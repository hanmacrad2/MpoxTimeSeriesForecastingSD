# Early warning of Mpox outbreaks in San Diego County

Code, package and data pointers for:

> Early warning of Mpox outbreaks in San Diego County using Lasso Vector Autoregression models with cross-jurisdictional lags. *In revision at Epidemics.*

The study forecasts weekly Mpox cases in San Diego County (SDC) two weeks ahead using a vector autoregression with lasso penalty (VAR-Lasso) fitted to SDC and ten correlated U.S. jurisdictions. It compares the VAR-Lasso with four univariate models (AR-Lasso, negative binomial AR-Lasso, ARIMA, naive persistence), using slope-weighted error metrics that prioritize accuracy during periods of rising incidence. It was developed with the County of San Diego Health and Human Services Agency (CSDHHSA) through Resilient Shield, part of the CDC-funded Insight Net network.

## What is in this repository

**`MpoxTimeSeriesForecastingSD`: an R package we built for this study.** It contains everything needed to fit the five models, produce rolling two-week-ahead forecasts, and compute the error metrics and figures. You can use it on its own, without the analysis scripts. See the package README in the `MpoxTimeSeriesForecastingSD/` subfolder.

**Analysis scripts** that call the package to produce each table and figure in the paper. They are the record of exactly how every reported number was made.

```
MpoxTimeSeriesForecastingSD/            <- repository root (this folder)
├── README.md                           <- this file
├── libs_required.R                     <- installs and loads the required packages and our package
├── 01_get_data_and_plot.R              <- Load data, filter and plot
├── 2_Run_forecast_models.R             <- runs the five models and compares them
├── Analysis/                           <- one script per table or figure
│   ├── 01_Data_summary_statistics_tab1.R
│   ├── 02_Cross_correlation_plot_fig_2.R
│   ├── 03_Performance_improvement_bar_plot_fig4.R
│   ├── 04_Predictor_selection_cross_corr_screening_section_4_4.R
│   ├── 05_Penalty_selection_lag_retention_table5.R
│   ├── 06_var_lasso_top_coefficients_fig6a_table_3.R
│   ├── 07_Autocorrelation_San_Diego_County_fig_S1.R
│   ├── 08_Coefficient_stability_univariate_AR_models_table_S3_S4.R
│   ├── 09_sensitivity_analysis_neg_bin_model_table_S6.R
│   ├── 10_sensitivity_arima_model_table_S5.R
│   ├── 11_Sensitivity_analysis_penalised_models_table_S6.R
│   └── Population_adjusted_rates/      <- Supplementary Section S6 (run in order)
│       ├── 01_Population_denominators_and_rates_table_S8.R
│       └── 02_Run_rate_models_table_S9_fig_S2.R
├── data/
│   └── ncbi_accessions.txt             <- NCBI accessions of the MPXV genomes
└── MpoxTimeSeriesForecastingSD/        <- the R package
    ├── DESCRIPTION, NAMESPACE
    └── R/                              <- model, metric and plotting code
```

## How to reproduce the paper

1. **Clone the repository.**
2. **Install our package.** The scripts load it with `library(MpoxTimeSeriesForecastingSD)`, so it must be installed first.
   ```r
   install.packages("devtools")
   devtools::install("MpoxTimeSeriesForecastingSD")    # run from the repository root
   ```
   Or from the command line, in the repository root: `R CMD INSTALL MpoxTimeSeriesForecastingSD`.
3. **Load the packages.** `source("libs_required.R")` installs any missing dependencies, loads them, and loads our package last.
4. **Get the data.** Run `1_get_data_local.R` after setting `FOLDER` and `DATA_FOLDER` to your local paths. See **Data** below. 
5. **Run the models.** `2_Run_forecast_models.R` fits the VAR-Lasso and the four comparison models over the 45 rolling forecasts and produces the comparison table (Table 2) and forecast figure (Figure 5).
6. **Reproduce a specific table or figure.** Run the matching script in `Analysis/` (table below). Run step 5 first, because the scripts read its results.

## Using the package in your own work

The package works on any weekly case data in the format described under **Data dictionary**. A minimal example:

```r
library(MpoxTimeSeriesForecastingSD)
library(dplyr)

# data_mpox: weekly counts, one row per jurisdiction-week (see Data dictionary).
# It is created by 1_get_data_local.R; the package does not include the data.
data_year  <- data_mpox %>% filter(Year >= 2023)
list_jur   <- c("NewYorkCity", "LA", "Georgia", "Illinois", "NewJersey", "SanDiego",
                "Philadelphia", "Colorado", "Massachusetts", "Ohio", "NewYork")
TRAIN_WEEK <- 84
lambda     <- 0.1

print_split_check(data_year, TRAIN_WEEK, list_jur)   # confirms the train and test windows

var_out   <- run_var_sd(data_year, TRAIN_WEEK, list_jur, lambda)
nb_out    <- run_nb_sd(data_year, TRAIN_WEEK, n_lags = 8, lambda = lambda)
ar_out    <- run_AR_model(data_year, TRAIN_WEEK = TRAIN_WEEK, jurs_selection = "SanDiego",
                          n_lags_ar = 8, lambda = lambda)
arima_out <- run_ARIMA_model(data_year, TRAIN_WEEK = TRAIN_WEEK, jurs_selection = "SanDiego",
                             max_p = 8)
naive_out <- run_naive_model(data_year, TRAIN_WEEK, "SanDiego", window = 1)

comparison <- bind_rows(var_out$overall, nb_out$overall, ar_out$overall,
                        arima_out$overall, naive_out$overall)
```

Always pass `lambda = 0.1` and the lag arguments explicitly. Some function defaults (`lambda = 0.05`, `n_lags = 5`) are not the settings used in the paper.

## Where each result comes from

| Script in `Analysis/` | Output in the paper |
|---|---|
| `01_Data_summary_statistics_tab1.R` | Table 1 |
| `02_Cross_correlation_plot_fig_2.R` | Figure 2 |
| `03_Performance_improvement_bar_plot_fig4.R` | Figure 4 |
| `04_Predictor_selection_cross_corr_screening_section_4_4.R` | Predictor screening (Section 4.4, Table 4) |
| `05_Penalty_selection_lag_retention_table5.R` | Table 5 |
| `06_var_lasso_top_coefficients_fig6a_table_3.R` | Figure 6a, Table 3 |
| `07_Autocorrelation_San_Diego_County_fig_S1.R` | Figure S1 |
| `08_Coefficient_stability_univariate_AR_models_table_S3_S4.R` | Tables S3, S4 |
| `09_sensitivity_analysis_neg_bin_model_table_S6.R` | Table S6 (negative binomial columns) |
| `10_sensitivity_arima_model_table_S5.R` | Table S5 (ARIMA orders) |
| `11_Sensitivity_analysis_penalised_models_table_S6.R` | Table S6 (VAR-Lasso and AR-Lasso columns) |
| `Population_adjusted_rates/01_Population_denominators_and_rates_table_S8.R` | Table S8 (population denominators). Builds the rate data used by the next script. |
| `Population_adjusted_rates/02_Run_rate_models_table_S9_fig_S2.R` | Table S9, Figure S2 (VAR-Lasso and AR-Lasso fitted to incidence rates). Run after `01_` in the same folder. |
| `2_Run_forecast_models.R` (repository root) | Table 2, Figure 5, Table S7 (unweighted metrics) |

## Data

**Source.** Weekly Mpox case counts by U.S. jurisdiction from the CDC National Surveillance System, 1 January 2023 to 24 November 2024 (the most recent data available). Data are used as released, at the geographic level at which each jurisdiction reports (state, county or city), with no aggregation or adjustment. New York State is analyzed excluding New York City.

**Availability.** 

**Genomic data (phylogeographic analysis).** The phylogeographic analysis (TreeTime, BEAST X) was run separately and is not part of this repository. The MPXV genomes are available through GISAID (EPI_SET_260729rw, https://doi.org/10.55876/gis8.260729rw), and the NCBI accessions are listed in `data/ncbi_accessions.txt`. Genomes from New York City were shared under a Data Use Agreement and have since been submitted to NCBI.

## Data dictionary

Understanding these definitions is essential for interpreting the lag structure (for example, Illinois at lags of 23 and 24 weeks).

**Case definition.** Cases are persons who tested positive for monkeypox virus or orthopoxvirus, as in the [CDC case definition](https://www.cdc.gov/monkeypox/php/case-definitions/index.html). Counts include all Mpox cases reported to CDC (clade I and clade II).

**Date definition.** Each case is assigned the earliest date available, which is one of: date of illness onset, diagnosis date, positive laboratory test report date, or date reported to CDC. This is an event date when onset or diagnosis is available and otherwise a reporting date. Weekly counts, and therefore lags, are measured on this date.

**Week definition.** [epidemiological (MMWR) week or ISO week, and the weekday on which `date_week_start` falls.] One lag is one week, so a lag of 24 weeks is about 5.5 months.

**Variables.**

| Variable | Description |
|---|---|
| `Jurisdiction` | Reporting jurisdiction. The model uses 11: NewYorkCity, LA, Georgia, Illinois, NewJersey, SanDiego, Philadelphia, Colorado, Massachusetts, Ohio, NewYork. |
| `Year` | Calendar year of the week. The models use `Year >= 2023`. |
| `date_week_start` | First day of the week for the case date defined above. |
| `Week_Number` | Running week index within the full series. The train and test split is made on this column. |
| `Cases` | Number of Mpox cases in that jurisdiction and week (raw, not smoothed). |

**Notes.** Counts for 2024 onward are preliminary and subject to change. Data for the most recent month may be incomplete because of reporting delays.

## Reproducibility

| Item | Setting |
|---|---|
| R version | R version 4.6.1 (2026-06-24 ucrt) |
| Operating system | Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200) |
| Package versions | See `sessionInfo.txt` in this repository [CONFIRM: generate it, see below]. Our own package is version 0.1.0. |
| Random seed | No random number generation is used in the forecasting. All models are fitted at a fixed penalty, so results are deterministic. |
| Training period | January to December 2023 (`TRAIN_WEEK = 84`, confirmed with `print_split_check()`) |
| Evaluation | 45 rolling two-week-ahead forecasts, January to November 2024 (weeks 86 to 130) |
| VAR-Lasso | `lambda = 0.1`, maximum 25 weekly lags, SDC plus ten predictor jurisdictions, first-differenced counts |
| AR-Lasso | `lambda = 0.1`, `n_lags_ar = 8`, first-differenced counts |
| Negative binomial AR-Lasso | `lambda = 0.1`, `n_lags = 8`, raw counts with log(1 + count) predictors |
| ARIMA | `auto.arima`, non-seasonal, stepwise, AICc, `max_p = 8` |
| Naive persistence | Last observed weekly count carried forward (`window = 1`) |
| Edge handling | No smoothing is applied, so no weeks are dropped at the start or end of the series. First differencing uses up the first week of each training window, and the lag structure uses the first p weeks of each window as predictors. |
| Forecast design | Expanding window. At each origin the model is fitted to all data up to that week, the one-week-ahead forecast is appended, the model is refitted, and the two-week-ahead forecast is kept (negative forecasts set to zero for the differenced models and ARIMA). |

To record package versions, run once after a full run:

```r
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
```

## Limitations

- The VAR-Lasso 25-week lag window was informed by phylogeographic evidence specific to SDC. Applying the approach to other jurisdictions would need comparable evidence.
- One test year and one forecast horizon (two weeks ahead).
- The penalty is on each package's own scale, so values are not comparable across models.

## Citation and contact

If you use this code or package, please cite the paper above. 

## References

1. Centers for Disease Control and Prevention. *U.S. Case Data: Monkeypox.* https://www.cdc.gov/monkeypox/data-research/cases/index.html. 