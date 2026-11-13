# Mpox Time Series Forecasting (VAR-Lasso & uni-variate time-series models), San Diego County

Forecasting weekly Mpox case counts for San Diego County (SDC) using a Vector Autoregression model with Lasso regularization (VAR-Lasso) and cross-jurisdictional lagged case counts from CDC national surveillance data.

## Abstract

Mpox is an orthopox virus of humans and animals that spreads through intimate contact. The episodic and spatially heterogeneous dynamics of Mpox transmission highlight the need for timely, area-specific forecasts to guide locally targeted public health responses in the U.S. We develop a vector autoregression model with Lasso regularization (VAR-Lasso) to generate rolling two-week-ahead forecasts of weekly Mpox cases for San Diego County (SDC) in collaboration with the County of San Diego Health and Human Services Agency, using national surveillance data from the Centers for Disease Control and Prevention (CDC). Phylogenetic analysis traced the largest SDC cluster to Mpox virus circulating in Illinois approximately six months earlier, and we therefore specified the VAR-Lasso model to allow up to 25 weeks (approximately 6 months) of lagged multivariate weekly case counts from SDC and ten correlated U.S. jurisdictions. The model is fitted to 2023 data and evaluated out of sample on weekly forecasts over 11 months in 2024. The VAR-Lasso assigned its largest coefficients to Illinois at lags of 23 and 24 weeks, matching the approximately 6-month interval inferred from the phylogenetic analysis. As the need for public health action is often greatest when incidence is increasing, our performance evaluation focuses on positive-slope weighted error metrics: slope-weighted root mean squared error (RMSE), slope-weighted mean absolute error (MAE), and slope-weighted bias. Forecast performance of the VAR-Lasso model is compared to a univariate Auto-Regressive (AR) Lasso model, a negative binomial lasso regression with multi-jurisdiction lagged predictors, an ARIMA model, and a naïve persistence model. Across all forecasted weeks in 2024, the VAR-Lasso reduces slope-weighted RMSE by 27% to 34%, slope-weighted MAE by 20% to 35%, and slope-weighted bias by 40% to 51%. These findings suggest that multivariate models using cross-jurisdictional case data can strengthen early forecasting of Mpox increases in San Diego County.

---

## Data Dictionary

Understanding these definitions is essential for interpreting the lag structure (e.g., "Illinois at lags of 23 and 24 weeks").

### Data source

Weekly Mpox case counts by U.S. jurisdiction, from the Centers for Disease Control and Prevention (CDC) national Mpox surveillance data. San Diego County data were obtained in collaboration with the County of San Diego Health and Human Services Agency. 

### Case definition

* Case counts include persons who tested positive for either monkeypox virus or orthopoxvirus, as described in the [CDC case definition](https://www.cdc.gov/monkeypox/php/case-definitions/index.html).
* Counts include all Mpox cases (clade I and clade II) reported to CDC.

### Date definition

The date assigned to each case is the **earliest date available** for that case, which is one of:

1. date of illness onset,
2. diagnosis date,
3. positive laboratory test report date, or
4. date reported to CDC.

This is an *event date* when onset or diagnosis is available, and otherwise a *reporting date*. Weekly counts, and therefore lags, are measured on this date.

### Data notes

* Case counts for 2024 onward are preliminary and subject to change.
* Data for the most recent month may be incomplete due to reporting delays and should be interpreted with caution.

### Variables (`data_mpox.xlsx`, sheet 1)

| Variable | Description |
|---|---|
| `Jurisdiction` | Reporting jurisdiction. Model uses 11: NewYorkCity, LA, Georgia, Illinois, NewJersey, SanDiego, Philadelphia, Colorado, Massachusetts, Ohio, NewYork. |
| `Year` | Calendar year of the week. Models use `Year >= 2023`. |
| `date_week_start` | First day of the week for the case date above.|
| `Week_Number` | Running week index within the full series. Train/test split is at `TRAIN_WEEK = 84`; out-of-sample forecasts are for weeks 86 to 130 (45 weeks). |
| `Cases` | Number of Mpox cases in that jurisdiction-week. |

### Week definition

**[CONFIRM: epidemiological (MMWR) week vs. ISO week.]** One lag = one week. A lag of 24 weeks therefore corresponds to about 5.5 months, consistent with the approximately 6-month interval from the phylogenetic analysis.

---

## Reproducibility

* **Software:** R version **[CONFIRM]**; packages `forecast`, `dplyr`, `tidyr`, `readxl`, `ggplot2`, plus the Lasso/VAR packages loaded in `libs_required.R` 
* **Operating system:**:
* **Random seed:** 
* **Hyperparameters:** `lambda = 0.05`, `TRAIN_WEEK = 84`, maximum 25 weekly lags for VAR-Lasso, `n_lags_ar = 8` for AR-Lasso, `max_p = 8` for ARIMA, naïve persistence `window = 1`
* **Edge handling: No smoothing**
* **Forecast design:** rolling two-week-ahead forecasts, fitted on data through week 84 and evaluated out of sample on 2024 weeks.

### Running the code

Scripts are in `R/`:

| File | Purpose |
|---|---|
| `data_processing.R` | Time-series formatting and train/test split |
| `VAR_lasso_model.R` | VAR-Lasso model |
| `AR_model.R` | Univariate AR-Lasso |
| `Arima_model.R` | ARIMA |
| `Negative_biomial_lagged_reg_lasso.R` | Negative binomial lasso regression |
| `Naive_persistence_model.R` | Naïve persistence |
| `performance_metrics.R` | Slope-weighted RMSE, MAE, bias |
| `plotting_functions.R`, `plot_forecast_horizon.R` | Figures |

1. Set `FOLDER` and `DATA_FOLDER` to your local paths.
2. Load `data_mpox.xlsx` and filter `Year >= 2023`.
3. Run the final-model block: `run_var_sd`, `run_nb_sd`, `run_AR_model`, `run_ARIMA_model`, `run_naive_model`, then compare with `bind_rows(...$overall)`.

## References

1. Centers for Disease Control and Prevention, National Center for Emerging and Zoonotic Infectious Diseases. *U.S. Case Data: Monkeypox.* Trends of monkeypox cases reported to CDC by date. Published September 16, 2024; updated October 8, 2026 (data reported to CDC as of October 1, 2026). Available at: https://www.cdc.gov/monkeypox/data-research/cases/index.html. Accessed November 6, 2026.
