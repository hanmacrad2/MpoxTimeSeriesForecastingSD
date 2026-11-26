# Mock data

**These data are synthetic.** They contain no real case counts, were not derived from the CDC data, and results obtained from them have no epidemiological meaning. They exist so that anyone can check that the code in this repository runs end to end, including readers who do not have access to the real data.

## Files

| File | Contents |
|---|---|
| `mock_data_mpox.xlsx` | Synthetic weekly counts for the 11 jurisdictions (sheet `MOCK_DATA_SYNTHETIC`) |
| `make_mock_data.R` | The script that generates it (fixed random seed, 2026) |

## How to use

In the data setup script (`01_get_data_and_plot.R`), where the data are read, set the folder and file name to the mock data:

```r
DATA_FOLDER <- "mock_data/"
file_name   <- "mock_data_mpox.xlsx"
```

The columns match the real data, so the rest of the scripts run as usual, starting with `01_get_data_and_plot.R`. Set `DATA_FOLDER` and `file_name` back to the real data to run the analysis on the CDC data.

To regenerate it, run from the repository root:

```r
install.packages("writexl")        # once
source("mock_data/make_mock_data.R")
```

## Format

Same columns as the real data (see the data dictionary in the main README):

| Column | Description |
|---|---|
| `Jurisdiction` | The 11 jurisdictions used in the analysis (the real file has about 56) |
| `date_week_start` | Sunday on which the week starts. Week 1 is 2022-05-29, week 32 is dated 2022-12-31, weeks 33 to 130 run from 2023-01-08 to 2024-11-17 (as in the real file) |
| `Week_Number` | 1 to 130; week 84 is the last training week and weeks 86 to 130 give the 45 rolling forecasts for 2024 |
| `Week_Year` | Week of the year and year, for example `22_2022`, `53_2022` (week 32), `1_2023` and `46_2024` |
| `Year` | Calendar year of `date_week_start`; `Year == 2023` has 52 weeks (weeks 33 to 84) |
| `Cases` | Weekly count (negative binomial, over-dispersed) |

## How the data were made

- Illinois follows a two-peak wave (spring 2023 and early 2024). San Diego County follows the same wave 24 weeks later, so the VAR-Lasso has a long-lag signal to find. The other jurisdictions follow it with shorter, different leads. These leads are illustrative and are not the lags reported in the paper.
- Every jurisdiction has a large summer-2022 wave, which the models exclude through `Year >= 2023`, as with the real data.
- Counts are drawn from a negative binomial distribution with a fixed seed.

## What it can and cannot show

- It shows that the pipeline runs and produces outputs of the right shape (forecasts, error metrics, coefficient tables, figures).
- It will not reproduce the paper's numbers. Any script with values typed into it, such as screened lags or peak correlations, will run but give different results from the paper.
- Do not report or cite results from this dataset.
