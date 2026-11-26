# =============================================================================
# create_mock_data.R.R
#
# Generates a SYNTHETIC weekly Mpox dataset in the same format as the real
# data, so the code in this repository can be run end to end without access to
# the CDC data. Nothing here is derived from real case counts, and results
# obtained with it have no epidemiological meaning.
#
# Run from the repository root:
#   source("mock_data/create_mock_data.R.R")
#
# Output
#   mock_data/data_mpox.xlsx   (sheet "MOCK_DATA_SYNTHETIC")
#
# Columns (as in the real data): Jurisdiction, date_week_start, Week_Number,
#   Week_Year, Year, Cases
#
# Design
#   - 11 jurisdictions, weeks 1-130. The calendar follows the real file:
#       weeks 1-31    Sundays from 2022-05-29 (Year 2022, Week_Year 22_2022 ...)
#       week 32       dated 2022-12-31, Week_Year 53_2022, Year 2022
#       weeks 33-130  Sundays from 2023-01-08 to 2024-11-17 (Week_Year 1_2023 ...)
#     So Year == 2023 has 52 weeks (weeks 33-84). Week_Number 84 is the last
#     training week and weeks 86-130 give the 45 rolling two-week-ahead
#     forecasts of 2024, as in the real analysis.
#   - Year is the calendar year of date_week_start. Week_Year is
#     "<week of year>_<year>".
#   - The real file has ~56 jurisdictions; only the 11 used in the analysis are
#     generated here.
#   - Illinois follows a two-peak wave (spring 2023 and early 2024). San Diego
#     County follows the same wave 24 weeks later, so there is a long-lag signal
#     for the VAR-Lasso to find. The other jurisdictions follow it with shorter,
#     different leads. These lags are illustrative only.
#   - Every jurisdiction also has a large summer-2022 wave, which the models
#     exclude by using Year >= 2023.
#   - Counts are negative binomial (over-dispersed, theta = 5), with a fixed
#     random seed.
# =============================================================================

set.seed(2026)

#*****************************
# 1. SETTINGS

N_WEEKS         <- 130
THETA           <- 5        # negative binomial size (smaller = more over-dispersed)
LAG_ILL         <- 24       # weeks by which San Diego follows Illinois
OUT_DIR         <- "mock_data"

# Per jurisdiction: baseline mean, height of the 2023-24 wave, height of the
# 2022 wave, and the lead (weeks) over San Diego County. Illinois leads by 24
# weeks (the long-lag signal); San Diego County is the target (lead 0).
spec <- tibble::tribble(
  ~Jurisdiction,   ~base, ~amp, ~amp22, ~lead,
  "NewYorkCity",    1.5,   16,   120,     12,
  "LA",             1.0,   12,    60,     18,
  "Georgia",        0.8,    8,    30,      2,
  "Illinois",       1.0,   22,    35,     24,
  "NewJersey",      0.6,    7,    20,     13,
  "SanDiego",       0.4,    5,    25,      0,
  "Philadelphia",   0.6,    7,    18,     16,
  "Colorado",       0.5,    6,    12,     20,
  "Massachusetts",  0.5,    5,    14,     14,
  "Ohio",           0.4,    4,    10,     15,
  "NewYork",        0.8,    6,    22,     10
)


#*****************************
# 2. CALENDAR

week_number     <- seq_len(N_WEEKS)
date_week_start <- c(as.Date("2022-05-29") + 7 * (0:30),   # weeks 1-31 (Sundays)
                     as.Date("2022-12-31"),                # week 32 (labelled 53_2022)
                     as.Date("2023-01-08") + 7 * (0:97))   # weeks 33-130 (Sundays)
year_cal        <- as.integer(format(date_week_start, "%Y"))

# week of the year, counted from the first Sunday used in each year's labels
week_of_year <- floor(as.numeric(date_week_start - as.Date(c("2022" = "2022-01-02",
                                                             "2023" = "2023-01-08",
                                                             "2024" = "2024-01-07")[as.character(year_cal)])) / 7) + 1
week_of_year[32] <- 53                                      # the real file's 53_2022
week_year    <- paste0(week_of_year, "_", year_cal)


#*****************************
# 3. LATENT MEAN AND COUNTS

bump <- function(t, centre, width) exp(-0.5 * ((t - centre) / width)^2)

# Illinois wave: peaks at weeks 44 (spring 2023) and 89 (early 2024)
wave <- function(t) bump(t, 44, 5) + 0.85 * bump(t, 89, 5)

mock_data <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
  s  <- spec[i, ]
  mu <- s$base +
    s$amp   * wave(week_number - (LAG_ILL - s$lead)) +    # follows Illinois, shifted
    s$amp22 * bump(week_number, 10, 5)                    # summer 2022 wave
  data.frame(Jurisdiction     = s$Jurisdiction,
             date_week_start  = date_week_start,
             Week_Number      = week_number,
             Week_Year        = week_year,
             Year             = year_cal,
             Cases            = rnbinom(N_WEEKS, size = THETA, mu = mu),
             stringsAsFactors = FALSE)
}))


#*****************************
# 4. CHECKS

stopifnot(all(tapply(mock_data$Cases, mock_data$Jurisdiction, sd) > 0),   # no constant series
          !anyNA(mock_data))

sdc <- subset(mock_data, Jurisdiction == "SanDiego")
cat("Jurisdictions:", length(unique(mock_data$Jurisdiction)),
    "| weeks per jurisdiction:", nrow(sdc), "\n")
cat("Weeks with Year == 2023:", sum(sdc$Year == 2023),
    "| Year >= 2023:", sum(sdc$Year >= 2023), "\n")
stopifnot(sum(sdc$Year == 2023) == 52, sum(sdc$Year >= 2023) == 98)
cat("Forecast weeks (Week_Number > 84, minus one):", sum(sdc$Week_Number > 84) - 1, "\n")


#*****************************
# 5. SAVE
OUT_DIR <- "C:/Users/h2cra/OneDrive/Documents/GitHub/MpoxTimeSeriesForecastingSD/mock_data"

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("Please install writexl: install.packages('writexl')")
}
dir.create(OUT_DIR, showWarnings = FALSE)
writexl::write_xlsx(list(MOCK_DATA_SYNTHETIC = mock_data),
                    file.path(OUT_DIR, "mock_data_mpox.xlsx"))
cat("Saved", file.path(OUT_DIR, "mock_data_mpox.xlsx"), "\n")

#**************************
#* Plot

#JURISDICTIONS
list_jur <- c("NewYorkCity", "LA", "Georgia", "Illinois", "NewJersey",
              "SanDiego",  "Philadelphia",  "Colorado", "Massachusetts",
              "Ohio", "NewYork") 

#PLOT
PLOT_WEEKLY_CASES(mock_data, list_jur)

