#*****************************************
# POPULATION-ADJUSTED INCIDENCE RATES
#
# Purpose
#   Convert weekly Mpox case counts to incidence rates (cases per 100,000
#   population) for the sensitivity analysis in Supplementary Section S6.
#   Each jurisdiction is divided by a fixed population denominator, the same
#   for every week.
#
# Population denominators (Supplementary Table S8)
#   Source: U.S. Census Bureau, see the reference list in the paper.
#   New York (state) is the series EXCLUDING New York City, so its denominator
#   is the New York State total minus the New York City population.
#
# Needs
#   Packages: dplyr, tibble
#   Objects defined elsewhere: data_year (weekly counts with columns
#   Jurisdiction, date_week_start, Week_Number, Cases) and list_jur
#*****************************************

library(dplyr)

#*****************************************
# 1. POPULATION DENOMINATORS
#*****************************************

NYC_POP      <- 8804190     # New York City
NY_STATE_POP <- 19786543    # New York State, including New York City  [CONFIRM source and year]

pop_df <- tibble::tribble(
  ~Jurisdiction,    ~Population,
  "NewYorkCity",     NYC_POP,
  "LA",              10014009,                    # Los Angeles County
  "Georgia",         11063669,
  "Illinois",        12633389,
  "NewJersey",       9395315,
  "SanDiego",        3298634,                     # San Diego County
  "Philadelphia",    1603797,
  "Colorado",        5912240,
  "Massachusetts",   7073479,
  "Ohio",            11756058,                    # [CONFIRM: 11,756,058 here, 11,808,390 in earlier notes]
  "NewYork",         NY_STATE_POP - NYC_POP       # New York State excluding New York City (10,982,353)
)

#*****************************************
# 2. CONVERT COUNTS TO RATES
#*****************************************

make_rate_data <- function(data_counts, pop_df, case_col = "Cases", per = 1e5) {
  
  'Convert weekly case counts to incidence rates per 100,000 population.
  
  Args:
    data_counts : long data frame with a Jurisdiction column and the count
                  column named by case_col.
    pop_df      : data frame with columns Jurisdiction and Population, one row
                  per jurisdiction.
    case_col    : name of the count column (Cases).
    per         : rate denominator (100,000).
  
  Each count is divided by the population of its jurisdiction and multiplied
  by per. The same population is used for every week. The original counts are
  kept in Cases_count so that forecasts can be converted back to counts.
  
  Stops with an error if any jurisdiction has no population.
  
  Returns the data frame with case_col replaced by the rate, plus Population
  and Cases_count columns.'
  
  missing <- setdiff(unique(data_counts$Jurisdiction), pop_df$Jurisdiction)
  if (length(missing) > 0) stop("No population for: ", paste(missing, collapse = ", "))
  
  data_counts %>%
    left_join(pop_df, by = "Jurisdiction") %>%
    mutate(Cases_count = .data[[case_col]],
           !!case_col := Cases_count / Population * per)
}

data_rate <- make_rate_data(data_year, pop_df)

#*****************************************
# 3. CHECKS
#*****************************************

# Converting back must recover the original counts
stopifnot(isTRUE(all.equal(data_rate$Cases * data_rate$Population / 1e5,
                           data_rate$Cases_count)))

# One population per jurisdiction, and every modelled jurisdiction present
stopifnot(all(list_jur %in% data_rate$Jurisdiction))
data_rate %>% distinct(Jurisdiction, Population)    # compare with Table S8