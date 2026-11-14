library(dplyr)
library(tidyr)

# =============================================================================
# Descriptive table: yearly Mpox cases by jurisdiction and % of the US yearly total
# for San Diego County plus the ten predictor jurisdictions (2022, 2023, 2024*),
# now with a Total column: cases summed over 2022-2024, as a % of the US total
# for the same three years.
# Needs: data_mpox (ALL years, columns Jurisdiction, Year, Cases).
# Untested (no R here).
# =============================================================================

DIGITS <- 1     # decimals in the percentages (set 0 for whole numbers)

# US yearly totals (CDC); 2024 is January to 24 November. National totals, so entered here.
us_totals <- c(`2022` = 30069, `2023` = 1747, `2024` = 2529)
us_ext    <- c(us_totals, Total = sum(us_totals))     # three-year US total = 34,345

# Row order and display names
row_order <- c("SanDiego", "NewYorkCity", "LA", "Illinois", "Colorado", "Philadelphia",
               "Ohio", "Georgia", "NewJersey", "NewYork", "Massachusetts")
pretty_names <- c(SanDiego = "San Diego County", NewYorkCity = "New York City",
                  LA = "Los Angeles County", Illinois = "Illinois", Colorado = "Colorado",
                  Philadelphia = "Philadelphia", Ohio = "Ohio", Georgia = "Georgia",
                  NewJersey = "New Jersey", NewYork = "New York (state)",
                  Massachusetts = "Massachusetts")

# ---- yearly case totals per jurisdiction, plus the 2022-2024 total ------------------
yearly <- data_mpox %>%
  filter(Jurisdiction %in% row_order, Year %in% 2022:2024) %>%
  group_by(Jurisdiction, Year) %>%
  summarise(cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
  complete(Jurisdiction = row_order, Year = 2022:2024, fill = list(cases = 0)) %>%
  mutate(Year = as.character(Year))

totals <- yearly %>%
  group_by(Jurisdiction) %>%
  summarise(cases = sum(cases), .groups = "drop") %>%
  mutate(Year = "Total")

long <- bind_rows(yearly, totals) %>%
  mutate(pct = 100 * cases / us_ext[Year])      # each cell relative to the US total for ITS period

# Quick check against the values already in your table (2022 / 2023 / 2024):
#   SDC 471 / 59 / 77, NYC 3837 / 198 / 392, LA 2281 / 120 / 202,
#   Illinois 1424 / 145 / 77, Georgia 1986 / 90 / 70
print(long %>% select(-pct) %>% pivot_wider(names_from = Year, values_from = cases), n = Inf)

# ---- total row for the 11 jurisdictions -----------------------------------------------
# CAUTION: if "New York (state)" includes New York City, NYC is counted twice here.
total_row <- long %>%
  group_by(Year) %>%
  summarise(cases = sum(cases), .groups = "drop") %>%
  mutate(pct = 100 * cases / us_ext[Year])

# ---- format cells as "n (x.x%)" ----------------------------------------------------------
fmt_cell <- function(n, pct) {
  sprintf("%s (%s%%)", format(n, big.mark = ","), formatC(pct, format = "f", digits = DIGITS))
}

body <- long %>%
  mutate(cell = fmt_cell(cases, pct),
         Jurisdiction = factor(Jurisdiction, levels = row_order)) %>%
  select(Jurisdiction, Year, cell) %>%
  pivot_wider(names_from = Year, values_from = cell) %>%
  arrange(Jurisdiction) %>%
  mutate(Jurisdiction = pretty_names[as.character(Jurisdiction)]) %>%
  select(Jurisdiction, `2022`, `2023`, `2024`, Total)

tot <- total_row %>%
  mutate(cell = fmt_cell(cases, pct)) %>%
  select(Year, cell) %>%
  pivot_wider(names_from = Year, values_from = cell) %>%
  mutate(Jurisdiction = "Total, 11 jurisdictions", .before = 1) %>%
  select(Jurisdiction, `2022`, `2023`, `2024`, Total)

us <- tibble(Jurisdiction = "US total cases",
             `2022`  = format(us_totals[["2022"]], big.mark = ","),
             `2023`  = format(us_totals[["2023"]], big.mark = ","),
             `2024`  = format(us_totals[["2024"]], big.mark = ","),
             Total   = format(sum(us_totals),      big.mark = ","))

tab1 <- bind_rows(body, tot, us)
names(tab1) <- c("Jurisdiction", "2022", "2023", "2024*", "Total 2022-2024*")
print(tab1, n = Inf)
write.csv(tab1, "Table1_yearly_cases_total.csv", row.names = FALSE)

#ORDERED LIST JUR
# 1. Turn the Total column back into numbers (it is text like "4,427 (12.9%)")
total_cases <- as.numeric(gsub(",", "", sub(" \\(.*$", "", tab1[[ncol(tab1)]])))

# 2. Drop the two summary rows and sort by total cases, largest first
ord <- tab1 %>%
  mutate(total_cases = total_cases) %>%
  filter(!Jurisdiction %in% c("Total, 11 jurisdictions", "US total cases")) %>%
  arrange(desc(total_cases))

# 3. The order as display names, and as the names used in your data
jur_display <- ord$Jurisdiction                      # "New York City", "Los Angeles County", ...
name_lookup <- setNames(names(pretty_names), pretty_names)
list_jur    <- unname(name_lookup[jur_display])      # "NewYorkCity", "LA", ...
                        # as a list, if the function needs one
print(list_jur)
