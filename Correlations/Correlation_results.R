library(ggplot2)
library(dplyr)
library(tidyr)

# =============================================================================
# Needs ONLY data_2023: weekly 2023 data with columns
#   Jurisdiction, Cases, date_week_start
# Lagged Pearson correlation between San Diego (time t) and each
# jurisdiction (time t - lag), lags 1-25 weeks.
# =============================================================================
data_2023 = data_year %>% filter(Year == 2023)

THRESH  <- 0.40
MAX_LAG <- 25
TARGET  <- "SanDiego"

list_jur <- c("NewYorkCity", "LA", "Illinois", "Colorado", "Philadelphia",
              "Ohio", "Georgia", "NewJersey", "NewYork", "Massachusetts")

pretty_names <- c(
  NewYorkCity = "New York City", LA = "Los Angeles County",
  Illinois = "Illinois", Colorado = "Colorado", Philadelphia = "Philadelphia",
  Ohio = "Ohio", Georgia = "Georgia", NewJersey = "New Jersey",
  NewYork = "New York (state)", Massachusetts = "Massachusetts"
)

# ---- 1. Wide format: one column per jurisdiction ---------------------------
cases_wide <- data_2023 %>%
  filter(Jurisdiction %in% c(TARGET, list_jur)) %>%
  select(date_week_start, Jurisdiction, Cases) %>%
  pivot_wider(names_from = Jurisdiction, values_from = Cases) %>%
  arrange(date_week_start)

absent <- setdiff(c(TARGET, list_jur), names(cases_wide))
if (length(absent) > 0) {
  warning("Not found in data_2023 (check spelling): ", paste(absent, collapse = ", "))
  list_jur <- intersect(list_jur, names(cases_wide))
}
cat("Weeks:", nrow(cases_wide), "|",
    as.character(min(cases_wide$date_week_start)), "to",
    as.character(max(cases_wide$date_week_start)), "\n")

# ---- 2. Lagged correlation function ------------------------------------------
# San Diego at week t vs jurisdiction at week t - lag
# (positive lag = the other jurisdiction LEADS San Diego)
lagged_cor <- function(x, y, lag) {
  n     <- length(x)
  x_lag <- x[(lag + 1):n]
  y_lag <- y[1:(n - lag)]
  ok    <- complete.cases(x_lag, y_lag)
  if (sum(ok) >= 3 && sd(x_lag[ok]) > 0 && sd(y_lag[ok]) > 0)
    cor(x_lag[ok], y_lag[ok]) else NA_real_
}

sd_series <- cases_wide[[TARGET]]

ccf_sd <- expand.grid(jurisdiction = list_jur, lag = 1:MAX_LAG,
                      stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(correlation = lagged_cor(sd_series, cases_wide[[jurisdiction]], lag)) %>%
  ungroup()

# ---- 3. Peak correlation per jurisdiction ------------------------------------
peaks <- ccf_sd %>%
  group_by(jurisdiction) %>%
  slice_max(correlation, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(correlation)) %>%
  mutate(meets_threshold = correlation >= THRESH)

print(peaks, n = Inf)   # peak r and the lag at which it occurs

# ---- 4. One plot: all jurisdictions, coloured lines --------------------------
ccf_sd <- ccf_sd %>%
  mutate(label = factor(pretty_names[jurisdiction],
                        levels = pretty_names[peaks$jurisdiction]))  # legend: strongest first

fig2 <- ggplot(ccf_sd, aes(x = lag, y = correlation, colour = label)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = THRESH, linetype = "dashed",
             colour = "black", linewidth = 0.6) +
  annotate("text", x = MAX_LAG, y = THRESH, label = paste0("r = ", THRESH),
           hjust = 1, vjust = -0.6, size = 3.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_colour_brewer(palette = "Paired", name = "Jurisdiction") +
  scale_x_continuous(breaks = seq(1, MAX_LAG, 2)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  labs(
    x = "Lag (weeks): other jurisdiction leads San Diego County",
    y = "Correlation (r)",
    title = "Lagged correlations between San Diego County and other jurisdictions",
    subtitle = "Weekly case counts, 2023; dashed line marks r = 0.40"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

print(fig2)
ggsave("Figure2_lagged_correlations.png", fig2,
       width = 10, height = 6, dpi = 300, bg = "white")