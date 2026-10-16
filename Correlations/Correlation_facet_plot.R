library(ggplot2)
library(dplyr)
library(tidyr)

# =============================================================================
# Needs ONLY data_2023: weekly 2023 data with columns
#   Jurisdiction, Cases, date_week_start
# Lagged Pearson correlation between San Diego (time t) and each
# jurisdiction (time t - lag), lags 1-25 weeks. One facet per jurisdiction.
# =============================================================================
if (!exists("data_2023")) data_2023 <- data_year %>% filter(Year == 2023)

THRESH  <- 0.40
MAX_LAG <- 25
TARGET  <- "SanDiego"
NCOL    <- 2          # number of facet columns

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

# ---- 2. Lagged correlation (same function as before) ------------------------
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

# ---- 3. Peak per jurisdiction -------------------------------------------------
peaks <- ccf_sd %>%
  group_by(jurisdiction) %>%
  slice_max(correlation, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(correlation)) %>%
  mutate(meets_threshold = correlation >= THRESH)

print(peaks, n = Inf)

# ---- 4. Plot data: panels ordered strongest -> weakest ------------------------
ord <- pretty_names[peaks$jurisdiction]

ccf_sd <- ccf_sd %>%
  mutate(label = factor(pretty_names[jurisdiction], levels = ord),
         above = correlation >= THRESH)

peak_lab <- peaks %>%
  filter(meets_threshold) %>%                       # label only peaks >= THRESH
  mutate(label = factor(pretty_names[jurisdiction], levels = ord),
         text  = sprintf("wk %d: r = %.2f", lag, correlation),
         hj    = ifelse(lag > MAX_LAG - 6, 1, 0.5)) # keep labels inside the panel

# ---- 5. Facet plot: one panel per jurisdiction ---------------------------------
fig2 <- ggplot(ccf_sd, aes(x = lag, y = correlation, colour = label)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = THRESH, ymax = Inf,
           fill = "grey50", alpha = 0.10) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = THRESH, linetype = "dashed",
             colour = "black", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.4) +
  geom_point(data = filter(ccf_sd, above), size = 2.8) +          # lags >= 0.40
  geom_point(data = peak_lab, shape = 21, size = 4,
             fill = NA, stroke = 1, show.legend = FALSE) +        # ring on the peak
  geom_text(data = peak_lab, aes(label = text, hjust = hj),
            vjust = -1.2, size = 4.8, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ label, ncol = NCOL) +
  scale_colour_brewer(palette = "Paired", guide = "none") +
  scale_x_continuous(breaks = seq(1, MAX_LAG, 4), limits = c(1, MAX_LAG)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.5)) +
  coord_cartesian(ylim = c(-1, 1.2)) +
  labs(
    x = "Lag (weeks): other jurisdiction leads San Diego County",
    y = "Correlation (r)",
    title = "Lagged correlations between San Diego County and each jurisdiction",
    subtitle = paste0("Weekly case counts, 2023. Dashed line: r = ", THRESH,
                      ";\nlarge points: lags with r \u2265 ", THRESH,
                      "; labelled point: peak correlation.")
  ) +
  theme_bw(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(colour = "grey30", size = 12))

print(fig2)
ggsave("Figure2_facet_correlations.png", fig2,
       width = 11, height = ceiling(length(list_jur) / NCOL) * 3 + 1.5,
       dpi = 300, bg = "white")