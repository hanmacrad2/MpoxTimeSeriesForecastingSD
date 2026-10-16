library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# =============================================================================
# SELF-CONTAINED: starts from the raw weekly data and computes everything.
# Needs: data_year = weekly data for 2023 with columns
#        Jurisdiction, Cases, date_week_start   (as in your existing code)
# e.g.   data_year <- data_mpox %>% filter(Year == 2023)
# =============================================================================

THRESH   <- 0.40     # selection threshold (matches Methods: r >= 0.40)
MAX_LAG  <- 25
TARGET   <- "SanDiego"

# ---- 1. Candidate predictors -----------------------------------------------
# Methods: the 20 jurisdictions with the highest total cases across 2022-2023.
# If the full dataset (data_mpox, incl. 2022) is loaded, compute them;
# otherwise fall back to every jurisdiction present in data_year.
if (exists("data_mpox")) {
  candidates <- data_mpox %>%
    filter(Year %in% 2022:2023) %>%
    group_by(Jurisdiction) %>%
    summarise(total = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
    filter(!Jurisdiction %in% c(TARGET, "California")) %>%  # California contains SD/LA
    slice_max(total, n = 20, with_ties = FALSE) %>%
    pull(Jurisdiction)
} else {
  candidates <- setdiff(unique(data_year$Jurisdiction), c(TARGET, "California"))
}
candidates <- intersect(candidates, unique(data_year$Jurisdiction))

# ---- 2. Wide format: one column per jurisdiction ---------------------------
cases_wide <- data_year %>%
  filter(Jurisdiction %in% c(TARGET, candidates)) %>%
  select(date_week_start, Jurisdiction, Cases) %>%
  pivot_wider(names_from = Jurisdiction, values_from = Cases) %>%
  arrange(date_week_start)

# ---- 3. Lagged correlations: SanDiego(t) vs candidate(t - lag) --------------
x <- cases_wide[[TARGET]]
n <- length(x)

corr_df <- expand.grid(jurisdiction_2 = candidates, lag = 1:MAX_LAG,
                       stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(correlation = {
    y      <- cases_wide[[jurisdiction_2]]
    x_lag  <- x[(lag + 1):n]
    y_lag  <- y[1:(n - lag)]
    ok     <- complete.cases(x_lag, y_lag)
    if (sum(ok) >= 3) cor(x_lag[ok], y_lag[ok]) else NA_real_
  }) %>%
  ungroup()

# ---- 4. Select predictors: peak correlation >= THRESH at any lag ------------
peaks_all <- corr_df %>%
  group_by(jurisdiction_2) %>%
  slice_max(correlation, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(correlation))

print(peaks_all, n = Inf)          # inspect the full ranking

peaks    <- filter(peaks_all, correlation >= THRESH)
selected <- peaks$jurisdiction_2
cat("Selected predictors (", length(selected), "):", selected, "\n")

# ---- 5. Pretty labels (unlisted names are shown as-is) ----------------------
pretty_names <- c(
  NewYorkCity = "New York City", LA = "Los Angeles County",
  Illinois = "Illinois", Colorado = "Colorado", Philadelphia = "Philadelphia",
  Ohio = "Ohio", Georgia = "Georgia", NewJersey = "New Jersey",
  NewYork = "New York (state)", Massachusetts = "Massachusetts",
  Texas = "Texas", Florida = "Florida", Washington = "Washington",
  NorthCarolina = "North Carolina"
)
lab <- function(v) ifelse(v %in% names(pretty_names), pretty_names[v], v)

plot_df <- corr_df %>%
  filter(jurisdiction_2 %in% selected) %>%
  mutate(label = lab(jurisdiction_2))

peaks <- peaks %>%
  mutate(label = lab(jurisdiction_2),
         peak_text = sprintf("r = %.2f at %d wk", correlation, lag))

ord <- peaks$label                      # already strongest -> weakest
plot_df$label <- factor(plot_df$label, levels = ord)
peaks$label   <- factor(peaks$label,   levels = ord)

# ---- 6. Figure 2: small multiples ------------------------------------------
fig2 <- ggplot(plot_df, aes(x = lag, y = correlation)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = THRESH, ymax = Inf,
           fill = "#2C7FB8", alpha = 0.08) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = THRESH, linetype = "dashed",
             colour = "#2C7FB8", linewidth = 0.5) +
  geom_line(colour = "grey30", linewidth = 0.8) +
  geom_point(data = peaks, colour = "#D95F0E", size = 2.6) +
  geom_text(data = peaks, aes(label = peak_text),
            colour = "#D95F0E", size = 3, vjust = -1, check_overlap = TRUE) +
  facet_wrap(~ label, ncol = 2) +
  scale_x_continuous(breaks = seq(1, MAX_LAG, 4), limits = c(1, MAX_LAG)) +
  scale_y_continuous(limits = c(-0.6, 1), breaks = seq(-0.5, 1, 0.5),
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    x = "Lag (weeks): other jurisdiction leads San Diego County",
    y = "Pearson correlation (r)",
    title = "Lagged correlations between San Diego County and predictor jurisdictions",
    subtitle = paste0("Weekly case counts, 2023. Dashed line: r = ", THRESH,
                      " selection threshold. Orange point: peak correlation.")
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = "grey70"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30", size = 9))

print(fig2)

ggsave("Figure2_lagged_correlations.png", fig2,
       width = 8, height = 9, dpi = 300, bg = "white")

# ---- 7. Alternative: heatmap -------------------------------------------------
fig2_heat <- ggplot(plot_df, aes(x = lag, y = fct_rev(label), fill = correlation)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_tile(data = filter(plot_df, correlation >= THRESH),
            fill = NA, colour = "black", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1, MAX_LAG, 2), expand = c(0, 0)) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(x = "Lag (weeks)", y = NULL,
       title = "Lagged correlations with San Diego County",
       subtitle = paste0("Black outline: r \u2265 ", THRESH)) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"))

print(fig2_heat)
ggsave("Figure2_heatmap_alt.png", fig2_heat,
       width = 9, height = 4.5, dpi = 300, bg = "white")