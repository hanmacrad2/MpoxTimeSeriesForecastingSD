#library(ggplot2)
#library(dplyr)

# =============================================================================
# Autocorrelation figure for San Diego County, same style as the lagged
# cross-correlation figure. 2 x 2 panels:
#   ACF / PACF of the weekly counts (levels), ACF / PACF of the first differences.
# Uses the 2023 TRAINING data only (train_ts). Untested (no R here).
# =============================================================================

data_2023 = data_year %>%filter(Year == 2023)
train_ts = GET_TS_DATA(data_2023)

TARGET  <- "SanDiego"
LAG_MAX <- 25                       # with ~50 weeks, lags beyond ~n/4 are unreliable
y <- train_ts[[TARGET]]             # 2023 training window only
d <- diff(y)                        # what the models actually fit

# ---- ACF / PACF values and 95% bounds, one data frame -------------------------
get_cf <- function(x, type, label) {
  n  <- length(x)
  cf <- if (type == "ACF") acf(x,  lag.max = LAG_MAX, plot = FALSE)
  else               pacf(x, lag.max = LAG_MAX, plot = FALSE)
  lag <- as.numeric(cf$lag)
  val <- as.numeric(cf$acf)
  keep <- lag >= 1                              # acf() also returns lag 0
  tibble(panel = label, lag = lag[keep], value = val[keep],
         bound = qnorm(0.975) / sqrt(n))
}

cf_df <- bind_rows(
  get_cf(y, "ACF",  "ACF: weekly cases (levels)"),
  get_cf(y, "PACF", "PACF: weekly cases (levels)"),
  get_cf(d, "ACF",  "ACF: first differences"),
  get_cf(d, "PACF", "PACF: first differences")
) %>%
  mutate(panel = factor(panel, levels = unique(panel)),
         above = abs(value) > bound)

# ---- labels: up to 3 largest values beyond the bound per panel -----------------
lab_df <- cf_df %>%
  filter(above) %>%
  group_by(panel) %>%
  slice_max(abs(value), n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(text = sprintf("lag %d: r = %.2f", lag, value),
         hj = case_when(lag <= 3 ~ 0, lag >= LAG_MAX - 2 ~ 1, TRUE ~ 0.5),
         vj = ifelse(value >= 0, -1.2, 1.9))

# panels where nothing exceeds the bound get a note
none_df <- cf_df %>%
  group_by(panel) %>%
  summarise(any_above = any(above), .groups = "drop") %>%
  filter(!any_above) %>%
  mutate(lag = LAG_MAX / 2, value = 0.95, text = "no lag beyond the 95% bound")

bounds_df <- cf_df %>% distinct(panel, bound)
dark_cols <- c("#1F4E9E", "#B2182B", "#1B7837", "#C45500")
col_map   <- setNames(dark_cols, levels(cf_df$panel))

fig_ar <- ggplot(cf_df, aes(x = lag, y = value, colour = panel)) +
  geom_rect(data = bounds_df, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = -bound, ymax = bound),
            fill = "grey50", alpha = 0.10) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_hline(data = bounds_df, aes(yintercept =  bound),
             linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_hline(data = bounds_df, aes(yintercept = -bound),
             linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 1.4) +
  geom_point(data = filter(cf_df, above), size = 2.8) +
  geom_text(data = lab_df, aes(label = text, hjust = hj, vjust = vj),
            colour = "black", size = 4.6, fontface = "bold", show.legend = FALSE) +
  geom_text(data = none_df, aes(label = text), colour = "black",
            size = 4.2, show.legend = FALSE) +
  facet_wrap(~ panel, ncol = 2) +
  scale_colour_manual(values = col_map, guide = "none") +
  scale_x_continuous(breaks = seq(1, LAG_MAX, 2), limits = c(1, LAG_MAX),
                     expand = expansion(mult = 0.04)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.5)) +
  coord_cartesian(ylim = c(-1, 1.15)) +
  labs(x = "Lag (weeks)", y = "Correlation (r)",
       title = "Autocorrelation of San Diego County weekly Mpox cases",
       subtitle = paste0("2023 training data. Dashed lines: approximate 95% bounds (\u00b11.96/\u221an); ",
                         "large points: lags beyond the bounds.")) +
  theme_bw(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(colour = "grey30", size = 12))

print(fig_ar)
ggsave("Figure_SanDiego_autocorrelation.png", fig_ar,
       width = 11, height = 7.5, dpi = 300, bg = "white")

# Lags beyond the bound, for the Methods text
print(cf_df %>% filter(above) %>% select(panel, lag, value, bound), n = Inf)