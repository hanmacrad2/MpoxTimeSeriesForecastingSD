# =============================================================================
# Top VAR-Lasso coefficients across the rolling refits (Figure 6a and Table 3)
#
# Purpose
#   Summarises the San Diego County (SDC) equation of the VAR-Lasso across the
#   rolling forecast origins:
#     - Figure 6a : the 5 largest coefficients (mean across refits, with the
#                   minimum-to-maximum range)
#     - Table 3   : stability of the Illinois lag 23 and lag 24 coefficients
#                   (mean, median, SD, range, % of refits non-zero, % of refits
#                   in which the term was the largest positive coefficient)
#
# Design
#   Uses the FINAL rolling VAR-Lasso run (lambda = 0.1; 45 refits in 2024).
#   A term absent from the non-zero coefficient table at an origin counts as 0
#   for that origin. Coefficients come from the first fit at each origin (the
#   model fitted to observed data).
#
# Needs
#   var_sd   : output of run_var_sd() (has $coeffs and $forecasts). In
#              02_run_all_models.R this object is called var_lasso_forecasts.
#   Packages : dplyr, ggplot2 (via libs_required.R)
#
# Outputs (in outputs/)
#   fig6a_top_coefficients.png
#   table3_illinois_coefficient_stability.csv
#   fig6a_top_coefficients_ranges.csv
# =============================================================================


#*****************************
# 1. SETTINGS

if (!exists("var_sd")) var_sd <- var_lasso_forecasts   # name used in the run-all script

N_TOP     <- 5               # number of coefficients shown in Figure 6a
ILL_LAGS  <- c(23, 24)       # Illinois lags reported in Table 3

pretty_jur <- function(x) {
  map <- c(NewYorkCity = "New York City", LA = "Los Angeles County",
           NewYork = "New York (state)", NewJersey = "New Jersey",
           Illinois = "Illinois", Colorado = "Colorado",
           Philadelphia = "Philadelphia", Ohio = "Ohio", Georgia = "Georgia",
           Massachusetts = "Massachusetts")
  unname(ifelse(x %in% names(map), map[x], x))
}


#*****************************
# 2. COEFFICIENTS ACROSS ALL REFITS

n_orig <- nrow(var_sd$forecasts)   # number of forecast origins (refits)

cf_all <- var_sd$coeffs %>%
  mutate(term = as.character(Predictor)) %>%
  filter(grepl("_L[0-9]+$", term)) %>%                 # drops the intercept
  mutate(jur = sub("_L[0-9]+$", "", term),
         lag = as.integer(sub(".*_L", "", term)))

# One row per (jurisdiction, lag): mean and range across ALL refits. If the term
# was absent (zero) in some refits, 0 is included in the mean and the range.
term_stats <- cf_all %>%
  group_by(term, jur, lag) %>%
  summarise(n_nz     = n(),
            mean_all = sum(Coefficient) / n_orig,
            lo       = if (n() < n_orig) min(Coefficient, 0) else min(Coefficient),
            hi       = if (n() < n_orig) max(Coefficient, 0) else max(Coefficient),
            .groups = "drop")

# Colours (approximated from the original figure; replace with the originals if needed)
fixed_cols <- c(Illinois = "#4DB8D9", LA = "#C5C84A", NewYorkCity = "#D45CC9")
pal_other  <- c("#C44D79", "#1B7837", "#C45500", "#6A3D9A", "#8C510A",
                "#00796B", "#9A7B00", "#333333", "#0077BB", "#B2182B")
jurs       <- unique(term_stats$jur)
rest       <- setdiff(jurs, names(fixed_cols))
jur_cols   <- c(fixed_cols[names(fixed_cols) %in% jurs],
                setNames(rep_len(pal_other, length(rest)), rest))


#*****************************
# 3. FIGURE 6a: LARGEST COEFFICIENTS

top_terms <- term_stats %>%
  arrange(desc(mean_all)) %>%
  slice_head(n = N_TOP) %>%
  mutate(label = factor(paste0(pretty_jur(jur), "\nlag ", lag),
                        levels = paste0(pretty_jur(jur), "\nlag ", lag)))

p_bars <- ggplot(top_terms, aes(x = label, y = mean_all, fill = jur)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.8) +
  geom_text(aes(y = hi, label = sprintf("%.2f", mean_all)),
            vjust = -0.7, size = 4.4, fontface = "bold") +
  scale_fill_manual(values = jur_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "VAR Lasso Coefficient", tag = "a)",
       title = "Top 5 largest VAR-Lasso coefficients for San Diego County",
       subtitle = paste0("Bars: mean over the ", n_orig,
                         " rolling refits. Whiskers: minimum to maximum across refits")) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_blank(),
        axis.text           = element_text(size = 15),
        plot.title          = element_text(size = 18),
        plot.subtitle       = element_text(colour = "grey30", size = 10.5),
        plot.tag            = element_text(face = "bold", family = "sans", size = 18),
        plot.tag.position   = c(0.01, 0.99))

print(p_bars)


#*****************************
# 4. TABLE 3: STABILITY OF THE ILLINOIS LAG 23 AND LAG 24 COEFFICIENTS

# the largest positive coefficient in each refit
top_per_origin <- cf_all %>%
  filter(Coefficient > 0) %>%
  group_by(Week_Number) %>%
  slice_max(Coefficient, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(Week_Number, top_term = term)

table3 <- cf_all %>%
  filter(jur == "Illinois", lag %in% ILL_LAGS) %>%
  left_join(top_per_origin, by = "Week_Number") %>%
  group_by(lag, term) %>%
  summarise(mean        = mean(Coefficient),
            median      = median(Coefficient),
            sd          = sd(Coefficient),
            min         = min(Coefficient),
            max         = max(Coefficient),
            pct_nonzero = 100 * n() / n_orig,
            pct_largest = 100 * sum(term == top_term) / n_orig,
            .groups = "drop") %>%
  dplyr::select(-term)

print(table3, digits = 3)
print(top_terms %>% dplyr::select(jur, lag, mean_all, lo, hi))


#*****************************
# 5. SAVE

dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/fig6a_top_coefficients.png", p_bars,
       width = 8, height = 5.5, dpi = 300, bg = "white")
write.csv(table3, "outputs/table3_illinois_coefficient_stability.csv", row.names = FALSE)
write.csv(top_terms %>% dplyr::select(jur, lag, mean_all, lo, hi),
          "outputs/fig6a_top_coefficients_ranges.csv", row.names = FALSE)