library(dplyr)
library(tidyr)
library(ggplot2)

# =============================================================================
# Panel A, two versions, both from the FINAL VAR-Lasso run (lambda = 0.05):
#   (1) p_bars    : the 5 largest coefficients with their range across the rolling refits
#   (2) p_profile : coefficient by lag for Illinois (line + range band), other
#                   jurisdictions as thin grey lines, and the phylogenetic lag range shaded
# Needs: var_sd (from run_var_sd), which has $coeffs and $forecasts.
# Untested (no R here).
# =============================================================================

N_LAGS    <- 25
PHYLO_LO  <- 22          # phylogenetic lag range, weeks (Section 2.2)
PHYLO_HI  <- 28
X_MAX     <- 28          # x-axis runs past the model's 25 lags so the whole phylogenetic band is visible
LAMBDA    <- 0.05
if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""

pretty_jur <- function(x) {
  map <- c(NewYorkCity = "New York City", LA = "Los Angeles County", NewYork = "New York (state)",
           NewJersey = "New Jersey", Illinois = "Illinois", Colorado = "Colorado",
           Philadelphia = "Philadelphia", Ohio = "Ohio", Georgia = "Georgia",
           Massachusetts = "Massachusetts")
  unname(ifelse(x %in% names(map), map[x], x))
}

# ---- one row per (jurisdiction, lag): mean and range across ALL refits ----------
# The coefficient table lists only non-zero terms, so a refit in which a term is
# absent counts as 0 for that term.
n_orig <- nrow(var_sd$forecasts)

cf_all <- var_sd$coeffs %>%
  mutate(term = as.character(Predictor)) %>%
  filter(grepl("_L[0-9]+$", term)) %>%                      # drops the intercept
  mutate(jur = sub("_L[0-9]+$", "", term),
         lag = as.integer(sub(".*_L", "", term)))

term_stats <- cf_all %>%
  group_by(term, jur, lag) %>%
  summarise(n_nz     = n(),
            mean_all = sum(Coefficient) / n_orig,
            lo       = min(Coefficient, if (n() < n_orig) 0 else Inf),
            hi       = max(Coefficient, if (n() < n_orig) 0 else -Inf),
            .groups = "drop")

# colours matched to the existing figure (approximated by eye from the image;
# replace the hex codes below if you have the originals)
fixed_cols <- c(Illinois = "#4DB8D9",       # light blue
                LA       = "#C5C84A",       # olive yellow-green
                NewYorkCity = "#D45CC9")    # orchid / magenta
# any other jurisdiction that appears gets one of these
pal_other <- c("#C44D79", "#1B7837", "#C45500", "#6A3D9A", "#8C510A",
               "#00796B", "#9A7B00", "#333333", "#0077BB", "#B2182B")
jurs     <- unique(term_stats$jur)
rest     <- setdiff(jurs, names(fixed_cols))
jur_cols <- c(fixed_cols[names(fixed_cols) %in% jurs],
              setNames(rep_len(pal_other, length(rest)), rest))

# =============================================================================
# (1) BAR CHART: 5 largest (positive) mean coefficients, with min-max across refits
# =============================================================================
top5 <- term_stats %>%
  arrange(desc(mean_all)) %>%
  slice_head(n = 5) %>%
  mutate(label = paste0(pretty_jur(jur), "\nlag ", lag))
top5$label <- factor(top5$label, levels = top5$label)

p_bars <- ggplot(top5, aes(x = label, y = mean_all, fill = jur)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.8) +
  geom_text(aes(y = hi, label = sprintf("%.2f", mean_all)),
            vjust = -0.7, size = 4.4, fontface = "bold") +
  scale_fill_manual(values = jur_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "VAR Lasso Coefficient",
       title = "Top 5 largest VAR-Lasso coefficients for San Diego County",
       #title = "Coefficient (Lag weeks)",
       #title = "Mpox weekly reported cases: Illinois vs San Diego, 2023-2024",
       subtitle = paste0("Bars: mean over the ", n_orig, " rolling refits. Whiskers: minimum to maximum across refits")) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(colour = "grey30", size = 10.5))

#SAVE
print(p_bars)
ggsave(paste0(FOLDER_SAVE, "Fig_panelA_bars.png"),    p_bars,    width = 8,  height = 5.5, dpi = 300, bg = "white")

# the numbers behind the bars, for the caption or text
print(top5 %>% select(jur, lag, mean_all, lo, hi, n_nz), n = Inf)