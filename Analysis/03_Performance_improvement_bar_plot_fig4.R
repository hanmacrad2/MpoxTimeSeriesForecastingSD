library(dplyr)
library(tidyr)
library(ggplot2)

# =============================================================================
# Figure 3: % improvement of the VAR-Lasso over each of the four comparison
# models, for slope-weighted RMSE, MAE and bias (three panels).
# Improvement = (|comparison| - |VAR-Lasso|) / |comparison| * 100 (absolute values
# for bias), the same formula as Table 2.
# Built directly from the FINAL model run objects, so it stays in sync with Table 2:
#   var_lasso_forecasts, ar_lasso_forecasts, nb_ar_lasso_forecasts, arima_forecasts, naive_forecasts
# =============================================================================

if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""

LAB_VAR   <- "VAR Lasso"
LAB_AR    <- "AR Lasso"
LAB_NB    <- "Neg Bin AR Lasso"
LAB_ARIMA <- "ARIMA"
LAB_NAIVE <- "Naïve (persistence)"

# ---- slope-weighted metrics for the five models ------------------------------
grab <- function(overall, label) {
  overall %>%
    slice(1) %>%
    transmute(model = label,
              rmse = slope_weighted_rmse,
              mae  = slope_weighted_mae,
              bias = slope_weighted_bias)
}

metrics <- bind_rows(
  grab(var_lasso_forecasts$overall,      LAB_VAR),
  grab(ar_lasso_forecasts$overall,    LAB_AR),
  grab(nb_ar_lasso_forecasts$overall,      LAB_NB),
  grab(arima_forecasts$overall, LAB_ARIMA),
  grab(naive_forecasts$overall, LAB_NAIVE)
)
print(metrics, digits = 7)

# Table 2 values, to check the objects above are the final runs:
#   VAR-Lasso          1.80  1.51  -1.09
#   AR-Lasso           2.53  2.07  -1.95
#   Negative binomial  2.71  2.26  -2.22
#   ARIMA              2.71  2.30  -2.19
#   Naive              2.46  1.88  -1.81
#
# To build the figure from the table instead of the model objects, uncomment:
# metrics <- tibble::tribble(
#   ~model,    ~rmse, ~mae,  ~bias,
#   LAB_VAR,    1.80,  1.51, -1.09,
#   LAB_AR,     2.53,  2.07, -1.95,
#   LAB_NB,     2.71,  2.26, -2.22,
#   LAB_ARIMA,  2.71,  2.30, -2.19,
#   LAB_NAIVE,  2.46,  1.88, -1.81)

# ---- % improvement of the VAR-Lasso relative to each comparison model --------
var_row <- metrics %>% filter(model == LAB_VAR)

imp <- metrics %>%
  filter(model != LAB_VAR) %>%
  transmute(model,
            `Slope-weighted RMSE` = (abs(rmse) - abs(var_row$rmse)) / abs(rmse) * 100,
            `Slope-weighted MAE`  = (abs(mae)  - abs(var_row$mae))  / abs(mae)  * 100,
            `Slope-weighted bias` = (abs(bias) - abs(var_row$bias)) / abs(bias) * 100)


print(imp %>% mutate(across(where(is.numeric), ~ round(.x, 0))))

imp_long <- imp %>%
  pivot_longer(-model, names_to = "metric", values_to = "improvement") %>%
  mutate(model  = factor(model, levels = c(LAB_AR, LAB_NB, LAB_ARIMA, LAB_NAIVE)),
         metric = factor(metric, levels = c("Slope-weighted RMSE",
                                            "Slope-weighted MAE",
                                            "Slope-weighted bias")))

cols <- setNames(c("#1B7837", "#D95F02", "#6A3D9A", "#E7298A"),
                 c(LAB_AR, LAB_NB, LAB_ARIMA, LAB_NAIVE))

fig3 <- ggplot(imp_long, aes(x = model, y = improvement, fill = model, colour = model)) +
  geom_col(width = 0.7, alpha = 0.45, linewidth = 1.1) +
  geom_text(aes(label = sprintf("%.0f", improvement)), colour = "black",
            vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~ metric, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = cols, guide = "none") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "% improvement of VAR-Lasso") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background   = element_blank(),
        strip.text         = element_text(face = "bold", size = 13),
        axis.text.x        = element_text(angle = 35, hjust = 1, size = 11))

print(fig3)
ggsave(paste0(FOLDER_SAVE, "Figure3_improvements.png"), fig3,
       width = 12, height = 4.8, dpi = 300, bg = "white")