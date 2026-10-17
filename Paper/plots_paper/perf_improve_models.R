library(dplyr)
library(tidyr)
library(ggplot2)

# =============================================================================
# Figure 3: % improvement of the VAR-Lasso over each of the four comparison
# models, for slope-weighted RMSE, MAE and bias (three panels).
# Improvement = (|comparison| - |VAR-Lasso|) / |comparison| * 100 (absolute values
# for bias), the same formula as Table 1.
# Built directly from the FINAL model run objects, so it stays in sync with Table 1:
#   var_sd, ar_model, nb_glm_out, arima_model, naive_model
# Untested (no R here).
# =============================================================================

if (!exists("FOLDER_SAVE")) FOLDER_SAVE <- ""

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
  grab(var_sd$overall,        "VAR-Lasso"),
  grab(ar_model$overall,      "AR-Lasso"),
  grab(nb_glm_out$overall,    "Negative binomial Lasso"),
  grab(arima_model$overall,   "ARIMA"),
  grab(naive_model$overall,   "Naïve (persistance)")
)
print(metrics, digits = 7)

# If you prefer to type the numbers in (e.g. to match Table 1 exactly), use this instead:
# metrics <- tibble::tribble(
#   ~model,              ~rmse,    ~mae,     ~bias,
#   "VAR-Lasso",         1.716735, 1.447338, -0.9518331,
#   "AR-Lasso",          2.529528, 2.066214, -1.9549121,
#   "Negative binomial", 2.680912, 2.243293, -2.1846729,
#   "ARIMA",             2.709571, 2.302901, -2.1883889,
#   "Naïve (last week)", 2.462214, 1.875000, -1.8125000)

# ---- % improvement of the VAR-Lasso relative to each comparison model -------------
var_row <- metrics %>% filter(model == "VAR-Lasso")

imp <- metrics %>%
  filter(model != "VAR-Lasso") %>%
  transmute(model,
            `Slope-weighted RMSE` = (abs(rmse) - abs(var_row$rmse)) / abs(rmse) * 100,
            `Slope-weighted MAE`  = (abs(mae)  - abs(var_row$mae))  / abs(mae)  * 100,
            `Slope-weighted bias` = (abs(bias) - abs(var_row$bias)) / abs(bias) * 100)

print(imp %>% mutate(across(where(is.numeric), ~ round(.x, 1))))   # compare with Table 1

imp_long <- imp %>%
  pivot_longer(-model, names_to = "metric", values_to = "improvement") %>%
  mutate(model  = factor(model, levels = c("AR-Lasso", "Negative binomial Lasso", "ARIMA", "Naïve (persistance)")),
         metric = factor(metric, levels = c("Slope-weighted RMSE", "Slope-weighted MAE", "Slope-weighted bias")))

cols <- c("AR-Lasso"          = "#1B7837",   # green
          "Negative binomial Lasso" = "#D95F02",   # orange
          "ARIMA"             = "#6A3D9A",   # purple
          "Naïve (persistance)" = "#E7298A")   # magenta

fig3 <- ggplot(imp_long, aes(x = model, y = improvement, fill = model, colour = model)) +
  geom_col(width = 0.7, alpha = 0.45, linewidth = 1.1) +
  geom_text(aes(label = sprintf("%.1f", improvement)), colour = "black",
            vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~ metric, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = cols, guide = "none") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "% improvement of VAR-Lasso") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 13),
        axis.text.x = element_text(angle = 35, hjust = 1, size = 11))

print(fig3)
ggsave(paste0(FOLDER_SAVE, "Figure3_improvements.png"), fig3,
       width = 12, height = 4.8, dpi = 300, bg = "white")