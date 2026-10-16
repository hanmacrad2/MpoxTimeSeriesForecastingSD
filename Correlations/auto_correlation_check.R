# =============================================================================
# How many own lags make sense for the San Diego AR model?
# Uses the 2023 TRAINING data only (no 2024 data), so the choice cannot be
# accused of being tuned on the test set. Untested (no R here).
# Needs: train_ts (from get_train_test_data(...)$train_data -> GET_TS_DATA)
# =============================================================================
#DATA

data_2023 = data_year %>%filter(Year == 2023)
train_ts = GET_TS_DATA(data_2023)

TARGET <- "SanDiego"
y <- train_ts[[TARGET]]          # weekly counts, training window only
d <- diff(y)                     # first differences: what the models actually fit
n <- length(d)
LAG_MAX <- 15                    # with ~50 weeks, lags beyond ~n/4 are unreliable
cat("Training weeks:", length(y), "| differences:", n,
    "| 95% bound for ACF/PACF: +/-", round(1.96 / sqrt(n), 3), "\n")

# ---- plots: levels and differences (look at both, see note below) ------------
png("SanDiego_ACF_PACF_2023.png", width = 1800, height = 1400, res = 200)
par(mfrow = c(2, 2))
acf(y,  lag.max = LAG_MAX, main = "ACF: weekly cases (levels)")
pacf(y, lag.max = LAG_MAX, main = "PACF: weekly cases (levels)")
acf(d,  lag.max = LAG_MAX, main = "ACF: first differences")
pacf(d, lag.max = LAG_MAX, main = "PACF: first differences")
dev.off()

# ---- order chosen by an information criterion (Yule-Walker AR fit) ------------
fit_d <- ar(d, aic = TRUE, order.max = 12)
fit_y <- ar(y, aic = TRUE, order.max = 12)
cat("AIC-chosen AR order, differenced series:", fit_d$order, "\n")
cat("AIC-chosen AR order, level series:      ", fit_y$order, "\n")

# ---- PACF cut-off: largest lag whose partial autocorrelation exceeds the bound -
pa <- pacf(d, lag.max = LAG_MAX, plot = FALSE)$acf[, 1, 1]
bound <- 1.96 / sqrt(n)
sig_lags <- which(abs(pa) > bound)
cat("Lags with |PACF| >", round(bound, 3), "(differences):",
    if (length(sig_lags)) sig_lags else "none", "\n")

# READ-OUT
#  * PACF of an AR(p) cuts off after lag p; ACF tails off.
#  * First differences of a noisy-but-flat series show ACF(1) near -0.5. That is
#    an artifact of differencing, not a real lag-1 effect, so judge the order
#    from the PACF of the levels too and report both.
#  * If nothing exceeds the bound, the series has no usable own-lag structure,
#    which is itself informative (it supports a very small order or none).