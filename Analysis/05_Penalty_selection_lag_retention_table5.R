# =============================================================================
# VAR-Lasso coefficient retention by penalty (Table 5)
#
# Purpose
#   Shows which coefficients in the San Diego County (SDC) equation of the
#   VAR-Lasso survive as the lasso penalty (lambda) increases. Two things are
#   tracked at each penalty:
#     - the Illinois coefficients at lags 23-25 (the hypothesised long-lag signal)
#     - how many of the 10 cross-jurisdiction terms flagged by the correlation
#       screening are retained (a selection rule that does not rely on Illinois
#       alone)
#
# Design
#   One VAR-Lasso fit per lambda on the 2023 TRAINING data only (first-differenced
#   weekly counts, 25 lags). No 2024 data and no forecasting loop. These are
#   2023-only fits, so coefficients differ from the rolling refits in Table 3.
#
# Needs
#   data_year, list_jur  : from 01_get_data_and_plot.R
#   Packages             : dplyr, tibble, BigVAR (via libs_required.R)
#   Functions            : GET_TS_DATA(), EXTRACT_NONZERO_LASSO_COEFFS()
#
# Outputs
#   outputs/table5_var_lasso_penalty_retention.csv  : full results, one row per lambda
#   outputs/table5_var_lasso_paper_table.csv        : columns as shown in Table 5
# =============================================================================


#*****************************
# 1. SETTINGS

TARGET   <- "SanDiego"
N_LAGS   <- 25
ILL_LAGS <- 23:25
LAMBDAS  <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 5, 10, 20, 30)

# Peak lag and correlation from the 2023 cross-correlation screening
# (cross_correlation_screening_sdc.R). SanDiego is the response, so it has no
# screened term. Row order sets the order of the "retained" lists in the table.
screen <- tibble::tribble(
  ~jurisdiction,   ~code,  ~lag, ~peak_r,
  "LA",            "LA",   17,   0.71,
  "Illinois",      "IL",   24,   0.70,
  "Colorado",      "CO",   19,   0.62,
  "Philadelphia",  "PH",   17,   0.61,
  "Ohio",          "OH",   17,   0.55,
  "Georgia",       "GA",    1,   0.53,
  "NewYorkCity",   "NYC",  13,   0.51,
  "NewJersey",     "NJ",   14,   0.49,
  "NewYork",       "NYS",  12,   0.47,
  "Massachusetts", "MA",   14,   0.41
) %>%
  mutate(term  = paste0(jurisdiction, "_L", lag),   # name used in the coefficient table
         label = paste(code, lag))                  # label used in the paper table

ill_terms <- paste0("Illinois_L", ILL_LAGS)


#*****************************
# 2. DATA (2023 only)

data_2023 <- data_year %>% filter(Year == 2023, Jurisdiction %in% list_jur)
Y_all <- GET_TS_DATA(data_2023) %>%
  dplyr::select(-Week_Number, -date_week_start) %>%
  as.matrix()

stopifnot(TARGET %in% colnames(Y_all),
          all(screen$jurisdiction %in% colnames(Y_all)))

cat("2023 weeks used:", nrow(Y_all), "| series:", ncol(Y_all), "\n")
print(dplyr::select(screen, term, peak_r))


#*****************************
# 3. FIT ONE PENALTY: returns the non-zero SDC-equation coefficients (named vector)

fit_var_nonzero <- function(lam) {
  Yd <- diff(Y_all)                      # the VAR-Lasso is fitted to differenced counts
  
  model <- constructModel(Yd, p = N_LAGS, struct = "Basic", gran = c(lam),
                          ownlambdas = TRUE, T1 = floor(nrow(Yd) * 0.65),
                          verbose = FALSE)
  fit <- cv.BigVAR(model)
  
  cf <- EXTRACT_NONZERO_LASSO_COEFFS(coef(fit), colnames(Y_all)) %>%
    filter(Response == TARGET,
           grepl("_L[0-9]+$", as.character(Predictor)))   # drops the intercept
  
  setNames(cf$Coefficient, as.character(cf$Predictor))
}


#*****************************
# 4. SUMMARISE ONE FIT: one row per penalty

summarise_fit <- function(lam, nz) {
  
  # Illinois lags 23-25 (0 if the penalty removed the term)
  ill <- setNames(numeric(length(ill_terms)), ill_terms)
  kept_ill <- intersect(ill_terms, names(nz))
  ill[kept_ill] <- nz[kept_ill]
  
  # rank among retained coefficients (1 = largest positive coefficient)
  ill_rank <- rank(-nz, ties.method = "min")[kept_ill]
  
  # largest positive coefficient in the equation
  positive <- nz[nz > 0]
  top <- if (length(positive)) names(which.max(positive)) else NA_character_
  
  # screened terms retained at this penalty
  kept <- screen$term %in% names(nz)
  
  tibble(lambda             = lam,
         n_candidates       = ncol(Y_all) * N_LAGS,
         n_nonzero          = length(nz),
         !!!as.list(ill),
         best_illinois_rank = if (length(ill_rank)) min(ill_rank) else NA_integer_,
         n_screened_kept    = sum(kept),
         pct_screened_kept  = round(100 * mean(kept), 1),
         screened_kept      = paste(screen$label[kept], collapse = "; "),
         screened_dropped   = paste(screen$label[!kept], collapse = "; "),
         largest_term       = top,
         largest_coef       = if (is.na(top)) NA_real_ else unname(nz[top]))
}


#*****************************
# 5. RUN ACROSS PENALTIES

run_one <- function(lam) {
  tryCatch(summarise_fit(lam, fit_var_nonzero(lam)),
           error = function(e) {
             message("lambda ", lam, " failed: ", conditionMessage(e))
             NULL
           })
}

results <- bind_rows(lapply(LAMBDAS, run_one))

options(width = 250)
print(results, n = Inf)

# Largest penalty that still retains all screened terms
all_kept <- results %>% filter(n_screened_kept == nrow(screen))
cat("\nLargest lambda retaining all", nrow(screen), "screened terms:",
    if (nrow(all_kept)) max(all_kept$lambda) else NA, "\n")


#*****************************
# 6. SAVE

paper_table <- results %>%
  transmute(lambda,
            non_zero_coefficients = n_nonzero,
            illinois_lag_23       = round(Illinois_L23, 3),
            illinois_lag_24       = round(Illinois_L24, 3),
            largest_term,
            largest_coefficient   = round(largest_coef, 3),
            screened_retained     = sprintf("%d (%d%%)", n_screened_kept,
                                            round(pct_screened_kept)),
            screened_terms        = screened_kept)

dir.create("outputs", showWarnings = FALSE)
write.csv(results,     "outputs/table5_var_lasso_penalty_retention.csv", row.names = FALSE)
write.csv(paper_table, "outputs/table5_var_lasso_paper_table.csv",       row.names = FALSE)