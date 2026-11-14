# =============================================================================
# Packages used by all scripts. Sourced by the data setup script.
# =============================================================================

# 1. Install anything that is missing (runs once, skips packages already there)
required_pkgs <- c("dplyr", "tidyr", "tibble", "readxl", "ggplot2", "ggh4x",
                   "scales", "reshape2", "forecast", "BigVAR", "mpath")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

# 2. Load the third-party packages. dplyr goes after the others so its select()
#    and filter() are not masked (mpath and others mask select()).
library(tidyr)
library(tibble)
library(readxl)     # reading data_mpox.xlsx
library(ggplot2)    # figures
library(ggh4x)      # facet_wrap2() in the plotting functions
library(scales)
library(forecast)   # ARIMA (auto.arima)
library(BigVAR)     # VAR-Lasso and AR-Lasso
library(mpath)      # negative binomial lasso (glmregNB)
library(dplyr)      # loaded last, see above

# reshape2 is called as reshape2::melt() in EXTRACT_NONZERO_LASSO_COEFFS(), so
# it must be installed but does not need library().

# 3. Load the paper package LAST so its functions are not overwritten by any
#    of the packages above.
library(MpoxTimeSeriesForecastingSD)