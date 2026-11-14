# =============================================================================
# 01. Load the data, define the jurisdictions, and plot weekly Mpox cases
#
# Purpose
#   Reads the weekly Mpox case data, keeps 2023 onward, defines the 11
#   jurisdictions used in the analysis (San Diego County plus the candidate
#   predictor jurisdictions), and plots weekly cases for each.
#
# Needs
#   DATA_FOLDER : folder containing data_mpox.xlsx. Define it in config_local.R
#                 (not committed to GitHub; see below).
#   Packages    : loaded from libs_required.R
#   Function    : PLOT_WEEKLY_CASES() (defined elsewhere in the repository)
#
# Setup
#   Create a file called config_local.R in the repository folder containing one
#   line, for example:
#     DATA_FOLDER <- "C:/path/to/your/data/folder/"
#   and add config_local.R to .gitignore so the path is not shared.
#
# Outputs
#   data_mpox  : full weekly case data
#   data_year  : data from YEAR_INTEREST onward (2023 training data + 2024 test)
#   list_jur   : the 11 jurisdictions (SanDiego is the forecast target)
#   data_model : data_year restricted to list_jur
#   outputs/mpox_weekly_cases.png : weekly cases by jurisdiction
# =============================================================================

#**************************************************
# 0. PACKAGES AND LOCAL SETTINGS

source("libs_required.R")

if (!file.exists("config_local.R")) {
  stop("Create config_local.R defining DATA_FOLDER ",
       "(the folder containing data_mpox.xlsx). See the header of this file.")
}
source("config_local.R")


#**************************************************
# 1. GET DATA

file_name <- "data_mpox.xlsx"
data_mpox <- readxl::read_excel(path = file.path(DATA_FOLDER, file_name))

# Keep 2023 onward (2022 is excluded because of the atypical 2022 outbreak)
YEAR_INTEREST <- 2023
data_year     <- data_mpox %>% filter(Year >= YEAR_INTEREST)


#**************************************************
# 2. JURISDICTIONS

list_jur <- c("NewYorkCity", "LA", "Georgia", "Illinois", "NewJersey",
              "SanDiego", "Philadelphia", "Colorado", "Massachusetts",
              "Ohio", "NewYork")

data_model <- data_year %>% filter(Jurisdiction %in% list_jur)


#**************************************************
# 3. PLOT WEEKLY CASES

# single figure, 11 panels in 2 columns
p <- PLOT_WEEKLY_CASES(data_model, list_jur, n_col_plot = 2, break_every = 8)
p

dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/mpox_weekly_cases.png", p, width = 18, height = 14, dpi = 300)