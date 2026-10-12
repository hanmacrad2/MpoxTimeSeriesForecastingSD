#1. DATA SETUP 

#RUN TEST SET
library(VARLASSOMpox)
library(forecast)

#FIX!! 
FOLDER = "C:/Users/h2cra/OneDrive/Documents/GitHub/Mpox_VAR_LASSO_Automate/"
source(paste0(FOLDER, 'libs_required.R'))

#**************************************************
#1. GET DATA #FIX!! 
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_GOLDEN_CDC/"
file_name = 'data_mpox.xlsx' 
data_mpox = readxl::read_excel(path = paste0(DATA_FOLDER, file_name))

#YEAR
YEAR_INTEREST = 2023
data_year = data_mpox  %>% filter(Year >= YEAR_INTEREST)

#JURISDICTIONS
list_jur <- c("NewYorkCity", "LA", "Illinois", "Colorado", "SanDiego",
                  "Philadelphia", "Ohio", "Georgia", "NewJersey", "NewYork", "Massachusetts")

data_model = data_year %>% filter(Jurisdiction %in% list_jur)
