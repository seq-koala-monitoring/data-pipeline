# Run data pipeline for inputs to the statistical model
rm(list=ls())

## 1. Install the R package and load libraries -----------------------------
#library(devtools)
#devtools::install_github('seq-koala-monitoring/data-pipeline') # only if needed
library(SEQKoalaDataPipeline)

## 2. Set global variables -------------------------------------------------
# Data directory
fcn_set_home_dir("C:\\Users\\uqfcho\\Documents/seq-koala-monitoring/working_data")

# Grid size (in meters) - default 100m
fcn_set_grid_size(100)

## 3. Retrieve grid generated as a raster file (for plotting if needed) ----
grid_raster <- fcn_get_grid()

## 4. Load the survey data as tables ---------------------------------------
master <- fcn_all_tables()

## 5. Load grid fractions as tables-----------------------------------------
grid_fractions <- fcn_all_transect_grid_fractions()

## 6. Load covariates from the directory -----------------------------------
# Check whether the directory for where covariates are stored is correct
print(fcn_get_raster_path()$covariates)

# If it is incorrect, specify the correct path
fcn_set_raster_path(list(covariates = 'final_covariates_output'))

# Specify the date intervals for covariate extraction
# Default: 6 month intervals from Oct 1994 to current time
dates <- fcn_date_intervals()

# Extract static covariates, with 6 and 12 month lags
cov_all <- fcn_cov_array(time_lag = c(6,12))


