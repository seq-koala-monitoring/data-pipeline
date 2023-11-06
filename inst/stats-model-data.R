# Run data pipeline for inputs to the statistical model
rm(list=ls())

## 1. Install the R package and load libraries -----------------------------
#library(devtools)
#devtools::install_github('seq-koala-monitoring/data-pipeline') # only if needed
#library(SEQKoalaDataPipeline)

## 2. Set global variables -------------------------------------------------
# Data directory
fcn_set_home_dir("N:/seq-koala-monitoring/working_data")

## Set db path
fcn_set_db_path(list(
  `1996` = 'SEQkoalaData.accdb',
  `2015` = '2015-2019 SEQKoalaDatabase DES_20231027.accdb',
  `2020` = 'KoalaSurveyData2020_cur.accdb'
))

# Grid size (in meters) - default 100m
fcn_set_grid_size(100)

# Output path
out_dir <- "M:\\Users\\uqfcho\\Documents\\seq-koala-monitoring\\output"

## 3. Retrieve grid generated as a raster file (for plotting if needed) ----
grid_raster <- fcn_get_grid()
terra::writeRaster(grid_raster, paste0(out_dir, "\\grid_raster.tif"), overwrite = T)

## 4. Load the survey data as tables ---------------------------------------
master <- fcn_all_tables()
saveRDS(master, paste0(out_dir, '\\master.rds'))

## 5. Load grid fractions as tables-----------------------------------------
grid_fractions <- fcn_all_transect_grid_fractions()
grid_fractions_comb <- dplyr::bind_rows(grid_fractions, .id = 'transect')
saveRDS(grid_fractions_comb, paste0(out_dir, '\\grid_fractions.rds'))
data.table::fwrite(grid_fractions_comb, paste0(out_dir, "\\grid_fractions.csv"))

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
cov_temporal_array <- abind::abind(cov_all$cov_temporal, along=3)
saveRDS(cov_all, paste0(out_dir, '\\covariates.rds'))
saveRDS(cov_all$cov_constant, paste0(out_dir, "\\cov_constant_array.rds"))
saveRDS(cov_temporal_array, paste0(out_dir, '\\cov_temporal_array.rds'))

# Save each time slice of the cov_temporal array in a separate CSV in case it needs to be loaded in chunks
purrr::map(seq_along(cov_all$cov_temporal), \(x) write.csv(cov_all$cov_temporal[[x]], paste0(out_dir, '\\cov_csv\\cov_', cov_all$cov_temporal[x], '.csv')))
