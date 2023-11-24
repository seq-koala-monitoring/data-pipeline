# Run data pipeline for inputs to the statistical model
rm(list=ls())

## 1. Install the R package and load libraries -----------------------------
#library(devtools)
#devtools::install_github('seq-koala-monitoring/data-pipeline') # only if needed
library(SEQKoalaDataPipeline)

## 2. Set global variables -------------------------------------------------
# Data directory
fcn_set_home_dir("M:\\Users\\uqfcho\\Documents\\seq-koala-monitoring\\working_data")

## Set db path
fcn_set_db_path(list(
  `1996` = 'SEQkoalaData.accdb',
  `2015` = '2015-2019 SEQKoalaDatabase DES_20231027.accdb',
  `2020` = 'KoalaSurveyData2020_cur.accdb'
))

# Grid size (in meters) - default 100m
fcn_set_grid_size(100)

# Output path
target_dir <- "M:\\Users\\uqfcho\\Documents\\seq-koala-monitoring\\output"
current_date <- format(Sys.Date(), format="%Y%m%d")
out_dir <- paste0(target_dir, '\\', current_date)
if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(paste0(out_dir, '\\cov_raster'))) dir.create(paste0(out_dir, '\\cov_raster'))
if (!dir.exists(paste0(out_dir, '\\cov_csv'))) dir.create(paste0(out_dir, '\\cov_csv'))

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

# Extract covariates
cov_all <- fcn_cov_array()
cov_constant_array <- cov_all$cov_constant
cov_temporal_array <- abind::abind(cov_all$cov_temporal, along=3)
#saveRDS(cov_all, paste0(out_dir, '\\covariates.rds'))
saveRDS(cov_constant_array, paste0(out_dir, "\\cov_constant_array.rds"))
saveRDS(cov_temporal_array, paste0(out_dir, '\\cov_temporal_array.rds'))

cov_constant_array_surveylocations <- cov_constant_array[cov_constant_array[,1] %in% grid_fractions_comb$GridID,,]
cov_temporal_array_surveylocations <- cov_temporal_array[cov_temporal_array[,1,1] %in% grid_fractions_comb$GridID,,]

saveRDS(cov_constant_array_surveylocations, paste0(out_dir, "\\cov_constant_array_surveylocations.rds"))
saveRDS(cov_temporal_array_surveylocations, paste0(out_dir, '\\cov_temporal_array_surveylocations.rds'))

## 7. Write covariates as RasterStack ----------------------------------
# Default: 6 month intervals from Oct 1994 to current time
dates <- fcn_date_intervals()
raster_stack_list <- fcn_covariate_raster_stack(dates, by_date = TRUE)
lapply(seq_along(raster_stack_list), function(i) {
  r <- raster_stack_list[[i]]
  terra::writeRaster(r, paste0(out_dir, '\\cov_raster\\cov_',names(raster_stack_list)[i] , '.tif'), overwrite = T)
  return()
})

## 8. Produce date interval lookup table -------------------------------
write.csv(fcn_date_interval_lookup(), paste0(out_dir, "\\date_interval_lookup.csv"))
cov_layer_df <- fcn_covariate_layer_df()
write.csv(cov_layer_df[,1:5], paste0(out_dir, '\\covariate_info.csv'))

## 9. Produce and save the adjacency matrix
adj_data <- fcn_adj_matrix()
saveRDS(adj_data, paste0(out_dir, "\\adj_data_queen.rds"))

adj_data <- fcn_adj_matrix(directions = 'rook')
saveRDS(adj_data, paste0(out_dir, "\\adj_data_rook.rds"))
