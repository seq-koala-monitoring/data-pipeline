# Run data pipeline for inputs to the statistical model
rm(list=ls())
gc()

## 1. Install the R package and load libraries -----------------------------
#library(devtools)
#devtools::install_github('seq-koala-monitoring/data-pipeline') # only if needed
library(SEQKoalaDataPipeline)
library(readr)
library(future)

## 2. Set global variables -------------------------------------------------
source('inst/set_params.R')

# Write to grid
grid_raster <- fcn_get_grid()
terra::writeRaster(grid_raster, paste0(out_dir, "\\grid_raster.tif"), overwrite = T)

## 4. Load the survey data as tables ---------------------------------------
master <- fcn_all_tables()
saveRDS(master, paste0(out_dir, '\\master.rds'))

master_sf <- fcn_all_tables_sf()
lapply(seq_along(master_sf), \(i) sf::st_write(master_sf[[i]], paste0(out_dir, '\\master_', names(master_sf)[i], '.shp'), append=F))

## 5. Load covariates from the directory -----------------------------------

# Extract covariates
if (run_cov_extraction) {
  source('inst/cov_temporal_parallel.R')
  source('inst/cov_temporal_array.R')
  #cov_all <- fcn_cov_array(write_path = paste0(out_dir, "")) # Writes the outputs to the output folder
  #cov_constant_array <- cov_all$cov_constant
  #cov_temporal_array <- cov_all$cov_temporal
  #readr::write_rds(cov_constant_array, paste0(out_dir, "\\cov_constant_array.rds"))
  #readr::write_rds(cov_temporal_array, paste0(out_dir, "\\cov_temporal_array.rds"))
}

## Run cov_temporal_array on a HPC (or a computer with 128GB RAM) here----------------

## Save cov_temporal_array in a separate file here ---

# Extract and save only those in surveylocations as a separate file
# Read results back from disk (output folder)
cov_constant_array <- readr::read_rds(paste0(out_dir, "\\cov_constant_array.rds"))
cov_temporal_array <- readr::read_rds(paste0(out_dir, "\\cov_temporal_array.rds"))

cov_temporal_array <- fcn_impute_temporal_cov(cov_temporal_array)

## 6. Load grid fractions as tables-----------------------------------------
grid_fractions <- fcn_all_transect_grid_fractions()
grid_fractions_comb <- dplyr::bind_rows(grid_fractions, .id = 'transect')
readr::write_rds(grid_fractions_comb, paste0(out_dir, '\\grid_fractions.rds'))
data.table::fwrite(grid_fractions_comb, paste0(out_dir, "\\grid_fractions.csv"))

## 7. Save grid cells in survey locations separately-----------------------------
cov_constant_array_surveylocations <- cov_constant_array[cov_constant_array[,1] %in% grid_fractions_comb$GridID,,]
cov_temporal_array_surveylocations <- cov_temporal_array[cov_temporal_array[,1,1] %in% grid_fractions_comb$GridID,,]

readr::write_rds(cov_constant_array_surveylocations, paste0(out_dir, "\\cov_constant_array_surveylocations.rds"))
readr::write_rds(cov_temporal_array_surveylocations, paste0(out_dir, '\\cov_temporal_array_surveylocations.rds'))

## 8. Resave grid fractions for the subset with complete covariate information -------
grid_id_non_na <- fcn_complete_grid_id(cov_constant_array, cov_temporal_array) # Get GridID index with non-NA covariates
cov_constant_array$GridID[!(cov_constant_array$GridID %in% grid_id_non_na)]

grid_fractions_complete_cov <- fcn_all_transect_grid_fractions(grid_id_vec = grid_id_non_na)
grid_fractions_comb_complete_cov <- dplyr::bind_rows(grid_fractions_complete_cov, .id = 'transect')
readr::write_rds(grid_fractions_comb_complete_cov, paste0(out_dir, '\\grid_fractions_complete_cov.rds'))
data.table::fwrite(grid_fractions_comb_complete_cov, paste0(out_dir, "\\grid_fractions_complete_cov.csv"))

transects_missing <- unique(grid_fractions_comb$TransectID)[(!(unique(grid_fractions_comb$TransectID) %in% unique(grid_fractions_comb_complete_cov$TransectID)))]

print(transects_missing)

GridID_missing <- grid_fractions_comb[grid_fractions_comb$TransectID %in% transects_missing,]$GridID

## 7. Save grid cells in survey locations separately with non-NA covariate information-----------------------------
cov_constant_array_surveylocations <- cov_constant_array[cov_constant_array[,1] %in% grid_fractions_comb$GridID ,,]
cov_temporal_array_surveylocations <- cov_temporal_array[cov_temporal_array[,1,1] %in% grid_fractions_comb$GridID,,]

# Save full arrays
cov_constant_array[cov_constant_array[,1] %in% grid_id_non_na,,] %>%
  readr::write_rds(paste0(out_dir, '\\cov_constant_array_complete_cov.rds'))
cov_temporal_array[cov_temporal_array[,1,1] %in% grid_id_non_na,,] %>%
  readr::write_rds(paste0(out_dir, '\\cov_temporal_array_complete_cov.rds'))

# Check for complete cases (any missing values)
fcn_complete_cases_check(cov_constant_array_surveylocations)
fcn_complete_cases_check(cov_temporal_array_surveylocations)

readr::write_rds(cov_constant_array_surveylocations, paste0(out_dir, "\\cov_constant_array_complete_cov_surveylocations.rds"))
readr::write_rds(cov_temporal_array_surveylocations, paste0(out_dir, '\\cov_temporal_array_complete_cov_surveylocations.rds'))

## 9. Produce date interval lookup table -------------------------------
write.csv(fcn_date_interval_lookup(), paste0(out_dir, "\\date_interval_lookup.csv"))
cov_layer_df <- fcn_covariate_layer_df()
write.csv(cov_layer_df[,1:5], paste0(out_dir, '\\covariate_info.csv'))

## 10. Produce and save the adjacency matrix
adj_data <- fcn_adj_matrix(secondary_grid_size = 5000)
saveRDS(adj_data, paste0(out_dir, "\\adj_data_queen.rds"))

adj_data <- fcn_adj_matrix(directions = 'rook')
saveRDS(adj_data, paste0(out_dir, "\\adj_data_rook.rds"))

## 11. Write covariates as RasterStack ----------------------------------
# Default: 6 month intervals from Oct 1994 to current time
if (run_cov_extraction) {
  library(future)
  library(future.apply)
  dates <- fcn_date_intervals()
  raster_stack_list <- fcn_covariate_raster_stack(dates, by_date = TRUE)

  plan(multisession)
  future_lapply(seq_along(raster_stack_list), function(i) {
    r <- raster_stack_list[[i]]
    terra::writeRaster(r, paste0(out_dir, '\\cov_raster\\cov_',names(raster_stack_list)[i] , '.tif'), overwrite = T)
    return()
  })
}

