# SETUP VARIABLES

working_data_dir <- r"(H:\seq-koala-monitor\working_data)"

# Home directory
fcn_set_home_dir(working_data_dir)

## Set db path
fcn_set_db_path(list(
  `1996` = 'SEQkoalaData.accdb',
  `2015` = '2015-2019 SEQKoalaDatabase DES_20231027.accdb',
  `2020` = 'KoalaSurveyData2020_cur.accdb',
  `integrated` = '1996 - 2023 SEQKoalaDatabase DES_Modelling_v240517.accdb'
))

# Set gdb path
fcn_set_gdb_path(list(
  koala_survey_data="KoalaSurveyData.gdb",
  total_db="Database_Spatial_v240517/Total_v240517.shp",
  koala_survey_sites="KoalaSurveySites_231108/KoalaSurveySites_231108.shp"
))

# Grid size (in meters) - default 100m
primary_grid_size <- 500
secondary_grid_size <- primary_grid_size*10

fcn_set_grid_size(primary_grid_size)

# Set line transect buffer width in meters (if generating transects using start and end coordinate information)
fcn_set_line_transect_buffer(28.7)

# Set covariate impute buffer distance (within the data pipeline)
fcn_set_cov_impute_buffer(0)

# Set study area buffer
fcn_set_study_area_buffer(0)

# If it is incorrect, specify the correct path
use_imputation <- FALSE

if (use_imputation) {
  fcn_set_raster_path(list(covariates = 'covariates_impute/output'))
} else {
  fcn_set_raster_path(list(covariates = 'covariates/output'))
}

# Run in parallel (requires RStudio API if true)
use_parallel <- TRUE

# Output path
state <- fcn_get_state()
target_dir <- r"(H:\seq-koala-monitor\output)"
current_date <- format(Sys.Date(), format="%Y%m%d")

out_dir <- paste0(target_dir, '\\', paste0(current_date, "_", state$grid_size, ifelse(use_imputation, '_1500', '')))

if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(paste0(out_dir, '\\cov_raster'))) dir.create(paste0(out_dir, '\\cov_raster'))
if (!dir.exists(paste0(out_dir, '\\cov_csv'))) dir.create(paste0(out_dir, '\\cov_csv'))

# Run covariate extraction algorithm; if not, read from disc in the output folder (computationally intensive, 30 minute run)
run_cov_extraction <- TRUE

## 3. Retrieve grid generated as a raster file (for plotting if needed) ----
# Check whether the directory for where covariates are stored is correct
print(fcn_get_raster_path()$covariates)

# Set genetic population feature class file (relative to working directory)
gen_pop_file_path <- "genetic_populations/Population_boundaries_v2.shp"

