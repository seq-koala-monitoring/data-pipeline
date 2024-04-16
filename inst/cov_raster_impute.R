# Impute missing values in the covariates using the QUT HPC
library(terra)

## Raw covariate data directory
raw_cov_data <- "working_data/covariates/output"

## Imputed covariate data
imputed_cov_data <- "working_data/covariates_impute/output"

# Covariate layer dataframe
cov_layer_df <- read.csv("working_data/covariates_impute/cov_layer_df.csv")

impute_raster <- function(lyr, categorical){
  covariate_source <- "working_data/covariates/output"
  raster_file <- rast(file.path(covariate_source, lyr))
  any_missing <- any(is.na(raster_file[]))
  buffer_mat <- focalMat(raster_file, 1500, 'circle', fillNA=TRUE)
  out_path <- file.path(imputed_cov_data, lyr)
  if (any_missing) {
    print(out_path)
    if (categorical) {
      print("Processing as categorical variable")
      buffer_mat[!is.na(buffer_mat)] <- 1
      focal(raster_file, buffer_mat, na.policy = "only", filename = out_path, fun = 'modal', overwrite=T, na.rm = TRUE)
    } else {
      print("Processing as continuous variable")
      focal(raster_file, buffer_mat, na.policy = "only", filename = out_path, fun = 'mean', overwrite=T, na.rm = TRUE)
    }
  } else {
    writeRaster(raster_file, filename = out_path, overwrite = TRUE)
  }
}

args <- commandArgs(trailingOnly = TRUE)
id <- sum(as.numeric(args[1]) + 1)
if (!is.null(id)) {
  cov_layer <- cov_layer_df[id,]
  if (cov_layer$continuous_discrete == 'Continuous') {
    impute_raster(cov_layer$filename, FALSE)
  } else {
    impute_raster(cov_layer$filename, TRUE)
  }
} else {
  stop(paste("Argument not read properly:", args[1]))
}


