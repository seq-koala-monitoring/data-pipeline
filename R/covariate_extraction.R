#' Covariate extraction functions

#' List all layers in covariate database
#' @export
fcn_list_covariate_layers <- function() {
  covariate_filepath <- fcn_get_raster_path()$covariates
  rastlist <- list.files(path = covariate_filepath, pattern='.tif$', all.files= T)
  return(rastlist)
}

#' List constant covariate layers
#' @export
fcn_list_covariate_layers_constant <- function() {
  rastList <- fcn_list_covariate_layers()
  return(rastList[grep("^[[:alnum:]]{5}\\.tif$", rastList)])
}

#' List temporally-variant covariate layers
#' @export
fcn_list_covariate_layers_temporal <- function() {
  rastList <- fcn_list_covariate_layers()
  return(rastList[grep("^[[:alnum:]]{5}_\\d{6}\\.tif$", rastList)])
}

#' List covariate layers as a df
fcn_covariate_layer_df <- function(layer = NULL) {
  constant_covariates <- data.frame(filename = fcn_list_covariate_layers_constant())
  temporal_covariates <- data.frame(filename = fcn_list_covariate_layers_temporal())
  df <- dplyr::bind_rows(list(constant = constant_covariates, temporal = temporal_covariates), .id = 'type') %>%
    mutate(name = substr(filename, 1, 5)) %>%
    mutate(date = as.numeric(gsub("\\D", "", filename))) %>%
    mutate(date = as.Date(paste0(date, "15"), "%Y%m%d"))

  if (!is.null(layer)) {
    df <- df %>%
      filter(name == layer)
  }

  return(df)
}

#' Load covariate layer to SpatRaster by name
#' @param cov name of covariate (string)
fcn_covariate_raster_load <- function(covariate = "htele") {
  raster_path <- fcn_get_raster_path()$covariates
  covariate_df <- fcn_covariate_layer_df() %>%
    filter(name == covariate)
  covariate_files <- purrr::map_chr(covariate_df$filename, function(x) file.path(raster_path, x))
  covariate_names <- ifelse(length(covariate_files)<=1, covariate, covariate_df$date)
  covariate_raster <- fcn_covariate_read_raster(covariate_files, covariate_names)
  covariate_raster
}

fcn_covariate_read_raster <- function(covariate_files, covariate_names = NA, project = T) {
  covariate_raster <- terra::rast(covariate_files)
  if (!is.na(covariate_names) & (length(names(covariate_raster)) == length(covariate_names))) {
    names(covariate_raster) <- covariate_names
  }
  if (project) {
    covariate_raster <- fcn_project_raster(covariate_raster)
  }
  covariate_raster
}

#' Project raster layer to CRS as specified in the environment
#' @param raster a `SpatRaster` object to be projected
fcn_project_raster <- function(raster) {
  state <- fcn_get_state()
  crs <- state$crs
  sf::sf_proj_network(TRUE)
  projected_raster <- terra::project(raster, paste0("EPSG:", crs))
  return(projected_raster)
}
