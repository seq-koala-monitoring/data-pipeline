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





