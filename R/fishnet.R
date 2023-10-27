#' Generate grid fishnet with an input spatial feature class
#'
#' @param feature_class Feature class (in 'sf' format) for fishnet creation
#' @param grid_size Grid size in km

#' Generate a fishnet grid with the input feature_class
#' @param feature_class An SF object of the spatial extent of the fishnet
#' @export
fcn_fishnet <- function(feature_class) {
  state <- fcn_get_state()
  grid_size <- state$grid_size
  fishnet <- sf::st_make_grid(feature_class, cellsize = state$grid_size, crs = state$crs) %>%
    sf::st_sf()
  #fishnet <- sf::st_sf(fishnet[feature_class])
  #fishnet_intersect <- dplyr::mutate(fishnet_intersect, GridID = paste(grid_size, 1:nrow(fishnet_intersect), sep = '_'))
  fishnet_intersect <- dplyr::mutate(fishnet, GridID = paste(grid_size, 1:nrow(fishnet), sep = '_'))
  return(fishnet_intersect)
}

fcn_fishnet_raster <- function(feature_class) {
  state <- fcn_get_state()
  grid_size <- state$grid_size
  feature_class_vect <- terra::vect(feature_class)
  fishnet <- terra::rast(feature_class_vect, res = grid_size, vals = 1)
  names(fishnet) <- "GridID"
  fishnet_masked <- terra::mask(fishnet, feature_class_vect)
  grid_id <- 1:length(fishnet_masked[fishnet_masked==1])
  fishnet_masked[fishnet_masked == 1] <- grid_id
  return(fishnet_masked)
}

#' @title Generate new grid based on grid_size
#' @param option: 'raster', or 'vector', denotes the format of the fishnet grid
#' @param buffer: buffer around the study area for grid generation, in meters
#' @export
fcn_new_grid <- function(option = 'raster', buffer = 0) {
  study_area <- fcn_get_study_area()
  if (buffer > 0) {
    study_area <- sf::st_buffer(study_area, buffer)
  }

  if (option == 'raster') {
    fishnet <- fcn_fishnet_raster(study_area)
  } else {
    fishnet <- fcn_fishnet(study_area)
  }
  fcn_set_grid(fishnet)
  return(fishnet)
}
