#' Generate grid fishnet with an input spatial feature class
#'
#' @param feature_class Feature class (in 'sf' format) for fishnet creation
#' @param grid_size Grid size in km

#' Generate a fishnet grid with the input feature_class
#' @param feature_class An SF object of the spatial extent of the fishnet
#' @export
fcn_fishnet <- function(feature_class) {
  state <- fcn_get_state()
  fishnet <- sf::st_make_grid(feature_class, cellsize = state$grid_size, crs = state$crs)
  fishnet_intersect <- sf::st_sf(fishnet[feature_class])
  fishnet_intersect <- dplyr::mutate(fishnet_intersect, GridID = 1:nrow(fishnet_intersect))
  return(fishnet_intersect)
}

#' Generate new grid based on grid_size
#' @export
fcn_new_grid <- function() {
  study_area <- fcn_get_study_area()
  fishnet <- fcn_fishnet(study_area)
  fcn_set_grid(fishnet)
  return(fishnet)
}
