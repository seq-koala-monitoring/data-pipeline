#' Linear referencing / dynamic segmentation functions
#'
#'

#' @param grid SF object of the grid
#' @param line_transect SF object of the line transect
#' @export
fcn_event_table <- function(grid, line_transect) {
  line_transect$length = sf::st_length(line_transect$geom)
  ldnc = sf::st_intersection(line_transect, grid)
  ldnc$lpercent = units::drop_units((sf::st_length(ldnc)/ldnc$length))
  event_table <- sf::st_drop_geometry(ldnc)
  return(event_table)
}
