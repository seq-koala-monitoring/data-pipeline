#' Linear referencing / dynamic segmentation functions
#'

#' @param feature_class SF object of the grid
#' @param line_transect SF object of the line transect
#' @export
fcn_event_table <- function(feature_class, line_transect) {
  line_transect$length = sf::st_length(line_transect$geom)
  ldnc = sf::st_intersection(line_transect, feature_class)
  ldnc$lpercent = units::drop_units((sf::st_length(ldnc)/ldnc$length))
  event_table <- sf::st_drop_geometry(ldnc)
  return(event_table)
}

#' Get Event Table (linear referencing) for a series of linear transects
fcn_event_table_raster <- function(input_raster, line_transect, n = 50) {
  names(input_raster) <- 'value'
  sample_seq <- seq(0,1,length.out=n)
  reference_points <- sf::st_line_sample(line_transect, sample = sample_seq) %>% sf::st_cast("POINT") %>% sf::st_sf()
  reference_points$TransectID <- rep(line_transect$TransectID, each = n)
  reference_points$Tlength <- rep(line_transect$Tlength, each = n)
  reference_points <- reference_points %>%
    group_by(TransectID) %>%
    mutate(distance_from_start = Tlength * sample_seq[row_number()])
  extract_table <- cbind(reference_points, terra::extract(input_raster,reference_points))
  ## Remove points with same information as the previous column
  event_table <- extract_table %>%
    ungroup() %>%
    mutate(dup = value == dplyr::lag(value) & TransectID == dplyr::lag(TransectID)) %>%
    filter(!dup) %>%
    mutate(distance_to_end = ifelse(dplyr::lead(TransectID) == TransectID, dplyr::lead(distance_from_start), Tlength)) %>%
    mutate(segment_length = distance_to_end - distance_from_start) %>%
    dplyr::select(TransectID, Tlength, value, distance_from_start, distance_to_end, segment_length, geometry)
  return(event_table)
}
