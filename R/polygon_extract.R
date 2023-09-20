#' Methods for extracting raster values from polygon

#' @title Extract raster values from polygon
#' @export
fcn_polygon_extract_raster <- function(input_raster, polygons) {
  # Convert to SpatVector
  #vect_polygons <- terra::vect(polygons)

  if (length(names(input_raster)) > 1) {
    warning(sprintf("Input raster has %s layers, only the first layer will be considered.", length(names(input_raster))))
  }

  # Check if transect id is unique
  fcn_check_transect_id_unique(polygons)

  # Extract values from input_raster
  extract_frac <- exactextractr::exact_extract(input_raster, polygons, fun= NULL, progress = FALSE)
  names(extract_frac) <- polygons$TransectID
  values <- extract_frac %>%
    lapply(function(x) {
      colnames(x) <- c(names(input_raster)[1], 'coverage_fraction')
      x %>%
        dplyr::mutate(coverage_fraction = coverage_fraction / sum(x$coverage_fraction))
    }) %>%
    dplyr::bind_rows(.id = 'TransectID') %>%
    dplyr::rename(fraction = coverage_fraction)

  # Merge back to the dataframe, into a long dataframe
  df <- polygons %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(values, by = 'TransectID', relationship = 'one-to-many')

  if ('TArea' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(area = TArea * fraction)
  }

  # Check if fractions sum up to 1
  fcn_check_fractions(df)

  return(df)
}
