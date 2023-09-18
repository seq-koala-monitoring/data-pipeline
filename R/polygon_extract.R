#' Methods for extracting raster values from polygon

fcn_polygon_extract_raster <- function(input_raster, polygons) {
  # Convert to SpatVector
  vect_polygons <- terra::vect(polygons)

  # Extract values from input_raster
  values <- terra::extract(input_raster, vect_polygons, exact = T)

  # Merge back to the dataframe, into a long dataframe
  df <- polygons %>%
    dplyr::mutate(ID = 1:nrow(polygons)) %>%
    dplyr::left_join(values, by = 'ID') %>%
    dplyr::select(-ID)

  return(df)
}
