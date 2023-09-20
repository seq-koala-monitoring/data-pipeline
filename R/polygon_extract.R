#' Methods for extracting raster values from polygon

#' @title Extract raster values from polygon
#' @export
fcn_polygon_extract_raster <- function(input_raster, polygons) {
  # Convert to SpatVector
  #vect_polygons <- terra::vect(polygons)

  if (length(names(input_raster)) > 1) {
    warning(sprintf("Input raster has %s layers, only the first layer will be considered.", length(names(input_raster))))
  }

  # Extract values from input_raster
  extract_frac <- exactextractr::exact_extract(input_raster, polygons, fun= NULL, progress = T)
  unique_values <- colnames(extract_frac)
  frac_df <- data.frame(ID = rep(1:nrow(polygons), each = length(unique_values)),
                        value = rep(unique_values, times= nrow(polygons)),
                        fraction = c(t(extract_frac)))
  values <- frac_df %>%
    dplyr::filter(fraction > 0) %>%
    dplyr::mutate(value = substr(value, 6, nchar(value)-1))
  colnames(values) <- c("ID", names(input_raster)[1], "fraction")

  # Merge back to the dataframe, into a long dataframe
  df <- polygons %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(ID = 1:nrow(polygons)) %>%
    dplyr::left_join(values, by = 'ID', relationship = 'one-to-many') %>%
    dplyr::select(-ID)

  if ('TArea' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(area = TArea * fraction)
  }

  # Check if fractions sum up to 1
  fcn_check_fractions(df)

  return(df)
}
