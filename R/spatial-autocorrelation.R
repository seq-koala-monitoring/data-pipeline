#' Generate adjacency matrix for spatial autocorrelation using a secondary grid, and lookup table from primary grid to secondary grid
#' @param secondary_grid_size: dimensions of the secondary grid, in m
#' @param directions: either "queen" or "rook", for calculating the contiguity weights matrix
#' @export
fcn_adj_matrix <- function(secondary_grid_size = 2000, directions = "queen") {
  grid_raster <- fcn_get_grid()
  study_area <- fcn_get_study_area() %>%
    sf::st_buffer(2000)
  study_area_vect <- terra::vect(study_area)

  # Generate secondary grid
  grid_raster_sp <- fcn_fishnet_raster(study_area, secondary_grid_size, T)
  names(grid_raster_sp) <- "SpGridID"
  grid_raster_comb <- c(grid_raster, terra::resample(grid_raster_sp, grid_raster, method = 'near'))
  grid_raster_lookup <- grid_raster_comb[]
  colnames(grid_raster_lookup) <- names(grid_raster_comb)
  # Select only non-NA values in GridID
  grid_raster_lookup <- as.data.frame(grid_raster_lookup) %>%
    dplyr::filter(!is.na(GridID))
  all_included <- sum(is.na(grid_raster_lookup[!is.na(grid_raster_lookup[,1]),2])) == 0
  if (!all_included) {
    stop("Larger grid does not fully cover the smaller grid")
  }

  # Keep only rows with non-NA in GridID
  grid_raster_lookup <- grid_raster_lookup[!is.na(grid_raster_lookup[,1]),]

  N <- length(grid_raster_sp[])
  adj <- terra::adjacent(grid_raster_sp, 1:N, directions = directions, pairs = TRUE, include = FALSE)
  adj_df <- as.data.frame(adj)
  adj_nested_df <- adj_df %>% dplyr::nest_by(from)
  neighborList <- adj_nested_df$data %>%
    lapply(\(x) x$to)
  weightList <- neighborList %>%
    lapply(\(x) rep(1, length(x)))
  adjacencyList <- nimble::as.carAdjacency(neighborList, weightList)

  out <- list(
    adjacencyList = adjacencyList,
    grid_lookup = grid_raster_lookup,
    grid_raster_comb = grid_raster_comb
  )
  return(out)
}

