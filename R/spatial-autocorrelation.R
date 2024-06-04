#' Generate adjacency matrix for spatial autocorrelation using a secondary grid, and lookup table from primary grid to secondary grid
#' @param secondary_grid_size: dimensions of the secondary grid, in m
#' @param directions: either "queen" or "rook", for calculating the contiguity weights matrix
#' @export
fcn_adj_matrix <- function(secondary_grid_size = 2000, directions = "queen") {
  state <- fcn_get_state()
  grid_raster <- fcn_get_grid()
  study_area <- fcn_get_study_area()
  grid_vec <- terra::as.polygons(grid_raster)
  grid_vec_centroid <- terra::centroids(grid_vec)
  grid_raster_sp <- fcn_fishnet_raster(study_area %>% sf::st_buffer(secondary_grid_size), secondary_grid_size, F)
  grid_vec_sp <- terra::as.polygons(grid_raster_sp)
  names(grid_vec_sp) <- "SpGridID_full"
  grid_intersection <- sf::st_intersects(sf::st_as_sf(grid_vec_centroid), sf::st_as_sf(grid_vec_sp))
  grid_lookup_sp <- data.frame(GridID = grid_vec_centroid[,1], SpGridID_full = sapply(grid_intersection, \(i) i[[1]]))

  # Re-index SpridID
  SpGridID_lookup <- data.frame(SpGridID_full = unique(grid_lookup_sp$SpGridID_full),
                         SpGridID = 1:length(unique(grid_lookup_sp$SpGridID_full)))
  grid_lookup_sp_joined <- grid_lookup_sp %>%
    dplyr::left_join(SpGridID_lookup, by = 'SpGridID_full')
  grid_raster_sp_conv <- terra::classify(grid_raster_sp, rcl = as.matrix(grid_lookup_sp_joined[,c(2,3)]), others = NA)
  grid_vec_sp_conv <- terra::merge(grid_vec_sp, SpGridID_lookup, by = 'SpGridID_full')[,c(2)]
  grid_vec_conv <- terra::merge(grid_vec, grid_lookup_sp_joined, by = 'GridID')[,c('GridID', 'SpGridID')]

  N <- grid_raster_sp_conv[] %>% length()
  adj <- terra::adjacent(grid_raster_sp_conv, 1:N, directions = directions, pairs = TRUE, include = FALSE)

  # Map cell index values to SpGridID values
  adj_mapped <- as.data.frame(adj)
  adj_mapped[,'from'] <- grid_raster_sp_conv[][adj[,'from']]
  adj_mapped[,'to'] <- grid_raster_sp_conv[][adj[,'to']]
  adj_mapped_filter <- adj_mapped %>%
    dplyr::filter(!is.na(from) & !is.na(to)) %>%
    dplyr::arrange(from, to)

  adj_df <- as.data.frame(adj_mapped_filter)
  adj_nested_df <- adj_df %>% dplyr::nest_by(from)
  neighborList <- adj_nested_df$data %>%
    lapply(\(x) x$to)
  weightList <- neighborList %>%
    lapply(\(x) rep(1, length(x)))
  adjacencyList <- nimble::as.carAdjacency(neighborList, weightList)

  out <- list(
    adjacencyList = adjacencyList,
    grid_lookup = grid_lookup_sp_joined[, c('GridID', 'SpGridID')],
    grid_raster_sp = grid_raster_sp_conv,
    grid_vec_sp = grid_vec_sp_conv
  )
  return(out)
}

