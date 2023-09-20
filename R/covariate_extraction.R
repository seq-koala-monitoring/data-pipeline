#' Covariate extraction functions

#' @title List all layers in covariate database
#' @export
fcn_list_covariate_layers <- function() {
  covariate_filepath <- fcn_get_raster_path()$covariates
  rastlist <- list.files(path = covariate_filepath, pattern='.tif$', all.files= T)
  return(rastlist)
}

#' @title List constant covariate layers
#' @export
fcn_list_covariate_layers_constant <- function() {
  rastList <- fcn_list_covariate_layers()
  return(rastList[grep("^[[:alnum:]]{5}\\.tif$", rastList)])
}

#' @title List temporally-variant covariate layers
#' @export
fcn_list_covariate_layers_temporal <- function() {
  rastList <- fcn_list_covariate_layers()
  return(rastList[grep("^[[:alnum:]]{5}_\\d{6}\\.tif$", rastList)])
}

#' @title List covariate layers as a df
#' @export
fcn_covariate_layer_df <- function(layer = NULL) {
  constant_covariates <- data.frame(filename = fcn_list_covariate_layers_constant())
  temporal_covariates <- data.frame(filename = fcn_list_covariate_layers_temporal())
  df <- dplyr::bind_rows(list(constant = constant_covariates, temporal = temporal_covariates), .id = 'type') %>%
    dplyr::mutate(name = substr(filename, 1, 5)) %>%
    dplyr::mutate(date = as.numeric(gsub("\\D", "", filename))) %>%
    dplyr::mutate(date = ifelse(is.na(date), NA, paste0("X", date)))

  if (!is.null(layer)) {
    df <- df %>%
      filter(name == layer)
  }

  return(df)
}

#' Load covariate layer to SpatRaster by name
#' @param covariate name of covariate (string)
#' @export
fcn_covariate_raster_load <- function(covariate = "htele") {
  raster_path <- fcn_get_raster_path()$covariates
  covariate_df <- fcn_covariate_layer_df() %>%
    dplyr::filter(name == covariate)
  covariate_files <- purrr::map_chr(covariate_df$filename, function(x) file.path(raster_path, x))
  if (length(covariate_files)>1) {
    covariate_names <- covariate_df$date
  } else {
    covariate_names <- covariate
  }

  covariate_raster <- fcn_covariate_read_raster(covariate_files, covariate_names)
  covariate_raster
}

#' @title Read raster
#' @export
fcn_covariate_read_raster <- function(covariate_files, covariate_names = NA, project = T, resample_to_grid = T) {

  state <- fcn_get_state()
  covariate_raster <- terra::rast(covariate_files)

  if (resample_to_grid) {
    # Resample to fishnet grid
    grid <- fcn_get_grid()
    if (class(fcn_new_grid())[1] != "SpatRaster") {
      grid <- fcn_new_grid(option = 'raster') # Covariate extraction with resampling only works when fishnet grid is raster
    }

    # Allowing default resampling - nearest if categorical, bilinear if continuous
    covariate_raster <- terra::resample(covariate_raster, grid)
  }

  if (all(!is.na(covariate_names) & (length(names(covariate_raster)) == length(covariate_names)))) {
    names(covariate_raster) <- covariate_names
  }
  if (project) {
    covariate_raster <- fcn_project_raster(covariate_raster)
  }
  covariate_raster
}

#' @title Project raster layer to CRS as specified in the environment
#' @param raster a `SpatRaster` object to be projected
fcn_project_raster <- function(raster) {
  state <- fcn_get_state()
  crs <- state$crs
  sf::sf_proj_network(TRUE)
  projected_raster <- terra::project(raster, paste0("EPSG:", crs))
  return(projected_raster)
}

#' @title Get covariate value from a spatio-temporal raster
#' @export
fcn_covariate_match_date <- function(route_table, col_names, method = 'bilinear') {
  # Date difference matrix between transect survey dates with route table layer dates
  date_diff_mat <- fcn_date_diff_matrix(as.Date(route_table$Date), fcn_ym_to_date(col_names))
  if (method == "bilinear") {
    weights <- fcn_date_bilinear_interpolation_date(date_diff_mat)
  } else {
    weights <- fcn_date_weights_nearest_date(date_diff_mat)
  }
  event_matrix <- route_table[,col_names] %>% as.matrix()
  route_table$value <- Matrix::rowSums(event_matrix * weights) %>% as.vector()
  return(route_table)
}

#' @title Extract covariates with mixed multipolygon/ linestring rows
#' @export
fcn_mixed_extract_raster <- function(input_raster, df) {

  df_no_geom <- sf::st_drop_geometry(df)
  fcn_join_to_original <- function(out) {
    out_select <- dplyr::select(out, TransectID, dplyr::all_of(names(input_raster)), fraction)
    dplyr::left_join(df_no_geom, out, by = 'TransectID')
  }

  fcn_check_transect_id_unique(df)

  df_geom_type <- sf::st_geometry_type(df)

  all_linestrings <- all(df_geom_type == 'LINESTRING')
  all_polygons <- all(df_geom_type != 'LINESTRING')

  if (!all_polygons) {
    # Handle linestrings with route table functions
    df_linestrings <- df[df_geom_type == 'LINESTRING',]
    route_table <- fcn_route_table_raster(input_raster, df_linestrings)
    df_linestring_extracted <- route_table %>%
      dplyr::select(TransectID, Date, dplyr::all_of(names(input_raster)), lpercent) %>%
      dplyr::rename(fraction = lpercent)
  }

  if (all_linestrings) {
    df_out <- fcn_join_to_original(df_linestring_extracted)
    return(df_out)
  }

  df_polygons <- df[df_geom_type != 'LINESTRING',]
  polygon_table <- fcn_polygon_extract_raster(input_raster, df_polygons)
  df_polygon_extracted <- polygon_table %>%
    dplyr::select(TransectID, Date, dplyr::all_of(names(input_raster)), fraction)

  if (all_polygons) {
    return(df_polygon_extracted %>% fcn_join_to_original())
  }

  if (all(names(df_linestring_extracted) == names(df_polygon_extracted))) {
    combined_table <- rbind(df_linestring_extracted, df_polygon_extracted)
    return(combined_table %>% fcn_join_to_original())
  } else {
    stop("Column names of the dataframes do not match.")
  }
}
