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
  return(rastList[grep(".*\\d{6}.tif$", rastList, invert =T)])
}

#' @title List temporally-variant covariate layers
#' @export
fcn_list_covariate_layers_temporal <- function() {
  rastList <- fcn_list_covariate_layers()
  return(rastList[grep(".*\\d{6}.tif$", rastList)])
}

#' @title Extract name from string containing dates
fcn_get_covariate_name <- function(input) {
  pattern <- ".*\\d{6}.tif$"
  if (grepl(pattern, input)) {
    output <- gsub("\\d{6}.tif$", "", input)
  } else {
    output <- gsub(".tif$", "", input)
  }
  return(output)
}


#' @title List covariate layers as a df
#' @export
fcn_covariate_layer_df <- function(layer = NULL) {
  state <- fcn_get_state()
  match_method <- state$covariate_time_match
  constant_covariates <- data.frame(filename = fcn_list_covariate_layers_constant())
  temporal_covariates <- data.frame(filename = fcn_list_covariate_layers_temporal())
  temporal_covariates <- temporal_covariates %>%
    dplyr::mutate(date = as.numeric(as.numeric(gsub(".*[^0-9]([0-9]{6})[^0-9]*", "\\1", filename)))) %>%
    dplyr::mutate(date = ifelse(is.na(date), NA, paste0("X", date))) %>%
    dplyr::mutate(fullname = sub('\\.tif$', '', filename))

  df <- dplyr::bind_rows(list(constant = constant_covariates,
                              temporal = temporal_covariates),
                         .id = 'type') %>%
    dplyr::mutate(name = sapply(filename, fcn_get_covariate_name)) %>%
    dplyr::mutate(match_method = match_method[name]) %>%
    dplyr::mutate(fullname = sub('\\.tif$', '', filename))

  # Exclude files that have a processing error, less than 100 bytes
  file_sizes <- file.info(paste0(state$home_dir, "\\", state$raster_path,'\\', df$filename))$size
  low_filesize <- 100
  if (any(file_sizes < low_filesize)) {
    invalid_files <- df$filename[file_sizes < low_filesize]
      warning(sprintf("Raster(s) %s appears to be less than 100bytes and therefore removed.", paste(invalid_files, collapse=", ")))
    df <- df[file_sizes > low_filesize,]
  }

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
  covariate_df <- fcn_get_covariate_df() %>%
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
    covariate_raster <- terra::resample(covariate_raster, grid, threads = TRUE)
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
  dates <- as.Date(route_table$Date)
  dates_unique <- unique(dates)
  #dates_unique_idx <- fcn_find_unique_idx(dates)

  date_diff_mat <- outer(dates_unique,  fcn_ym_to_date(col_names), function(a,b) b-a)
  #if (method == "bilinear") {
  #  weights <- fcn_date_bilinear_interpolation_date(date_diff_mat)
  #  weights_table <- weights[match(dates, dates_unique),]
  #  event_matrix <- route_table[,col_names] %>% as.matrix()
  #  route_table$value <- as.vector(rowSums(event_matrix * weights_table, na.rm = T))
  #} else {
    idx <- apply(date_diff_mat, 1, function(x) which.min(abs(x)))
    idx_table <- idx[match(dates, dates_unique)]
    event_matrix <- route_table[,col_names] %>% as.matrix()
    rownames(event_matrix) <- colnames(event_matrix) <- NULL
    route_table$value <- sapply(seq_along(idx_table), \(i) event_matrix[i, idx_table[i]])
  #}

  return(route_table)
}

#' @title Match date for a dataframe with transect Dates and multi-temporal covariate layers
#' @export
fcn_covariate_df_match_date <- function(df) {
  cov_info <- fcn_get_covariate_df()
  cov_temp <- cov_info %>%
    dplyr::filter(type == 'temporal')
  cov_temp_names <- unique(cov_temp$name)
  # Iterate through all temporal covariate names
  for (i in 1:length(cov_temp_names)) {
    cov_name_i <- cov_temp_names[i]
    cov_name_df_i <- cov_temp %>%
      dplyr::filter(name == cov_name_i)
    cov_name_list <- cov_name_df_i$fullname
    df_out <- fcn_covariate_match_date(df[, c('Date', cov_name_list)], cov_name_list, method = cov_name_df_i$match_method[1])
    df$value <- df_out$value
    rm(df_out)
    df <- df %>%
      dplyr::rename(!!cov_name_i := value) %>%
      dplyr::select(-dplyr::all_of(cov_name_list))
  }
  return(df)
}

#' @title Extract covariates with mixed multipolygon/ linestring rows
#' @export
fcn_mixed_extract_raster <- function(input_raster, df) {

  df_no_geom <- sf::st_drop_geometry(df)
  fcn_join_to_original <- function(out) {
    out_select <- dplyr::select(out, TransectID, dplyr::all_of(names(input_raster)), fraction)
    dplyr::left_join(df_no_geom, out_select, by = 'TransectID')
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

#' @title Extract all covariate by fishnet grid
#' filename_list: list of covariates by filenames, if unspecified extracts all files
#' @export
fcn_extract_covariate_grid <- function(filename_list = NULL) {
  fishnet <- fcn_get_grid()
  covariates <- fcn_get_covariate_df()
  if (is.null(filename_list)) filename_list <- covariates$filename
  study_area <- fcn_get_study_area()
  covariate_raster <- lapply(as.list(filename_list), function(n) {
    cov <- fcn_get_cov_raster(n)
    if (!is.null(cov)) {
      return(cov)
    }
    print(sprintf("Reading raster: %s", n))
    path <- paste0(fcn_get_raster_path()$covariates, '\\', n)
    r <- terra::rast(path)
    resampled <- terra::resample(r, fishnet, method = 'mode', threads = TRUE)
    resampled <- terra::clamp(resampled, lower = terra::minmax(r)[1], upper = terra::minmax(r)[2], values = F)
    fcn_set_cov_raster(resampled, n)
    return(resampled)
  })
  covariate_raster_resampled <- terra::rast(covariate_raster)
  covariate_raster_cropped <- terra::mask(covariate_raster_resampled, terra::vect(study_area))
  names(covariate_raster_cropped) <- sub(".tif$", "", filename_list)
  covariate_raster_combined <- c(fishnet, covariate_raster_cropped)
  return(covariate_raster_combined)
}

fcn_terra_get_nodata_value <- function(path) {
  v <- grep("NoData Value", terra::describe(path), value=TRUE)
  strsplit(v, "=")[[1]][2] %>% as.numeric()
}

#' @title Extract covariate in data frame format with GridID
#' @param cov: the output of fcn_extract_covariate_grid
#' @export
fcn_cov_grid_df <- function(cov = NULL, buffer = c(0)) {
  if (is.null(cov)) {
    cov <- fcn_get_cov_raster()
  }
  cell_with_values <- is.na(cov[['GridID']][]) == F
  res_list <- lapply(buffer, function(b) {
    if (b > 0) {
      cov_b <- terra::focal(cov, w = ceiling(b/state$grid_size), fun=sum, na.rm=TRUE)
    } else {
      cov_b <- cov
    }

    # Extract raw values from raster
    df <- cov_b[cell_with_values] %>%
      as.data.frame()
    colnames(df) <- names(cov)
    df <- df %>%
      dplyr::select('GridID', dplyr::everything())
    if (b > 0) {
      colnames(df)[2:length(colnames(df))] <- paste0(colnames(df)[2:length(colnames(df))], '_', b)
    }
    return(df)
  })

  df <- purrr::reduce(res_list, dplyr::inner_join, by = 'GridID')

  return(df)
}

#' Get all covariates as raster stacks
#' @export
fcn_covariate_raster_stack <- function(date_interval = NULL, by_date = TRUE) {
  cov_raster_list <- list()
  cov_df <- fcn_covariate_layer_df()
  if (is.null(date_interval)) date_interval <- fcn_get_date_intervals()
  cov_constant <- cov_df %>%
    dplyr::filter(type == 'constant')
  cov_temporal <- cov_df %>%
    dplyr::filter(type == 'temporal')
  cov_constant_name <- unique(cov_constant$filename)
  cov_constant_raster <- fcn_extract_covariate_grid(cov_constant_name)

  cov_temporal_name <- unique(cov_temporal$name)
  if (by_date) {
    for (d in date_interval) {
      cov_d <- lapply(cov_temporal_name, \(x) fcn_covariate_interval_mean(d, x, get_df = FALSE)[[x]])
      cov_raster_list[d$id] <- c(cov_constant_raster, terra::rast(cov_d))
    }
  } else {
    for (cov in cov_temporal_name) {
      cov_raster_list[cov] <- fcn_covariate_temporal_raster_stack(cov, date_interval)
    }
    cov_raster_list$constant <- cov_constant_raster
  }

  return(cov_raster_list)
}

#' Get temporal covariates as raster stacks
fcn_covariate_temporal_raster_stack <- function(cov_name = NULL, date_interval = NULL) {
  if (is.null(date_interval)) {
    date_interval <- fcn_get_date_intervals()
  }
  rasters <- lapply(date_interval, function(x) {
    r <- fcn_covariate_interval_mean(x, cov_name, get_df = FALSE)
    return(r[[1]]) # Return the layer without GridID
  })
  return(rasters)
}
