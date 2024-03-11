#' Extract master databases from the scripts

#' @title Extract all tables
#' @export
fcn_all_tables <- function(table_names = c('line_transect', 'perp_distance', 'strip_transect', 'uaoa')) {
  master <- list()

  if ('line_transect' %in% table_names) master$line_transect = fcn_line_transect_table()
  if ('perp_distance' %in% table_names) master$perp_distance = fcn_perp_distance_all()
  if ('strip_transect' %in% table_names) master$strip_transect = fcn_strip_transect_all()
  if ('uaoa' %in% table_names) master$uaoa = fcn_all_of_area_all()

  master <- lapply(master, fcn_add_date_interval)
  return(master)
}

#' @title Extract all tables in SF format
#' @export
fcn_all_tables_sf <- function(table_names = c('line_transect', 'strip_transect', 'uaoa')) {
  master <- list()

  if ('line_transect' %in% table_names) master$line_transect = fcn_line_transect_sf_all()
  if ('strip_transect' %in% table_names) master$strip_transect = fcn_strip_transect_sf_all()
  if ('uaoa' %in% table_names) master$uaoa = fcn_all_of_area_sf_all()
  master <- lapply(master, fcn_add_date_interval)
  return(master)
}

#' @title Extract transect information with grid fractions
#' @param buffer: a vector of polygon buffer sizes to extract,
#' @param keep_all: if TRUE, keep all columns from transect. if FALSE, keeps only the TransectID, GridID and Fractions
#' @param grid_id_vec: if NULL, then keep all grids in the fishnet. Otherwise, specify a vector of GridID to keep. All grid fractions will still sum to 1 even if some grid cells are dropped.
#' @export
fcn_all_transect_grid_fractions <- function(buffer = c(0), keep_all = FALSE, grid_id_vec = NULL) {
  fishnet <- fcn_get_grid()
  master <- fcn_all_tables_sf()
  buffer <- sort(buffer)

  master_grid <- lapply(master, function(df) {
    res <- fcn_extract_raster_buffer(df, fishnet, 0)

    if (!is.null(grid_id_vec)) {
      # GridID vector specified: select only grid cells in the list and rescale weights to sum to 1
      res <- res %>%
        dplyr::filter(GridID %in% grid_id_vec) %>%
        dplyr::group_by(GridID) %>%
        dplyr::mutate(fraction = fraction / sum(fraction)) %>%
        dplyr::ungroup()
    }

    if (length(buffer) == 1) {
      var_name <- c('fraction')
    } else {
      var_name <- paste0('fraction_', buffer)
    }
    res <- res %>% dplyr::rename(!!var_name[1] := fraction)
    if (length(buffer) > 1) {
      for (i in 2:length(buffer)) {
          res_buffer <- fcn_extract_raster_buffer(df, fishnet, buffer[i]) %>%
            dplyr::rename(!!var_name[i] := fraction)
          res <- res %>%
            dplyr::select(TransectID, GridID, dplyr::all_of(var_name[1:(i-1)])) %>%
            dplyr::right_join(res_buffer, by = c('TransectID', 'GridID'))
        }
        # Fill up empty weights with zeros
        res <- res %>%
          dplyr::mutate_at(var_name, ~replace(., is.na(.), 0))
      }

    if (!keep_all) {
      res <- res %>%
        dplyr::select('TransectID', 'GridID', 'Date', dplyr::all_of(var_name))

      res <- fcn_add_date_interval(res)
    }
    return(res)
  })
  return(master_grid)
}

#' @title Extract raster after buffering polygons
fcn_extract_raster_buffer <- function(df, fishnet, buffer = 0) {
  if (buffer > 0) {
    df <- sf::st_buffer(df, dist = buffer, joinStyle = 'ROUND')
  }
  res <- fcn_mixed_extract_raster(fishnet, df)
  return(res)
}

#' @title Extract LGA information
fcn_get_lga <- function() {
  lga <- lapply(c('1996','2020'), function(x) fcn_sql_exec(x, 'lga')) %>%
    dplyr::bind_rows()
  return(lga)
}

fcn_sightings_table <- function() {
  all_tables <- fcn_all_tables(c('line_transect', 'strip_transect', 'uaoa'))
  all_tables %>%
    purrr::map(function(x) dplyr::select(x, c('TransectID', 'Number_Sightings'))) %>%
    dplyr::bind_rows(.id = 'Method')
}

fcn_summarise_sightings <- function(group_by_cols = c('LGA', 'Method', 'db')) {
  lga <- fcn_get_lga()
  all_tables <- fcn_all_tables(c('line_transect', 'strip_transect', 'uaoa'))
  dots <- lapply(group_by_cols, as.symbol)

  all_tables %>%
    purrr::map(function(x) dplyr::select(x, c('TransectID', 'Number_Sightings', 'db'))) %>%
    dplyr::bind_rows(.id = 'Method') %>%
    dplyr::left_join(lga, by = 'TransectID') %>%
    dplyr::group_by(.dots = dots) %>%
    summarise(Sightings = sum(Number_Sightings))
}

#' @title Remove observations where the observations cannot be identified uniquely, and warn accordingly
#' @export
fcn_keep_distinct <- function(df, cols = c('SiteNumber', 'TransectNumber', 'SurveyNumber')) {
  df_unite <- tidyr::unite(df, value, cols, remove = FALSE)
  if (anyDuplicated(df_unite$value)) {
    warning(sprintf("%i records cannot be uniquely identified with %s. Keeping only distinct records.",

                           sum(duplicated(df_unite$value)), paste0(cols, collapse = '-')))
    df <- df_unite %>%
      dplyr::distinct(value, .keep_all = TRUE) %>%
      dplyr::select(-value)
  }
  return(df)
}

#' @title Join transect grid with covariate based on GridID
#' @export
fcn_transect_grid_covariates <- function(transect_grid, cov = NULL) {
  cov_df <- fcn_cov_grid_df(cov)
  res <- lapply(transect_grid, function(df) {
    if ('GridID' %in% colnames(df)) {
      res <- dplyr::left_join(df, cov_df, by = 'GridID')
      return(res)
    } else {
      return(df)
    }
  })
  return(res)
}

#' @title Update covariate table to match date
#' @export
fcn_master_covariate_df_match_date <- function(transect_grid, chunk_size = 10000) {
  res <- lapply(transect_grid[c('line_transect','strip_transect', 'uaoa')], function(df) {
    # Break up large dataframes for memory requirements
    nrow_df <- nrow(df)
    rows_processed <- 0
    for (i in 1:ceiling(nrow_df/chunk_size)) {
      e <- fcn_covariate_df_match_date(df[rows_processed:min(nrow_df,(rows_processed+chunk_size)),])
      if (i==1) {
        df_out <- e
      } else {
        rbind(df_out,e)
      }
      rows_processed <- min(nrow_df,(rows_processed+chunk_size))
      print(sprintf("Progress: %s percent", round(100*rows_processed/ nrow_df, digits = 0)))
    }
    return(df_out)
  })
  names(res) <- c('line_transect','strip_transect', 'uaoa')
  res$perp_distance <- transect_grid$perp_distance
  return(res)
}

