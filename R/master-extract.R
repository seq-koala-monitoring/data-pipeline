#' Extract master databases from the scripts

#' @title Extract all tables
#' @export
fcn_all_tables <- function(table_names = c('line_transect', 'perp_distance', 'strip_transect', 'uaoa')) {
  master <- list()

  if ('line_transect' %in% table_names) master$line_transect = fcn_line_transect_table()
  if ('perp_distance' %in% table_names) master$perp_distance = fcn_perp_distance_all()
  if ('strip_transect' %in% table_names) master$strip_transect = fcn_strip_transect_all()
  if ('uaoa' %in% table_names) master$uaoa = fcn_all_of_area_all()

  return(master)
}

#' @title Extract all tables in SF format
#' @export
fcn_all_tables_sf <- function(table_names = c('line_transect', 'strip_transect', 'uaoa')) {
  master <- list()

  if ('line_transect' %in% table_names) master$line_transect = fcn_line_transect_sf_all()
  if ('strip_transect' %in% table_names) master$strip_transect = fcn_strip_transect_sf_all()
  if ('uaoa' %in% table_names) master$uaoa = fcn_all_of_area_sf_all()

  return(master)
}

#' @title Extract transect information with grid fractions
#' @export
fcn_all_transect_grid_fractions <- function(buffer = 0) {
  fishnet <- fcn_get_grid()
  master <- fcn_all_tables_sf()
  if (buffer > 0) {
    master <- lapply(master, function(x) sf::st_buffer(x, dist = buffer, joinStyle = 'ROUND'))
  }

  master_grid <- lapply(master, function(x) fcn_mixed_extract_raster(fishnet, x))
  return(master_grid)
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
