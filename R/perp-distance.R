#' Generate tables for koala sightings of perpendicular distances

fcn_perp_distance_table <- function(year) {
  if (!(year %in% c(1996,2020))) {
    stop("Year is invalid or not yet available.")
  }

  result <- switch(
    as.character(year),
    '1996' = fcn_perp_distance_table_1996(),
    '2020' = fcn_perp_distance_table_2020()
  )
}

#' @title Extract perpendicular distances for all databases
#' @export
fcn_perp_distance_all <- function() {
  db_1996 <- fcn_perp_distance_table_1996()
  db_2020 <- fcn_perp_distance_table_2020()
  out_db <- list(`1996-2015` = db_1996,
                 `2020-cur` = db_2020) %>%
    dplyr::bind_rows(.id = 'db')
  return(out_db)
}

#' @title Extract perpendicular distances for koala sightings for 1996 database
#' @export
fcn_perp_distance_table_1996 <- function() {
  table <- fcn_sql_exec('1996', 'perp-distance')
  fcn_check_transect_id(table)
  return(table)
}

#' @title Extract perpendicular distances for koala sightings for 2020 database
#' @export
fcn_perp_distance_table_2020 <- function() {
  table <- fcn_sql_exec('2020', 'perp-distance')
  fcn_check_transect_id(table)
  return(table)
}

#' @title Check whether transect ID exists in line transect tables
#' @export
fcn_check_transect_id <- function(db) {
  comb_db <- fcn_line_transect_table() %>%
    dplyr::select(TransectID)
  unmatched_rows <- dplyr::anti_join(db, comb_db, by = 'TransectID')
  if (nrow(unmatched_rows) > 0) {
    stop("TransectID not fully matched to TransectID in line transect table")
  }
  return()
}
