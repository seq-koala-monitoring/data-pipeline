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
  state <- fcn_get_state()
  if (state$use_integrated_db) {
    db <- fcn_perp_distance_table_integrated()
    db <- fcn_db_to_date(db)
    return(db)
  }
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

#' @title Extract perpendicular distances for koala sightings for 2020 database
#' @export
fcn_perp_distance_table_integrated <- function() {
  table <- fcn_sql_exec('integrated', 'perp-distance')
  fcn_check_transect_id(table)
  return(table)
}

#' @title Check whether transect ID exists in line transect tables
#' @export
fcn_check_transect_id <- function(db, perp_dist = FALSE) {
  line_transect_table <- fcn_line_transect_table()
  comb_db <- line_transect_table %>%
    dplyr::select(TransectID)
  unmatched_rows <- dplyr::anti_join(db, comb_db, by = 'TransectID')
  if (nrow(unmatched_rows) > 0) {
    stop("TransectID not fully matched to TransectID in line transect table")
  }

  # Conduct additional checks if the table is a perpendicular distance table
  if (perp_dist) {
    fcn_check_perp_distances_in_line_transect(line_transect_table, db)
  }

  return()
}

#' @title Check if all sightings in the line transect is represented in the perpendicular transects table
fcn_check_perp_distances_in_line_transect <- function(line_transect_table, perp_dist_table) {
  # Perpendicular distance not in line transect table
  in_line_transect <- perp_dist_table$TransectID %in% line_transect_table$TransectID
  if (any(!in_line_transect)) {
    stop(sprintf("%s transects in perpendicular distances table not in line transect table"), sum(!in_line_transect))
  }

  # Check line transect IDs in perp distance table
  in_perp_distance <- line_transect_table[line_transect_table$Number_Sightings>0,"TransectID"] %in% perp_dist_table$TransectID
  if (any(!in_perp_distance)) {
    stop(sprintf("%s transects in line transects table not in perpendicular distances table"), sum(!in_line_transect))
  }
  return()
}
