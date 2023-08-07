#' Extract Line transect tables

#' Wrapper function for generating line transect tables
#' @export
fcn_line_transect_table <- function(year) {
  result <- switch(
    as.character(year),
    '1996' = fcn_line_transect_table_1996(),
    '2020' = fcn_line_transect_table_2020()
  )
}

#' @export
fcn_line_transect_table_1996 <- function() {
  return()
}

#' @export
fcn_line_transect_table_2020 <- function() {
  state <- fcn_get_state()
  query_path <- fs::path_package("sql", "2020/line-transect.sql", package='SEQKoalaDataPipeline')
  query <- readr::read_file(query_path)
  conn <- fcn_db_connect_table(2020)
  table <- RODBC::sqlQuery(conn, query, errors = T)
  fcn_db_disconnect(conn)
  return(table)
}

#' @export
fcn_line_transect_sf_2020 <- function() {
  table_2020 <- fcn_line_transect_table_2020()
  line_transect_sf <- fcn_line_transect_sf(table_2020)
}

#' @export
fcn_transect_start_points_2020 <- function() {
  table_2020 <- fcn_line_transect_table_2020()
  line_transect_sf <- fcn_line_transect_sf(table_2020, start_point = T)
}
