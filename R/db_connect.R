#' Generate a database connection
#' @param db_path Path to the database

#' @export
fcn_db_connect <- function(db_path) {
  DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  PATH <- paste0(DRIVERINFO, "DBQ=", db_path)
  channel <- RODBC::odbcDriverConnect(PATH)
  return(channel)
}

#' @export
fcn_db_connect_table <- function(year) {
  state <- fcn_get_state()
  year_str <- as.character(year)
  full_db_path <- file.path(state$home_dir, state$db_path[year_str])
  if (!file.exists(full_db_path)) {
    stop(paste("Database in file path", full_db_path, "does not exist"))
  }
  conn <- fcn_db_connect(full_db_path)
  return(conn)
}

#' @export
fcn_db_disconnect <- function(conn) {
  RODBC::odbcClose(conn)
}
