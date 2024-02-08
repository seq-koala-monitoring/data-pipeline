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
fcn_db_connect_table <- function(name) {
  state <- fcn_get_state()
  full_db_path <- file.path(state$home_dir, state$db_path[name])
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

#' @title SQL extract table by database year and script name
#' @export
fcn_sql_exec <- function(db_name, script_name) {
  query_path <- fs::path_package("sql", sprintf("%s/%s.sql", db_name, script_name), package='SEQKoalaDataPipeline')
  if (!file.exists(query_path)) {
    stop(sprintf("SQL script does not exist in path %s", query_path))
  }
  query <- readr::read_file(query_path)
  conn <- fcn_db_connect_table(db_name)
  table <- RODBC::sqlQuery(conn, query, errors = TRUE)
  if (is.character(table)) {
    fcn_db_disconnect(conn)
    stop(table)
    return()
  } else {
    fcn_db_disconnect(conn)
    return(table)
  }
}
