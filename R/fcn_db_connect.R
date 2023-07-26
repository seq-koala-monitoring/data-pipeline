#' Generate a database connection
#' @param db_path Path to the database

fcn_db_connect <- function(db_path) {
  DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  PATH <- paste0(DRIVERINFO, "DBQ=", db_path)
  channel <- odbcDriverConnect(PATH)
  return(channel)
}
