#' Create spatial sf object from start and end point of line transects with eastings and northings
#'
#' @param lineTransect A dataframe with columns of Start_Eastings, Start_Northings, End_Eastings, and End_Northings
#' @param colnames A list with items Start_Eastings, Start_Northings, End_Eastings, and End_Northings, which are strings corresponding to the column name
#' @param crs Coordinate reference system (default: 7856, GDA2020)

#' @export
fcn_line_transect_sf <- function(lineTransect, colnames =
                                   list(Start_Eastings = "Start_Eastings",
                                        Start_Northings = "Start_Northings",
                                        End_Eastings = "End_Eastings",
                                        End_Northings = "End_Northings")) {

  state <- fcn_get_state()

  # Check if all columns exist in lineTransect dataframe
  lapply(colnames, function(name) {
    if ((name %in% base::colnames(lineTransect)) == F) {
      stop(paste("Column", name, "does not exist in lineTransect dataframe."))
    }
  })

  # Convert to spatial object
  start_eastings = lineTransect[,colnames$Start_Eastings, drop = T]
  start_northings = lineTransect[,colnames$Start_Northings, drop = T]
  end_eastings = lineTransect[,colnames$End_Eastings, drop = T]
  end_northings = lineTransect[,colnames$End_Northings, drop = T]

  coord_list <- list(Start_Eastings = start_eastings,
                     Start_Northings = start_northings,
                     End_Eastings= end_eastings,
                     End_Northings = end_northings
  )

  lineTransectSpatial <- purrr::pmap(
    coord_list,
    function(Start_Eastings, Start_Northings, End_Eastings, End_Northings) {
      mat <- matrix(c(Start_Eastings, Start_Northings, End_Eastings, End_Northings), 2, 2, byrow = T)
      sf::st_linestring(mat)
    })

  # Convert to feature class collection
  lineTransectSf <- sf::st_sfc(lineTransectSpatial)
  sf::st_crs(lineTransectSf) <- state$crs
  df <- sf::st_sf(cbind(lineTransect,lineTransectSf))
  return(df)
}
