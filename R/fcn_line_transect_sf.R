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
                                        End_Northings = "End_Northings"),
                                 crs = 7856) {

  # Check if all columns exist in lineTransect dataframe
  lapply(colnames, function(name) {
    if ((name %in% base::colnames(lineTransect)) == F) {
      stop(paste("Column", name, "does not exist in lineTransect dataframe."))
    }
  })

  # Convert to spatial object
  lineTransectSpatial <- purrr::pmap(
    list(Start_Eastings = lineTransect[Start_Eastings],
         Start_Northings = lineTransect[Start_Northings],
         End_Eastings= lineTransect[End_Eastings],
         End_Northings =lineTransect[End_Northings]
    ), function(Start_Eastings, Start_Northings, End_Eastings, End_Northings) {
      sf::st_linestring(matrix(c(Start_Eastings, Start_Northings, End_Eastings, End_Northings), 2, 2, byrow = T))
    })

  # Convert to feature class collection
  lineTransectSf <- sf::st_sfc(lineTransectSpatial)
  sf::st_crs(lineTransectSf) <- crs
  df <- sf::st_sf(cbind(lineTransect,lineTransectSf))
  return(df)
}
