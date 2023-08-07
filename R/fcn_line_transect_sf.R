#' Create spatial sf object from start and end point of line transects with eastings and northings
#'
#' @param lineTransect A dataframe with columns of Start_Eastings, Start_Northings, End_Eastings, and End_Northings
#' @param colnames A list with items Start_Eastings, Start_Northings, End_Eastings, and End_Northings, which are strings corresponding to the column name
#' @param start_point A boolean indicating whether the output sf should be just the start points, or the whole table

#' @export
fcn_line_transect_sf <- function(lineTransect, colnames =
                                   list(Start_Eastings = "Start_Eastings",
                                        Start_Northings = "Start_Northings",
                                        End_Eastings = "End_Eastings",
                                        End_Northings = "End_Northings"),
                                 start_point = F) {

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

  if (start_point) {
    coord_list <- list(Start_Eastings = start_eastings,
                       Start_Northings = start_northings
    )
    reduce_func <- \(Start_Eastings, Start_Northings) fcn_coord_to_points(Start_Eastings, Start_Northings)
    lineTransectSpatial <- purrr::pmap(
      coord_list,
      reduce_func
    )
  } else {
    coord_list <- list(Start_Eastings = start_eastings,
                       Start_Northings = start_northings,
                       End_Eastings= end_eastings,
                       End_Northings = end_northings
    )
    reduce_func <- \(Start_Eastings, Start_Northings, End_Eastings, End_Northings) fcn_coord_to_linestring(Start_Eastings, Start_Northings, End_Eastings, End_Northings)
    lineTransectSpatial <- purrr::pmap(
      coord_list,
      reduce_func
    )
  }

  # Convert to feature class collection
  lineTransectSf <- sf::st_sfc(lineTransectSpatial)
  sf::st_crs(lineTransectSf) <- state$crs
  df <- sf::st_sf(cbind(lineTransect,lineTransectSf))

  # Get distance
  df$length <- sf::st_length(df)

  return(df)
}

#' Coordinate to linestrings
#' @export
fcn_coord_to_linestring <- function(Start_Eastings, Start_Northings, End_Eastings, End_Northings) {
  mat <- matrix(c(Start_Eastings, Start_Northings, End_Eastings, End_Northings), 2, 2, byrow = T)
  return(sf::st_linestring(mat))
}

#' Coordinate to SF
#' @export
fcn_coord_to_points <- function(Start_Eastings, Start_Northings) {
  return(sf::st_point(c(Start_Eastings, Start_Northings)))
}
