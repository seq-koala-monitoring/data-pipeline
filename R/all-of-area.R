#' Generate tables for koala sightings of perpendicular distances

fcn_all_of_area_table <- function(year) {
  if (!(year %in% c(1996,2020))) {
    stop("Year is invalid or not yet available.")
  }

  result <- switch(
    as.character(year),
    '1996' = fcn_all_of_area_table_1996(),
    '2020' = fcn_all_of_area_table_2020()
  )
}

#' @title Extract perpendicular distances for all databases
#' @export
fcn_all_of_area_all <- function() {
  db_1996 <- fcn_all_of_area_table_1996()
  db_2020 <- fcn_all_of_area_table_2020()  %>%
    dplyr::mutate(Date = as.POSIXct(Date))
  out_db <- list(`1996-2015` = db_1996, `2020-cur` = db_2020) %>%
    dplyr::bind_rows(.id = 'db')
  return(out_db)
}

#' @title Extract perpendicular distances for koala sightings for 1996 database
#' @export
fcn_all_of_area_table_1996 <- function() {
  table <- fcn_sql_exec('1996', 'all-of-area')
  return(table)
}

#' @title Extract perpendicular distances for koala sightings for 2020 database
#' @export
fcn_all_of_area_table_2020 <- function() {
  table <- fcn_sql_exec('2020', 'all-of-area')
  return(table)
}

#' @title Extract strip transects with spatial representation in polygons
#' @export
fcn_all_of_area_sf_all <- function() {
  state <- fcn_get_state()
  koala_survey_data_path <- fcn_get_gdb_path()$koala_survey_data
  all_of_areas <- fcn_all_of_area_all()
  site_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveySites", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=Site)

  sf::st_geometry(site_sf) <- "geometry"

  joined_table <- dplyr::inner_join(site_sf, all_of_areas, by = c('SiteNumber'))
  unjoined_table <- dplyr::anti_join(all_of_areas, site_sf, by = c('SiteNumber'))
  return(joined_table)
}


