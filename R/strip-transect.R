#' Generate tables for koala sightings of perpendicular distances

fcn_strip_transect_table <- function(year) {
  if (!(year %in% c(1996,2020))) {
    error("Year is invalid or not yet available.")
  }

  result <- switch(
    as.character(year),
    '1996' = fcn_strip_transect_table_1996(),
    '2020' = fcn_strip_transect_table_2020()
  )
}

#' @title Extract perpendicular distances for all databases
#' @export
fcn_strip_transect_all <- function() {
  db_1996 <- fcn_strip_transect_table_1996()
  db_2020 <- fcn_strip_transect_table_2020() %>% mutate(Date = as.POSIXct(Date))
  out_db <- list(`1996-2015` = db_1996, `2020-cur` = db_2020) %>%
    dplyr::bind_rows(.id = 'db')
  return(out_db)
}

#' @title Extract perpendicular distances for koala sightings for 1996 database
#' @export
fcn_strip_transect_table_1996 <- function() {
  table <- fcn_sql_exec('1996', 'strip-transect')
  return(table)
}

#' @title Extract perpendicular distances for koala sightings for 2020 database
#' @export
fcn_strip_transect_table_2020 <- function() {
  table <- fcn_sql_exec('2020', 'strip-transect')
  return(table)
}

#' @title Extract strip transects with spatial representation in polygons
#' @export
fcn_strip_transect_sf_all <- function() {
  state <- fcn_get_state()
  koala_survey_data_path <- fcn_get_gdb_path()$koala_survey_data
  strip_transects <- fcn_strip_transect_all()
  transect_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveyTransects", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=Site, TransectNumber=Transect, SurveyNumber=Survey)
  site_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveySites", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=Site)

  sf::st_geometry(transect_sf) <- "geometry"
  sf::st_geometry(site_sf) <- "geometry"

  joined_table <- dplyr::inner_join(transect_sf, strip_transects, by = c('SiteNumber', 'TransectNumber', 'SurveyNumber'))
  unjoined_table <- dplyr::anti_join(strip_transects, transect_sf, by = c('SiteNumber', 'TransectNumber', 'SurveyNumber'))

  if (nrow(unjoined_table) > 0) {
    site_transect_sf <- dplyr::inner_join(site_sf, unjoined_table, by = c('SiteNumber'))
    discarded_row_number <- nrow(dplyr::anti_join(unjoined_table, site_sf, by = c('SiteNumber')))
    warning(sprintf("Strip Transect: attribute join incomplete. \nJoined uniquely: %s. \nJoined at the site level: %s. \nJoin failed: %s\n", nrow(joined_table), nrow(site_transect_sf), discarded_row_number))
    cols <- c("TransectID","SiteID", "Date","TArea", "Number_Sightings", "Number_Observers", "geometry")
    joined_table <- rbind(dplyr::select(joined_table, cols), dplyr::select(site_transect_sf, cols))
  } else {
    message(sprintf("All line transects (%s records) successfully joined.\n", nrow(joined_table)))
  }
  return(joined_table)
}


