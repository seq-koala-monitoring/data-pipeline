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

#' @title Get all line transects in SF format
#' @export
fcn_line_transect_sf_all <- function(cols = c("TransectID","SiteID","Date","TrSiteID",
                                              "Tlength","Sighting_Number","Number_Observers",
                                              "Start_Eastings","Start_Northings","End_Eastings",
                                              "End_Northings","geometry")) {

  db_1996 <- fcn_line_transect_sf_1996() %>% rename(TrSiteID = TransectNumber) %>% dplyr::select(cols)
  db_2020 <- fcn_line_transect_sf_2020() %>% dplyr::select(cols)

  return(rbind(db_1996, db_2020))
}

#' @export
fcn_line_transect_table_1996 <- function() {
  state <- fcn_get_state()
  query_path <- fs::path_package("sql", "1996/line-transect.sql", package='SEQKoalaDataPipeline')
  query <- readr::read_file(query_path)
  conn <- fcn_db_connect_table(1996)
  table <- RODBC::sqlQuery(conn, query, errors = T)
  fcn_db_disconnect(conn)
  return(table)
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
fcn_line_transect_sf_1996 <- function() {
  state <- fcn_get_state()
  koala_survey_data_path <- fcn_get_gdb_path()$koala_survey_data
  transect_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveyLineTransects", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=site, TransectNumber=transect, SurveyNumber=survey)
  site_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveySites", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=Site)
  sf::st_geometry(site_sf) <- "geometry"
  line_transects <- fcn_line_transect_table_1996()

  joined_table <- dplyr::inner_join(transect_sf, line_transects, by = c('SiteNumber', 'TransectNumber', 'SurveyNumber'))
  joined_sf <- fcn_sf_start_end_coord(joined_table) # Add start and end eastings/ northings information

  unjoined_table <- dplyr::anti_join(line_transects, transect_table, by = c('SiteNumber', 'TransectNumber', 'SurveyNumber'))

  if (nrow(unjoined_table) > 0) {
    # Incomplete join - use site information and report number of columns
    site_transect_sf <- dplyr::inner_join(site_sf, unjoined_table, by = c('SiteNumber')) %>%
      dplyr::mutate(Start_Eastings = NA, Start_Northings = NA, End_Eastings = NA, End_Northings = NA)
    discarded_row_number <- nrow(dplyr::anti_join(unjoined_table, site_table, by = c('SiteNumber')))
    warning(sprintf("Attribute join incomplete. \nJoined uniquely: %s. \nJoined at the site level: %s. \nJoin failed: %s", nrow(joined_table), nrow(site_transect_sf), discarded_row_number))
    # Select columns and join back to the line transect representations
    cols <- c("TransectID","SiteID","TransectNumber", "Date","Tlength", "Sighting_Number", "Number_Observers", "Start_Eastings", "Start_Northings", "End_Eastings", "End_Northings", "geometry")
    joined_table <- rbind(dplyr::select(joined_sf, cols), dplyr::select(site_transect_sf, cols))
  } else {
    message(sprintf("All line transects (%s records) successfully joined.\n", nrow(joined_table)))
  }

  return(joined_table)
}

#' @title Get start and end coordinate columns in SF objects with line/ multilinestring objects
#' @export
fcn_sf_start_end_coord <- function(sf_object) {
  coord_df <- sf::st_coordinates(sf_object) %>% as.data.frame()
  if ('L2' %in% colnames(coord_df)) {
    # Multilinestring present
    if (max(coord_df$L1) > 1) {
      warning("Multilinestring with more than two nodes present. Start and End coordinates will ignore segments after the first one.")
      coord_df <- coord_df %>% dplyr::filter(L1 <= 1)
    }
    coord_df$L1 <- coord_df$L2
    coord_df$L2 <- NULL
  }

  # Reshape the long format coordinate matrix to a wide format with columns start and end coordinates per transect record
  coords_wide <- coord_df %>%
    dplyr::mutate(start_end = rep(c("Start", "End"), length.out = nrow(coord_df))) %>%
    dplyr::rename(Eastings = X, Northings = Y) %>%
    tidyr::pivot_wider(names_from = start_end, values_from = c('Eastings', 'Northings'), names_sep = '_', names_glue = "{start_end}_{.value}")

  # Combine into a dataframe without geometry
  combined_df <- cbind(sf_object, coords_wide[,c('Start_Eastings', 'Start_Northings', 'End_Eastings', 'End_Northings')]) %>%
    sf::st_drop_geometry()

  # Regenerate coordinates by start and end coordinates to replace multilinestrings to linestrings
  combined_sf <- fcn_line_transect_sf(combined_df)

  return(combined_sf)
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
