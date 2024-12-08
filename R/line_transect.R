#' Extract Line transect tables

#' Wrapper function for generating line transect tables
#' @export
fcn_line_transect_table <- function(cols = c("TransectID","SiteID","Date","TrSiteID",
                                                 "Tlength","Number_Sightings","Number_Observers")) {
  state <- fcn_get_state()
  if (state$use_integrated_db) {
    db <- fcn_line_transect_table_integrated()
    return(db)
  }

  db_1996 <- fcn_line_transect_table_1996() %>%
    dplyr::rename(TrSiteID = TransectNumber) %>%
    dplyr::select(cols)
  db_2020 <- fcn_line_transect_table_2020() %>% dplyr::select(cols)
  out_db <- list(`1996-2015` = db_1996, `2020-cur` = db_2020) %>%
    dplyr::bind_rows(.id = 'db')
  return(out_db)
}

#' @title Get all line transects in SF format
#' @export
fcn_line_transect_sf_all <- function(cols = c("TransectID","SiteID","Date","TrSiteID",
                                              "Tlength","Number_Sightings","Number_Observers",
                                              "Start_Eastings","Start_Northings","End_Eastings",
                                              "End_Northings","geometry")) {
  state <- fcn_get_state()
  if (state$use_integrated_db) {
    db <- fcn_line_transect_sf_integrated()
    return(db)
  }

  db_1996 <- fcn_line_transect_sf_1996() %>%
    dplyr::rename(TrSiteID = TransectNumber) %>%
    dplyr::select(cols)
  db_2020 <- fcn_line_transect_sf_2020() %>%
    dplyr::select(cols)

  out_db <- list(`1996-2015` = db_1996, `2020-cur` = db_2020) %>%
    dplyr::bind_rows(.id = 'db')
  return(out_db)
}

#' @export
fcn_line_transect_table_1996 <- function() {
  state <- fcn_get_state()
  table <- fcn_sql_exec('1996', 'line-transect')

  # Check if the transect ID uniquely identifies the line transects
  fcn_check_transect_id_unique(table)

  return(table)
}

#' @export
fcn_line_transect_table_2020 <- function() {
  state <- fcn_get_state()
  table <- fcn_sql_exec("2020", "line-transect")

  # Check if the transect ID uniquely identifies the line transects
  fcn_check_transect_id_unique(table)

  return(table)
}

#' @export
fcn_line_transect_table_integrated <- function() {
  state <- fcn_get_state()
  table <- fcn_sql_exec("integrated", "line-transect")

  # Check if the transect ID uniquely identifies the line transects
  fcn_check_transect_id_unique(table)
  table <- fcn_db_to_date(table)
  return(table)
}

#' @export
fcn_line_transect_sf_1996 <- function() {
  state <- fcn_get_state()
  koala_survey_data_path <- fcn_get_gdb_path()$koala_survey_data
  transect_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveyLineTransects", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=site, TransectNumber=transect, SurveyNumber=survey)
  transect_sf <- fcn_keep_distinct(transect_sf)

  site_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveySites", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=Site)
  sf::st_geometry(site_sf) <- "geometry"
  line_transects <- fcn_line_transect_table_1996()

  joined_table <- dplyr::inner_join(transect_sf, line_transects, by = c('SiteNumber', 'TransectNumber', 'SurveyNumber'), relationship = 'one-to-many')
  joined_sf <- fcn_sf_start_end_coord(joined_table) # Add start and end eastings/ northings information

  unjoined_table <- dplyr::anti_join(line_transects, transect_sf, by = c('SiteNumber', 'TransectNumber', 'SurveyNumber'))

  if (nrow(unjoined_table) > 0) {
    # Incomplete join - use site information and report number of columns
    site_transect_sf <- dplyr::inner_join(site_sf, unjoined_table, by = c('SiteNumber')) %>%
      dplyr::mutate(Start_Eastings = NA, Start_Northings = NA, End_Eastings = NA, End_Northings = NA)
    discarded_row_number <- nrow(dplyr::anti_join(unjoined_table, site_sf, by = c('SiteNumber')))
    warning(sprintf("Line Transect: attribute join incomplete. \nJoined uniquely: %s. \nJoined at the site level: %s. \nJoin failed: %s\n", nrow(joined_table), nrow(site_transect_sf), discarded_row_number))
    # Select columns and join back to the line transect representations
    cols <- c("TransectID","SiteID","TransectNumber", "Date","Tlength", "Number_Sightings", "Number_Observers", "Start_Eastings", "Start_Northings", "End_Eastings", "End_Northings", "geometry")
    joined_table <- rbind(dplyr::select(joined_sf, cols), dplyr::select(site_transect_sf, cols))
  } else {
    message(sprintf("All line transects (%s records) successfully joined.\n", nrow(joined_table)))
  }

  fcn_check_transect_id_unique(joined_table)

  return(joined_table)
}

fcn_line_transect_sf_integrated <- function() {
  db_original <- fcn_line_transect_table_integrated()
  sp <- fcn_get_integrated_db_sf()
  db_joined <- sp %>%
    dplyr::inner_join(db_original, by = "TransectID")
  db_not_joined <- dplyr::anti_join(db_original, sp, by = "TransectID")

  # Create spatial representation if the db is not joined
  db_coords_present <- db_not_joined %>%
    dplyr::filter(!is.na(Start_Eastings) & !is.na(Start_Northings) & !is.na(End_Eastings) & !is.na(End_Northings))

  db_coords_absent <- dplyr::filter(db_not_joined, !(TransectID %in% db_coords_present))

  line_transect <- fcn_line_transect_sf(db_coords_present)
  buffer_width <- fcn_get_line_transect_buffer()
  line_transect_buffer <- sf::st_buffer(line_transect, endCapStyle = "FLAT", dist = buffer_width)
  db <- rbind(db_joined, line_transect_buffer)

  # Recover unjoined records to join at the site level
  site_join_results <- fcn_join_koala_survey_sites(db_coords_absent)
  db <- rbind(db, site_join_results$joined_table)
  unjoined_table <- site_join_results$unjoined_table

  if (nrow(db) != nrow(db_original)) {
    warning(sprintf("Integrated DB line transects (total=%s): %s joined with total shp, %s created with coordinates, %s joined with site information, %s not joined. Final result: %s records", nrow(db_original), nrow(db_joined), nrow(db_coords_present), nrow(site_join_results$joined_table), nrow(db_original) - nrow(db), nrow(db)))
  }

  return(db)
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
  fcn_check_transect_id_unique(line_transect_sf)
  return(line_transect_sf)
}

#' @export
fcn_transect_start_points_2020 <- function() {
  table_2020 <- fcn_line_transect_table_2020()
  line_transect_sf <- fcn_line_transect_sf(table_2020, start_point = T)
}

