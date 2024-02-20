#' Generate tables for koala sightings of perpendicular distances

fcn_all_of_area_table <- function(year) {
  state <- fcn_get_state()
  if (state$use_integrated_db) {
    db <- fcn_all_of_area_table_integrated()
    return(db)
  }

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
  state <- fcn_get_state()
  if (state$use_integrated_db) {
    db <- fcn_all_of_area_table_integrated()
    return(db)
  }

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

#' @title Extract perpendicular distances for koala sightings for integrated database
#' @export
fcn_all_of_area_table_integrated <- function() {
  table <- fcn_sql_exec('integrated', 'all-of-area')
  fcn_check_transect_id_unique(table)
  table <- fcn_db_to_date(table)
  return(table)
}

#' @title Extract strip transects with spatial representation in polygons
#' @export
fcn_all_of_area_sf_all <- function() {
  state <- fcn_get_state()

  all_of_areas <- fcn_all_of_area_all()
  if (state$use_integrated_db) {
    site_sf <- fcn_get_integrated_db_sf()
    joined_table <- dplyr::inner_join(site_sf, all_of_areas, by = c('TransectID'))
    unjoined_table <- dplyr::anti_join(all_of_areas, site_sf, by = c('TransectID'))

    # Recover unjoined records to join at the site level
    site_join_results <- fcn_join_koala_survey_sites(unjoined_table)
    joined_table <- rbind(joined_table, site_join_results$joined_table)
    unjoined_table <- site_join_results$unjoined_table

    if (nrow(site_join_results$joined_table) > 0) {
      warning(sprintf("Strip Transect: attribute join incomplete, %s transects joined at the site level", nrow(site_join_results$joined_table)))
    }

  } else {
    koala_survey_data_path <- fcn_get_gdb_path()$koala_survey_data
    site_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveySites", quiet = TRUE) %>%
      sf::st_transform(state$crs) %>%
      dplyr::rename(SiteNumber=Site)
    sf::st_geometry(site_sf) <- "geometry"
    joined_table <- dplyr::inner_join(site_sf, all_of_areas, by = c('SiteNumber'))
    unjoined_table <- dplyr::anti_join(all_of_areas, site_sf, by = c('SiteNumber'))
  }

  if (nrow(unjoined_table) > 0) {
    warning(paste0("Number of rows not joined: ", nrow(unjoined_table)))
  }

  return(joined_table)
}


