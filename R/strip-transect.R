#' Generate tables for koala sightings of perpendicular distances

fcn_strip_transect_table <- function(year) {
  if (!(year %in% c(1996,2020))) {
    stop("Year is invalid or not yet available.")
  }

  result <- switch(
    as.character(year),
    '1996' = fcn_strip_transect_table_1996(),
    '2015' = fcn_strip_transect_table_2015(),
    '2020' = fcn_strip_transect_table_2020()
  )
}

#' @title Extract perpendicular distances for all databases except for 2015
#' @export
fcn_strip_transect_all <- function() {
  state <- fcn_get_state()
  if (state$use_integrated_db) {
    db <- fcn_strip_transect_table_integrated()
    return(db)
  }

  db_1996 <- fcn_strip_transect_table_1996()
  db_2015 <- fcn_strip_transect_table_2015()
  db_2020 <- fcn_strip_transect_table_2020() %>%
    dplyr::mutate(Date = as.POSIXct(Date))
  out_db <- list(`1996-2015` = db_1996, `2020-cur` = db_2020) %>%
    dplyr::bind_rows(.id = 'db')
  return(out_db)
}

#' @title Extract strip transects for 1996 database
#' @export
fcn_strip_transect_table_1996 <- function() {
  table <- fcn_sql_exec('1996', 'strip-transect')
  return(table)
}

#' @title Extract strip transects for 2015 database
#' @export
fcn_strip_transect_table_2015 <- function() {
  table <- fcn_sql_exec('2015', 'strip-transect')
  table$Date <- lubridate::ymd(table$date_ymd)
  table$date_ymd <- NULL
  return(table)
}

#' @title Extract strip transects for 2020 database
#' @export
fcn_strip_transect_table_2020 <- function() {
  table <- fcn_sql_exec('2020', 'strip-transect')
  return(table)
}

#' @title Extract strip transects for integrated database
#' @export
fcn_strip_transect_table_integrated <- function() {
  table <- fcn_sql_exec('integrated', 'strip-transect')
  fcn_check_transect_id_unique(table)
  table <- fcn_db_to_date(table)
  return(table)
}

#' @title Extract strip transects with spatial representation in polygons
#' @export
fcn_strip_transect_sf_all <- function() {
  state <- fcn_get_state()
  strip_transects <- fcn_strip_transect_all()

  if (state$use_integrated_db) {
    transect_sf <- fcn_get_integrated_db_sf()
    joined_table <- dplyr::inner_join(transect_sf, strip_transects, by = c('TransectID'))
    unjoined_table <- dplyr::anti_join(strip_transects, transect_sf, by = c('TransectID'))

    # Recover unjoined records to join at the site level
    site_join_results <- fcn_join_koala_survey_sites(unjoined_table)
    joined_table <- rbind(joined_table, site_join_results$joined_table)
    unjoined_table <- site_join_results$unjoined_table

    if (nrow(site_join_results$joined_table) > 0) {
      warning(sprintf("Strip Transect: attribute join incomplete, %s transects joined at the site level", nrow(site_join_results$joined_table)))
    }

    if (nrow(unjoined_table) > 0) {
      warning(sprintf("Strip Transect: attribute join incomplete, with %s transects dropped.", nrow(unjoined_table)))
    }
    return(joined_table)
  }

  koala_survey_data_path <- fcn_get_gdb_path()$koala_survey_data
  transect_sf <- sf::st_read(koala_survey_data_path, layer = "KoalaSurveyTransects", quiet = TRUE) %>%
    sf::st_transform(state$crs) %>%
    dplyr::rename(SiteNumber=Site, TransectNumber=Transect, SurveyNumber=Survey)
  transect_sf <- fcn_keep_distinct(transect_sf, cols = c('SiteNumber', 'TransectNumber', 'SurveyNumber'))
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

  # Join with 2015 database
  st_2015 <- fcn_strip_transect_sf_2015() %>%
    dplyr::select(TransectID, SiteID, Date, TArea, Number_Sightings, Number_Observers, geometry)
  joined_table <- rbind(joined_table, st_2015)

  return(joined_table)
}

#' Get the table in the 2015-2019 database to its spatial representation
#' @export
fcn_strip_transect_sf_2015 <- function() {
  data <- fcn_strip_transect_table_2015()
  state <- fcn_get_state()
  data.poly <- data %>%
    dplyr::group_by(TransectID) %>%
    sf::st_as_sf(coords = c("Eastings", "Northings"), crs = state$crs) %>%
    tidyr::nest() %>%
    dplyr::mutate(geometry = purrr::map(data, ~ sf::st_convex_hull(sf::st_union(.x)))) %>%
    dplyr::mutate(data = purrr::map(data, ~sf::st_drop_geometry(.x))) %>%
    tidyr::unnest(cols = c(data, geometry),names_repair = "unique") %>%
    dplyr::reframe(
      TransectID = unique(TransectID),
      SiteID = unique(SiteID),
      Date = unique(Date),
      TArea = unique(TArea),
      Number_Sightings = max(Number_Sightings),
      Number_Observers = unique(Number_Observers),
      geometry = unique(geometry)
    )
  data_sf <- data.poly %>%
    sf::st_as_sf(crs = state$crs)
  is_polygon <- sf::st_geometry_type(data_sf) == 'POLYGON'
  data_sf_polygon <- data_sf[is_polygon,]
  fcn_check_transect_id_unique(data_sf_polygon)
  return(data_sf_polygon)
}

#' Join any table with the KoalaSurveySites to get spatial representation
#' @export
fcn_join_koala_survey_sites <- function(table, site_id_col = "SiteID") {
  table <- dplyr::filter(table, !is.na(!!site_id_col))
  unjoined_table <- dplyr::filter(table, is.na(!!site_id_col))
  table_cols <- colnames(table)
  koala_survey_site_path <- fcn_get_gdb_path()$koala_survey_sites
  koala_survey_site <- sf::st_read(koala_survey_site_path, quiet = TRUE)
  joined_table <- dplyr::inner_join(koala_survey_site, table, by = dplyr::join_by("Site" == (!!site_id_col))) %>%
    dplyr::mutate(!!site_id_col := Site) %>%
    dplyr::mutate(Year_diff = lubridate::year(Date) - Survey_Yr) %>%
    dplyr::group_by(TransectID) %>%
    dplyr::filter(Year_diff >= 0) %>%
    dplyr::group_by(TransectID, !!as.symbol(site_id_col)) %>%
    dplyr::top_n(1, -abs(Year_diff)) %>%
    dplyr::select(c(table_cols, "geometry"))
  unjoined_table <- rbind(unjoined_table, dplyr::anti_join(table, koala_survey_site, by = dplyr::join_by((!!site_id_col) == "Site")))
  if (nrow(table) != nrow(joined_table)) {
    if (nrow(unjoined_table) > 0) {
      warning(sprintf("%s records not joined using Site Information", nrow(unjoined_table)))
    } else {
      warning(sprintf("%s records removed due to NA site id", nrow(table) - nrow(joined_table)))
    }
  }

  return(list(joined_table = joined_table, unjoined_table = unjoined_table))
}

