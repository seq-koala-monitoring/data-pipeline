#' Sets environment for storing state and functions for modifying the state

the <- new.env(parent = emptyenv())
the$home_dir <- getwd()

the$db_path <- list(
  `1996` = 'SEQkoalaData.accdb',
  `2015` = "",
  `2020` = 'KoalaSurveyData2020_cur.accdb'
)

the$gdb_path <- list(
  seq_koala_survey_database="koala_survey_data_ver2_0.gdb",
  koala_survey_data="KoalaSurveyData.gdb"
)

# Grid size in meters
the$grid_size <- 1000

the$study_area <- list(dsn = "basedata.gdb", layer = "seqrp_study_area_2017_mga56")

# Standardized projection system (GDA2020 / MGA zone 56 as default)
the$crs <- 7856

#' Get the whole state, or elements of the state if a second argument is specified
#' @export
fcn_get_state <- function(elem = NULL) {
  return(the)
}

#' Set home directory path
#' @export
fcn_set_home_dir <- function(dir) {
  old <- the$home_dir
  the$home_dir <- dir
  invisible(old)
}

#' Get study area in SF format
#' @export
fcn_get_study_area <- function() {
  state <- fcn_get_state()
  study_area_args <- the$study_area
  study_area_args$dsn <- paste0(state$home_dir, '\\', study_area_args$dsn)
  study_area <- do.call(sf::st_read, study_area_args)
  study_area_transformed <- sf::st_transform(study_area, state$crs)
  return(study_area_transformed)
}

#' Report database paths
#' @export
fcn_get_db_path <- function() {
  state <- fcn_get_state()
  home_dir <- state$home_dir
  full_path <- lapply(the$db_path, function(x) {
    if (x == "") return(NULL)
    paste0(home_dir, '\\', x)
  })
  return(full_path)
}

#' Set Coordinate Reference System
#' @export
fcn_set_crs <- function(crs) {
  old <- the$crs
  the$crs <- crs
  invisible(old)
}

#' Overwrite grid when default fishnet is generated
fcn_set_grid <- function(grid) {
  old <- the$grid
  the$grid <- grid
  invisible(old)
}

fcn_get_grid <- function() {
  the$grid
}


#' Set Grid Size
#' @export
fcn_set_grid_size <- function(grid_size) {
  old <- the$grid_size
  the$grid_size <- grid_size
  invisible(old)
}
