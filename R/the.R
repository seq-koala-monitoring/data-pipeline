#' Sets environment for storing state and functions for modifying the state

the <- new.env(parent = emptyenv())
the$home_dir <- getwd()

the$db_path <- list(
  `1996` = 'SEQkoalaData.accdb',
  `2015` = NULL,
  `2020` = 'KoalaSurveyData2020_cur.accdb'
)

the$gdb_path <- list(
  seq_koala_survey_database="koala_survey_data_ver2_0.gdb",
  koala_survey_data="KoalaSurveyData.gdb",
  covariates="final_covariates.gdb"
)

# Grid size in meters (default: 1000 meters)
the$grid_size <- 1000

the$study_area <- list(dsn = "basedata.gdb", layer = "seqrp_study_area_2017_mga56")

the$raster_path <- list(
  covariates = "final_covariates_raster"
)

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
  fcn_check_paths(dir, "Home directory", TRUE)
  invisible(old)
}

#' Get study area in SF format
#' @export
fcn_get_study_area <- function() {
  state <- fcn_get_state()
  study_area_args <- the$study_area
  dsn_path <- file.path(state$home_dir, study_area_args$dsn)
  fcn_check_paths(dsn_path, "Study area file", TRUE) # Halt execution if study area file is not found
  study_area_args$dsn <- dsn_path
  study_area_args$quiet <- TRUE # silence the call
  study_area <- do.call(sf::st_read, study_area_args)
  study_area_transformed <- sf::st_transform(study_area, state$crs)
  return(study_area_transformed)
}

#' Report database paths
#' @export
fcn_get_db_path <- function() {
  state <- fcn_get_state()
  home_dir <- state$home_dir
  full_path <- purrr::imap(the$db_path, function(x, idx) {
    if (is.null(x) || identical(x, "")) return(NULL)
    path <- file.path(home_dir, x)
    fcn_check_paths(path, paste("Database", idx))
    path
  })
  return(full_path)
}

#' Set database paths
#' @export
fcn_set_db_path <- function(db_path, obj = NULL) {
  if (!is.null(obj)) {
    state <- fcn_get_state()
    db_path_name <- db_path
    # Only set property for that key
    db_path <- state$db_path
    db_path[obj] <- db_path_name
  }

  purrr::imap(db_path, function(x, idx) {
    if (is.null(x) || identical(x, "")) return(NULL)
    path <- file.path(home_dir, x)
    fcn_check_paths(path, paste("Database", idx), stop_exec = T)
  })

  old <- the$db_path
  the$db_path <- db_path
  invisible(old)
}

#' Set Coordinate Reference System
#' @export
fcn_set_crs <- function(crs) {
  old <- the$crs
  the$crs <- crs
  invisible(old)
}

#' Overwrite grid when default fishnet is generated
#' @export
fcn_set_grid <- function(grid) {
  old <- the$grid
  the$grid <- grid
  invisible(old)
}

#' @export
fcn_get_grid <- function() {
  the$grid
}


#' Set Grid Size
#' @export
fcn_set_grid_size <- function(grid_size) {
  old <- the$grid_size
  the$grid_size <- grid_size
  invisible(old)
  if (grid_size < 1000) {
    warning("Grid size is smaller than 1000 meters. Grid creation may take a while. If continuing, use `fcn_new_grid()` to generate new grid.")
    return()
  }
  if (grid_size > 50000) {
    warning("Grid size is larger than 50000 meters, which will generate less than 20 grids. Prediction resolution may be too low. If continuing, use `fcn_new_grid()` to generate new grid.")
    return()
  }
  fcn_new_grid()
}

#' Get GDB object path
#' @export
fcn_get_gdb_path <- function() {
  state <- fcn_get_state()
  home_dir <- state$home_dir
  full_path <- purrr::imap(the$gdb_path, function(x, idx) {
    if (is.null(x) || identical(x, "")) return(NULL)
    path <- file.path(home_dir, x)
    fcn_check_paths(path, paste("Database", idx))
    return(path)
  })
  return(full_path)
}

#' Set GDB paths
#' @export
fcn_set_gdb_path <- function(db_path, obj = NULL) {
  if (!is.null(obj)) {
    state <- fcn_get_state()
    db_path_name <- db_path
    # Only set property for that key
    db_path <- state$gdb_path
    db_path[obj] <- db_path_name
  }

  purrr::imap(db_path, function(x, idx) {
    if (is.null(x) || identical(x, "")) return(NULL)
    path <- file.path(home_dir, x)
    fcn_check_paths(path, paste("Geodatabase", idx), stop_exec = T)
  })

  old <- the$db_path
  the$db_path <- db_path
  invisible(old)
}

#' Get raster directories
fcn_get_raster_path <- function() {
  state <- fcn_get_state()
  home_dir <- state$home_dir
  full_path <- purrr::imap(the$raster_path, function(x, idx) {
    if (is.null(x) || identical(x, "")) return(NULL)
    path <- file.path(home_dir, x)
    fcn_check_paths(path, paste("Database", idx))
    path
  })
  return(full_path)
}
