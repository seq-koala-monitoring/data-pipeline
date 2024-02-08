#' Sets environment for storing state and functions for modifying the state

the <- new.env(parent = emptyenv())
the$home_dir <- getwd()

the$db_path <- list(
  `1996` = 'SEQkoalaData.accdb',
  `2015` = '2015-2019 SEQKoalaDatabase DES.accdb',
  `2020` = 'KoalaSurveyData2020_cur.accdb',
  `integrated` = 'Database_Spatial_v240206/1996 - 2023 SEQKoalaDatabase DES_Modelling_v2402061.accdb'
)

the$gdb_path <- list(
  seq_koala_survey_database="koala_survey_data_ver2_0.gdb",
  koala_survey_data="KoalaSurveyData.gdb",
  covariates="final_covariates.gdb"
)

# Grid size in meters (default: 100 meters)
the$grid_size <- 100

the$study_area <- list(dsn = "basedata.gdb", layer = "seqrp_study_area_2017_mga56")

the$raster_path <- list(
  covariates = "final_covariates_output"
)

# Standardized projection system (GDA2020 / MGA zone 56 as default)
the$crs <- 7856

# Boolean for whether raster files should be resampled to the generated grid
the$resample_to_grid <- TRUE

# Covariate time-matching algorithm
the$covariate_time_match = list(
  hlkha = 'nearest',
  hlpsz = 'bilinear',
  hlrem = 'nearest',
  hlwdy = 'bilinear'
)

the$cov_raster <- list()

# Whether to save covariate rasters to memory (speeds up computation by avoiding re-computation but uses more RAM)
the$save_cov_raster_memory <- FALSE

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
  state <- fcn_get_state()
  home_dir <- state$home_dir
  if (!is.null(obj)) {
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
  if (is.null(the$grid)) {
    grid <- fcn_new_grid()
    return(grid)
  }
  the$grid
}


#' Set Grid Size
#' @export
fcn_set_grid_size <- function(grid_size) {
  old <- the$grid_size
  the$grid_size <- grid_size
  invisible(old)
  if (grid_size < 100) {
    warning("Grid size is smaller than 100 meters. Grid creation may take a while. If continuing, use `fcn_new_grid()` to generate new grid.")
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
  state <- fcn_get_state()
  home_dir <- state$home_dir
  if (!is.null(obj)) {
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
#' @export
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

#' Set raster directory
#' @export
fcn_set_raster_path <- function(val) {
  old <- the$raster_path
  the$raster_path <- val
  invisible(old)
}

#' @title Set parameter: resample to grid
#' @export
fcn_set_resample_to_grid <- function(val) {
  old <- the$resample_to_grid
  the$resample_to_grid <- val
  invisible(old)
}

fcn_set_cov_raster <- function(val, name) {
  state <- fcn_get_state()
  if (state$save_cov_raster_memory) {
    name <- sub("\\..*$", "", name)
    old <- the$cov_raster
    old[[name]] <- val
    the$cov_raster <- old
    invisible(old)
  }
}

fcn_get_cov_raster <- function(name) {
  name <- sub("\\..*$", "", name)
  if (is.null(the$cov_raster[[name]])) {
    return(NULL)
  } else {
    cov <- the$cov_raster[[name]]
  }
  return(cov)
}

fcn_set_save_cov_raster_memory <- function(value) {
  old <- the$save_cov_raster_memory
  the$save_cov_raster_memory <- value
  invisible(old)
}

fcn_set_covariate_time_match <- function(obj) {
  old <- the$covariate_time_match
  the$covariate_time_match <- val
  invisible(old)
}

fcn_get_covariate_df <- function() {
  if (is.null(the$covariate_df)) {
    fcn_set_covariate_df()
  }
  return(the$covariate_df)
}

fcn_set_covariate_df <- function() {
  cov_layer_df <- fcn_covariate_layer_df()
  old <- the$covariate_df
  the$covariate_df <- cov_layer_df
  invisible(old)
}

#' @export
fcn_get_date_intervals <- function() {
  state <- fcn_get_state()
  if(is.null(state$date_intervals)) {
    fcn_set_date_intervals()
  }
  return(the$date_intervals)
}

fcn_set_date_intervals <- function(val = NULL) {
  old <- the$date_intervals
  if (is.null(val)) {
    the$date_intervals <- fcn_date_intervals()
  } else {
    the$date_intervals <- val
  }
  invisible(old)
}
