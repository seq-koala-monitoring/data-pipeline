#' Helper functions
#'
#' Check paths
#' @export
fcn_check_paths <- function(path, suffix = "", stop_exec = F) {
  if (!file.exists(path)) {
    msg <- paste(suffix, 'does not exist in', path)
    if (stop_exec) {
      stop(msg)
    } else {
      warning(msg)
    }
  }
}

#' @title Find indices of unique elements in a vector
fcn_find_unique_idx <- function(vec) split(seq_along(vec), vec)

#' @title Write list by name
fcn_write_list_csv <- function(write_list, path, prefix) {
  lapply(seq_along(write_list), function(i) {
    x <- write_list[i]
    fullpath <- paste0(path, prefix, names(write_list)[i], '.csv')
    data.table::fwrite(x, fullpath, sep = ',')
  })
}

#' @title Add date interval id to all of the dates
#' @param df: dataframe with column Date
#' @export
fcn_add_date_interval <- function(df, name_field = 'time_period_id') {
  date_intervals <- fcn_get_date_intervals()
  dates <- df$Date
  intervals <- purrr::map(date_intervals, \(x) x$interval)
  interval_names <- sapply(date_intervals, \(x) x[[name_field]])
  which_intv <- intervals %>%
    purrr::map(\(intv) which(dates %within% intv))
  date_idx <- rep(NA, length(dates))
  for (i in 1:length(which_intv)) {
    if (length(which_intv[[i]]) > 0) date_idx[which_intv[[i]]] <- interval_names[i]
  }

  df$TimePeriodID <- date_idx
  if (sum(is.na(date_idx)) > 0) {
    warning(sprintf("%s dates do not fall within interval period. Returning NAs.", sum(is.na(date_idx))))
  }
  return(df)
}
