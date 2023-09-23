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
