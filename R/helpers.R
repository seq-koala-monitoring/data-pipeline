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
