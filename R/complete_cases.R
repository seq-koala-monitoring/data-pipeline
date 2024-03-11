## Functions to check for missing data in covariate extraction and keep values that are not NA

#' Get indices of GridID with complete covariate data
#' @export
fcn_complete_grid_id <- function(cov_constant_array, cov_temporal_array) {
  constant_array_idx <- complete.cases(cov_constant_array)
  temporal_array <- lapply(1:dim(cov_temporal_array)[3], function(i) {
    return(complete.cases(cov_temporal_array[,,i]))
  })

  temporal_array_idx <- do.call(cbind, temporal_array) %>%
    apply(1, all)
  idx <- cbind(constant_array_idx, temporal_array_idx) %>%
    apply(1, all)
  return(cov_constant_array$GridID[idx])
}

#' Check for complete cases for 2-D and 3-D matrices/ data.frames
#' @export
fcn_complete_cases_check <- function(mat) {
  if (length(dim(mat)) == 2) {
    return(all(complete.cases(mat)))
  } else if (length(dim(mat)) == 3) {
    nslices <- dim(mat)[3]
    no_missing <- sapply(1:nslices, \(i) all(complete.cases(mat[,,i])))
    return(all(no_missing))
  }
}
