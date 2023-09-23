# Functions for processing dates

#' @title Convert year-month representation to dates
#' @export
fcn_ym_to_date <- function(year_month) {
  num <- substr(year_month, nchar(year_month)-5, nchar(year_month))
  date <- lubridate::ym(num)
  if (any(is.na(date))) {
    stop(sprintf("Date string %s failed to parse.", year_month))
  }
  return(date)
}

#' @title Get number of days difference between the two dates
#' @export
fcn_date_diff <- function(date_a, date_b) {
  return(date_b - date_a)
}

#' @title Get a matrix of date difference from a vector of reference dates with data
#' @export
fcn_date_diff_matrix <- function(dates, ref_dates) {
  sapply(ref_dates, fcn_date_diff, dates)
}

#' @title Convert date difference matrix to sparse matrix of nearest dates
#' @export
fcn_date_weights_nearest_date <- function(date_diff_list) {
  lapply(date_diff_list, function(x) {
    res <- list(
      weight = c(1),
      idx = c(which.min(abs(v)))
    )
    return(res)
  })
}

#' @title Convert date difference matrix to sparse matrix of weights of the two closest dates for temporal interpolation
#' @export
fcn_date_bilinear_interpolation_date <- function(date_diff_matrix) {
  t(apply(date_diff_matrix, 1, fcn_interpolation_weight))
}

#' @title Given a vector of date differences, find interpolation weight of the n values smallest in the vector
fcn_interpolation_weight <- function(v) {
  out_vec <- rep(0, times = length(v))
  # All reference dates are in the past
  if (all(v < 0)) {
    out_vec[1] <- 1
    return(out_vec)
  }

  # The date is larger than all reference dates; take the most recent value
  if (all(v > 0)) {
    out_vec[length(v)] <- 1
    return(out_vec)
  }

  # Date is in the middle of reference dates; find the two dates closest to it and calculate its weight
  sorted_v <- sort(abs(v), index.return = T)
  closest_values <- sorted_v$x[1:2]
  weights <- (sum(closest_values) - closest_values) / sum(closest_values)
  out_vec[sorted_v$ix[1:2]] <- weights
  return(out_vec)
}
