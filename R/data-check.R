#' Functions for checking data correctness
#'
#' @title Check for duplicate TransectID
#' @param df: dataframe with column TransectID
fcn_check_transect_id_unique <- function(df) {
  if(anyDuplicated(df$TransectID)) {
    stop(sprintf("%s duplicate values found in TransectID", sum(duplicated(df$TransectID))))
  }
}

#' @title Check if fractions in extracted covariates sum up to approximately 1
#' @param df: a dataframe with TransectID and fractions columns
fcn_check_fractions <- function(df) {
  fraction_sum <- df %>%
    dplyr::group_by(TransectID) %>%
    dplyr::summarise(frac_sum = sum(fraction))

  not_one <- (abs(fraction_sum$frac_sum - 1) > 1e-2)

  if (any(not_one)) {
    stop(sprintf("%i TransectIDs detected with fractions unequal to 1", sum(not_one)))
  }
}

#' @title Convert date in the integrated database to a date object
#' @param df data frame
fcn_db_to_date <- function(df, fmt = "%d/%m/%Y") {
  df %>%
    dplyr::mutate(Date = as.Date(Date, format = fmt))
}
