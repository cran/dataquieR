#' Check, if `x` contains valid missing codes
#'
#' @param x a vector of values
#'
#' @return `TRUE` or `FALSE`
#' @family robustness_functions
#' @noRd
util_is_valid_missing_codes <- function(x) {
  # TODO: Use it everywhere, where we still have literally defined checks, currently.
  if (suppressWarnings(all(na.rm = TRUE,
          is.na(x) == is.na(as.numeric(x))))) {
    return(TRUE)
  }
  if (all(na.rm = TRUE, is.na(x) == suppressWarnings(
    is.na(util_parse_date(x))))) {
    return(TRUE)
  }
  if (all(na.rm = TRUE, is.na(x) == suppressWarnings(
    is.na(util_parse_time(x))))) {
    return(TRUE)
  }
  return(FALSE)
}
