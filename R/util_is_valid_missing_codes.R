#' Check, if `x` contains valid missing codes
#'
#' @param x a vector of values
#'
#' @return `TRUE` or `FALSE`
#' @family robustness_functions
#' @keywords internal
util_is_valid_missing_codes <- function(x) {
  # TODO: Use it everywhere, where we still have literally defined checks, currently.
  if (suppressWarnings(all(na.rm = TRUE,
          is.na(x) == is.na(as.numeric(x))))) {
    return(TRUE)
  }
  if (all(na.rm = TRUE, is.na(x) == suppressWarnings(
    is.na(lubridate::as_datetime(x))))) {
    return(TRUE)
  }
  return(FALSE)
}
