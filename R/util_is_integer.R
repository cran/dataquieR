#' Check for integer values
#'
#' This function checks if a variable is integer.
#'
#' @param x the object to test
#' @param tol precision of the detection. Values deviating more than `tol` from
#'            their closest integer value will not be deemed integer.
#'
#' @return `TRUE` or `FALSE`
#' @seealso is.integer
#'
#' Copied from the documentation of \link[base]{is.integer}
#'
#' \link[base]{is.integer} detects, if the storage mode of an R-object is
#' integer. Usually, users want to know, if the values are integer. As suggested
#' by \link[base]{is.integer}'s documentation, `is.wholenumber` does so.
#'
#'
#' @family robustness_functions
#' @concept data_management
#' @keywords internal
util_is_integer <- function(x, tol = .Machine$double.eps^0.5) {
  if (is.numeric(x)) {
    r <- abs(x - round(x)) < tol & !is.nan(x)
    # & x <= .Machine$integer.max & this would return, whether x can be stored as an integer.
    #   x >= - .Machine$integer.max
  } else {
    r <- rep(FALSE, length(x))
  }
  r[is.na(r)] <- TRUE # NA is not not an integer
  r
}
