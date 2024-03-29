#' Test, if values of x are empty, i.e. NA or whitespace characters
#' @param x the vector to test
#' @return a logical vector, same length as x; TRUE, if resp. element in x is
#'         "empty"
#'
#' @family robustness_functions
#' @concept process
#' @keywords internal
util_empty <- function(x) {
  (is.na(x) | trimws(x) == "")
}
