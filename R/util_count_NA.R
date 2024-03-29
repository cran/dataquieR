#' Support function to count number of `NA`s
#'
#' Counts the number of `NA`s in x.
#'
#' @param x object to count `NA`s in
#'
#' @return number of `NA`s
#'
#' @family process_functions
#' @concept process
#' @keywords internal
util_count_NA <- function(x) {
  sum(is.na(x))
}
