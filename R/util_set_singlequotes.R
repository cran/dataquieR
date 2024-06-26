#' Utility function single quote string
#'
#' This function generates usual single-quotes for each element of the character
#' vector.
#'
#' @param string Character vector
#'
#' @return quoted string
#'
#' @family string_functions
#' @concept process
#' @keywords internal
util_set_sQuoteString <- function(string) {
  old <- options(useFancyQuotes = FALSE)
  on.exit(options(old))
  sQuote(string)
}
