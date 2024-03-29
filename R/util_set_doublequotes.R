#' Utility function to put strings in quotes
#'
#' This function generates usual double-quotes for each element of the character
#' vector
#'
#' @param string Character vector
#'
#' @return quoted string
#'
#' @family string_functions
#' @concept process
#' @keywords internal
util_set_dQuoteString <- function(string) {
  old <- options(useFancyQuotes = FALSE)
  on.exit(options(old))
  dQuote(string)
}
