#' utility function to set string in backticks
#'
#' Quote a set of variable names with backticks
#'
#' @param x variable names
#'
#' @return quoted variable names
#'
#' @seealso `util_bQuote`
#' @family process_functions
#' @concept data_management
#' @noRd
util_backtickQuote <- function(x) {
  na <- is.na(x)
  res <- paste0("`", x, "`")
  res[res == "``"] <- ""
  res[na] <- NA
  res
}
