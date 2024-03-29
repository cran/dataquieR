#' Put in back-ticks
#'
#' also escape potential back-ticks in x
#'
#' @param x a string
#'
#' @return x in back-ticks
#'
#' @seealso [util_backtickQuote]
#' @family process_functions
#' @concept data_management
#' @keywords internal
util_bQuote <- function(x) {
  if (length(x) == 0)
    return(character(0))
  nna <- !is.na(x)
  x[nna] <- gsub("\\", "\\\\", x[nna], fixed = TRUE)
  x[nna] <- gsub("`", "\\`", x[nna], fixed = TRUE)
  x[nna] <- paste0("`", x[nna], "`")
  x
}
