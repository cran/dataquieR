#' utility function to set string in backticks
#'
#' Quote a set of variable names with backticks
#'
#' @param x variable names
#'
#' @return quoted variable names
#'
util_backtickQuote <- function(x) {
  na <- is.na(x)
  res <- paste0("`", x, "`")
  res[res == "``"] <- ""
  res[na] <- NA
  res
}
