#' identify `NA`-only variables
#'
#' This utility function identifies variables with `NA`s values only.
#'
#' @param x the variable to check a vector
#'
#' @return flagged binary vector
#'
util_only_NAs <- function(x) {
  Ns <- length(x)
  NNAs <- sum(is.na(x))
  return(ifelse(Ns == NNAs, 1, 0))
}
