#' identify `NA`-only variables
#'
#' This utility function identifies variables with `NA`s values only.
#'
#' @param x the variable to check a vector
#'
#' @return flagged binary vector
#'
#' @seealso [util_correct_variable_use]
#' @family robustness_functions
#' @concept data_management
#' @keywords internal
util_only_NAs <- function(x) {
  Ns <- length(x)
  NNAs <- sum(is.na(x))
  return(ifelse(Ns == NNAs, 1, 0))
}
