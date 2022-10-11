#' Utility function for six sigma deviations rule
#'
#' This function calculates outliers according to the rule of six sigma
#' deviations.
#'
#' @param x [numeric] data to check for outliers
#'
#' @return binary vector
#'
#' @importFrom stats sd
util_sixsigma <- function(x) {
  xmu <- mean(x, na.rm = TRUE)
  xsd <- sd(x, na.rm = TRUE)
  lth <- xmu - 3 * xsd
  uth <- xmu + 3 * xsd
  xbin <- ifelse(x < lth | x > uth, 1, 0)
  return(xbin)
}
