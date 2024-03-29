#' Utility function Tukey outlier rule
#'
#' This function calculates outliers according to the rule of Tukey.
#'
#' @param x [numeric] data to check for outliers
#'
#' @return binary vector
#'
#' @importFrom stats quantile IQR
#'
#' @family outlier_functions
#' @concept outlier
#' @keywords internal

util_tukey <- function(x) {
  xq1 <- as.numeric(quantile(x, na.rm = TRUE)[2])
  xiqr <- IQR(x, na.rm = TRUE)
  xq3 <- as.numeric(quantile(x, na.rm = TRUE)[4])
  lth <- xq1 - 1.5 * xiqr
  uth <- xq3 + 1.5 * xiqr
  xbin <- ifelse(x < lth | x > uth, 1, 0)
  return(xbin)
}
