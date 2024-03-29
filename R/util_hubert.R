#' utility function for the outliers rule of Hubert and Vandervieren 2008
#'
#' function to calculate outliers according to the rule of Huber et al. This
#' function requires the package `robustbase`
#'
#' @param x [numeric] data to check for outliers
#'
#' @return binary vector
#'
#' @family outlier_functions
#' @concept outlier
#' @keywords internal

util_hubert <- function(x) {
  lth <- robustbase::adjboxStats(x, doScale = FALSE)$stats[1]
  uth <- robustbase::adjboxStats(x, doScale = FALSE)$stats[5]
  xbin <- ifelse(x < lth | x > uth, 1, 0)
  return(xbin)
}
