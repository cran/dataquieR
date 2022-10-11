#' Utility function outliers according to the rule of Huber et al.
#'
#' This function calculates outliers according to the rule of Huber et al.
#'
#' @param x [numeric] data to check for outliers
#'
#'
#' @return binary vector
#'
#' @importFrom stats sd
util_sigmagap <- function(x) {

  # sd
  xsd <- sd(x, na.rm = TRUE)
  xmu <- mean(x, na.rm = TRUE)

  # dataframe of original values and their distances
  ints <- data.frame(RN = seq_along(x), VALUE = x)
  ints <- ints[order(ints$VALUE), ]
  ints$int <- c(0, diff(ints$VALUE))
  ints$sigmagap <- ifelse(ints$int > xsd, 1, 0)

  if (any(!is.na(ints$sigmagap)) && max(ints$sigmagap, na.rm = TRUE) == 1) {
    # if break is low
    if (max(ints$VALUE[which(ints$sigmagap == 1)], na.rm = TRUE) < xmu) {
      ints$sigmagap[1:min(which(ints$sigmagap == 1), na.rm = TRUE)] <- 1
    }
    if (min(ints$VALUE[which(ints$sigmagap == 1)], na.rm = TRUE) > xmu) {
      ints$sigmagap[min(which(ints$sigmagap == 1), na.rm = TRUE):length(x)] <-
        1
    }
  }

  # order to original seq of data
  ints <- ints[order(ints$RN), ]

  xbin <- ints$sigmagap

  return(xbin)
}
