#' Utility function observations in subgroups
#'
#' This function uses `!is.na` to count the number of non-missing observations in subgroups of
#' the data (list) and in a set of user defined response variables. In some applications it is
#' required that the number of observations per e.g. factor level is higher than a user-defined
#' minimum number.
#'
#' @param x data frame
#' @param rvs variable names
#'
#' @return matrix of flags
util_observations_in_subgroups <- function(x, rvs) {
  if (dim(x)[1] == 0) {
    x[1, ] <- t(rep(NA, length(x)))
  }

  y <- !apply(x[, rvs, drop = FALSE], 2, is.na)


  if (!is.matrix(y)) {
    y <- as.matrix(t(y))
  }

  colnames(y) <- paste0("OBSERVATIONS_OUT_", rvs)
  x <- cbind(x, y)
}
