#' Get the dimensions of a `dq_report2` result
#'
#' @param x a `dataquieR_resultset2` result
#'
#' @return dimensions
#' @export
dim.dataquieR_resultset2 <- function(x) {
  stopifnot(inherits(x, "dataquieR_resultset2"))
  c(length(rownames(x)), length(colnames(x)), length(resnames(x)))
}
