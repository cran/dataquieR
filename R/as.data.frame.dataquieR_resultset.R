#' Convert a full `dataquieR` report to a `data.frame`
#'
#' @description
#'
#' Deprecated
#'
#' @param ... Deprecated
#' @param x Deprecated
#'
#' @return Deprecated
#' @export
as.data.frame.dataquieR_resultset <- function(x, ...) {
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()")
}
