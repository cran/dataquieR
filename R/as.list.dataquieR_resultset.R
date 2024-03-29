#' Convert a full `dataquieR` report to a `list`
#'
#' @description
#'
#' Deprecated
#'
#' @param x Deprecated
#' @param ... Deprecated
#'
#' @return Deprecated
#'
#' @export
as.list.dataquieR_resultset <- function(x, ...) {
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()")
}
