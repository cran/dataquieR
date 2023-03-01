#' return the number of result slots in a report
#'
#' @param x the `dataquieR` report (v2.0)
#'
#' @return the number of used result slots
#'
#' @export
nres <- function (x) dim(x)[3L]
