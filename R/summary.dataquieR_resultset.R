#' Summarize a [dataquieR] report
#'
#' @param ... Deprecated
#'
#' @description
#' Deprecated
#' @return Deprecated
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr %>%
summary.dataquieR_resultset <- function(...) {
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()")
}
