#' Generate a full DQ report
#'
#' Deprecated
#'
#' @param ... Deprecated
#' @return Deprecated
#' @export
#' @importFrom stats setNames
dq_report <- function(...) { # nocov start
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()")
} # nocov end
