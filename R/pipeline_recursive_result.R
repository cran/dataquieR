#' Convert a pipeline result data frame to named encapsulated lists
#'
#' @description
#'
#' Deprecated
#'
#' @param ... Deprecated
#'
#' @return Deprecated
#'
#' @export
#' @importFrom stats setNames
pipeline_recursive_result <- function(...) { # nocov start
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()")
} # nocov end
