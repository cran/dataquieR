#' Call (nearly) one "Accuracy" function with many parameterizations at once
#' automatically
#'
#' @param ... Deprecated
#'
#' @description
#'
#' Deprecated
#'
#'
#' @return Deprecated
#'
#' @export
#' @importFrom stats setNames
#'
pipeline_vectorized <- function(...) { # nocov start
  lifecycle::deprecate_stop("2.1.0", what = "pipeline_vectorized()",
                      with = "dq_report2()")
} # nocov end
