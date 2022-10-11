#' Convert a full `dataquieR` report to a `list`
#'
#' @description
#' converts a [dataquieR report][dq_report] to a [list]. Intended for use
#' in pipelines.
#'
#' @param x [dataquieR report][dq_report]
#' @param ... arguments passed to [pipeline_recursive_result]
#'
#' @return a [list] with one element per indicator call. Each element is an
#'                  encapsulated sub-list as described
#'                  in [pipeline_recursive_result]
#'
#' @export
as.list.dataquieR_resultset <- function(x, ...) {
  .tolist <- function(x, ...) {
    if (is.data.frame(x)) {
      pipeline_recursive_result(x, ...)
    } else {
      x
    }
  }
  lapply(x$long_format, .tolist, ...)
}
