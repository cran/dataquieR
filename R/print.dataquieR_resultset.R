#' Generate a RMarkdown-based report from a [dataquieR] report
#'
#'
#' @param ... deprecated
#'
#' @return deprecated
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset <- function(...) {
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()")
}



# rstudioapi::jobRunScript()
