#' Print a [dataquieR] result returned by pipeline_vectorized
#' @aliases dataquieR_result
#' @param x [list] a dataquieR result from [pipeline_vectorized]
#' @param ... passed to print. Additionally, the argument `slot` may be passed
#'            to print only specific sub-results.
#'
#' @return see print
#' @export
#' @examples
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' result <- pipeline_vectorized(acc_margins, cores = list(mode = "local"),
#'   resp_vars = "SBP_0", group_vars = "USR_BP_0",
#'   study_data = study_data, meta_data = meta_data, label_col = LABEL
#' )
#' single_result <- result$`group_vars = USR_BP_0`$`resp_vars = SBP_0`
#' print(single_result, slot = "SummaryPlot")
print.dataquieR_result <- function(x, ...) {
  if (length(attr(x, "message")) > 0) {
    for (m in attr(x, "message")) {
      message(m)
    }
  }
  if (length(attr(x, "warning")) > 0) {
    for (w in attr(x, "warning")) {
      warning(w)
    }
  }
  error_shown <- FALSE
  if (length(attr(x, "error")) > 0) {
    e <- attr(x, "error")[[1]]
    try(stop(e))
    error_shown <- TRUE
  }
  attr(x, "message") <- NULL
  attr(x, "warning") <- NULL
  attr(x, "error") <- NULL
  class(x) <- setdiff(class(x), "dataquieR_result")
  opts <- list(...)
  if ("slot" %in% names(opts)) {
    if (opts$slot %in% names(x)) {
      print(x[[opts$slot]])
    } else {
      if (!error_shown) util_error("Cannot find %s in result", opts$slot)
    }
  } else {
    NextMethod()
  }
}
