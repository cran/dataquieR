#' Print a [dataquieR] result returned by pipeline_vectorized
#' @aliases dataquieR_result
#' @param x [list] a dataquieR result from [pipeline_vectorized] or
#'                 [util_eval_to_dataquieR_result]
#' @param ... passed to print. Additionally, the argument `slot` may be passed
#'            to print only specific sub-results.
#' @seealso [util_pretty_print()]
#' @return see print
#' @export
print.dataquieR_result <- function(x, ...) {
  old_opts <- options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.ERRORS_WITH_CALLER = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE,
    dataquieR.MESSAGES_WITH_CALLER = FALSE)
  on.exit(old_opts)
  if (length(attr(x, "message")) > 0) {
    for (m in attr(x, "message")) {
      util_message(m)
    }
  }
  if (length(attr(x, "warning")) > 0) {
    for (w in attr(x, "warning")) {
      util_warning(w)
    }
  }
  error_shown <- FALSE
  if (length(attr(x, "error")) > 0) {
    e <- attr(x, "error")[[1]]
    try(util_error(e))
    error_shown <- TRUE
  }
  attr(x, "message") <- NULL
  attr(x, "warning") <- NULL
  attr(x, "error") <- NULL
  if (inherits(x, "empty")) {
    return()
  }
  class(x) <- setdiff(class(x), c("dataquieR_result", "square_result_list"))
  if (inherits(x, "dataquieR_NULL")) {
    x <- NULL
  }
  opts <- list(...)
  if ("slot" %in% names(opts)) {
    if (opts$slot %in% names(x)) {
      print(x[[opts$slot]])
    } else {
      if (!error_shown) util_error("Cannot find %s in result", opts$slot)
    }
  } else {
    print(x, ...) # NextMethod()
  }
}
