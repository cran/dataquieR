#' Verify assumptions made by the code, that must be TRUE
#'
#' @seealso [`stopifnot`]
#'
#' @param ... see [`stopifnot`]
#' @param label [character] a label for the assumptions, can be missing
#' @param label_only [logical] if `TRUE` and `label` is given, the condition
#'                                will not be displayed, if `FALSE`
#'
#' @return `invisible(FALSE)`, if not stopped.
util_stop_if_not <- function(..., label, label_only) {
  ok <- try(eval.parent(
    call("stopifnot", substitute(...))
  ), silent = TRUE)
  if (missing(label)) {
    label <- ""
  }
  if (missing(label_only)) {
    label_only <- TRUE
  }
  util_expect_scalar(label, check_type = is.character)
  util_expect_scalar(label_only, check_type = is.logical)
  if (inherits(ok, "try-error")) {
    cm <- conditionMessage(attr(ok, "condition"))
    if (label_only && nzchar(label)) cm <- label
    if (!label_only && nzchar(label) && nzchar(cm))
      cm <- paste0(label, ": ", cm)
    util_error(
      paste("Internal error:", cm),
      applicability_problem = FALSE)
  }
  invisible(ok)
}
