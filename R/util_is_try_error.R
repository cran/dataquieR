#' Check, if `x` is a try-error
#'
#' @param x
#'
#' @return [logical()] if it is a try-error
#' @keywords internal
util_is_try_error <- function(x) {
  inherits(x, "try-error")
}

#' Extract condition from try error
#'
#' @param x the try-error object
#'
#' @return [condition] of the try-error
#' @keywords internal
util_condition_from_try_error <- function(x) {
  if (!util_is_try_error(x)) {
    util_error("Internal error %s: Not a try-error. Please report, sorry.",
               dQuote(class(x)))
  } else {
    attr(x, "condition")
  }
}
