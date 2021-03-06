#' Warn about a problem in `varname`, if `x` has no natural order
#'
#' Also warns, if R does not have a comparison operator for `x`.
#'
#' @param x [vector] of data
#' @param varname [character] len=1. Variable name for warning messages
#'
#' @return invisible(NULL)
util_warn_unordered <- function(x, varname) {
  if (missing(varname)) varname <- as.character(substitute(x))
  if (sum(dim(x) != 1) > 1) {
    util_error(
      "util_warn_unordered only works on effectively one-dimensional input")
  }
  warn <- FALSE
  if (is.factor(x)) {
    if (!is.ordered(x)) {
      warn <- TRUE
    }
  } else if (!is.numeric(x)) {
    if (all(DATA_TYPES_OF_R_TYPE[class(x)] !=
        DATA_TYPES$DATETIME)) {
      warn <- TRUE
    }
  }
  if (warn) {
    util_warning(
      c(
        "Don't know, how to compare values of %s (%s) -- comparisons",
        "may still be possbile, but they could be meaningless."
      ),
      dQuote(varname),
      paste0(sQuote(unique(c(class(x), typeof(x)))), collapse = ", ")
    )
  }
  invisible(NULL)
}
