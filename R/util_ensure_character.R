#' convert a value to character
#'
#' @param x the value
#' @param error [logical] if `TRUE`, an error is thrown, a warning otherwise
#'                        in case of a conversion error
#' @param error_msg error message to be  displayed, if conversion was not
#'                  possible
#' @param ... additional arguments passed to [util_error] or [util_warning]
#'            respectively in case of an error, and if an `error_msg` has
#'            been passed
#'
#' @return as.character(x)
util_ensure_character <- function(x, error = FALSE, error_msg, ...) {
  if (missing(error_msg)) {
    error_msg <-
      "conversion of %s to a character was not possible for all of its values"
    arg_name <- deparse1(substitute(x))
    error_args <- list(error_msg, sQuote(arg_name))
  } else {
    util_expect_scalar(error_msg,
                       check_type = is.character,
                       allow_more_than_one = TRUE)
    error_args <- list(error_msg, ...)
  }
  util_expect_scalar(error, check_type = is.logical)
  x1 <- suppressWarnings(as.character(x))
  if (!all(is.na(x) == is.na(x1))) {
    do.call(ifelse(error, util_error, util_warning), error_args)
  }
  x1
}
