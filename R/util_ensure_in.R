#' similar to match.arg
#'
#' will only warn and return a cleaned `x`.
#'
#' @param x [character] vector of needles
#' @param set [character] vector representing the haystack
#' @param err_msg [character] optional error message. Use %s twice, once for
#'                            the missing elements and once for proposals
#' @param error [logical] if `TRUE`, the execution will stop with an error,
#'                        if not all `x` are elements of `set`, otherwise,
#'                        it will throw a warning and "clean" the vector `x`
#'                        from unexpected elements.
#' @param applicability_problem [logical] error indicates unsuitable resp_vars
#'
#' @return [character] invisible(intersect(x, set))
util_ensure_in <- function(x, set, err_msg, error = FALSE, applicability_problem = NA) {
  util_expect_scalar(error, check_type = is.logical)
  if (missing(err_msg)) {
    err_msg <- sprintf("Missing %%s from %s, did you mean %%s?", sQuote(
      util_deparse1(substitute(set))))
  }
  mis <- !(x %in% set)
  if (sum(mis) > 0) {

    prop <-
      vapply(x[mis],
           FUN.VALUE = character(1),
           function(v) {
              set[which.min(adist(trimws(v),
                                  trimws(set),
                                  ignore.case = TRUE,
                                  fixed = TRUE))]
           }
      )

    ifelse(error, util_error, util_warning)(
      err_msg,
      paste0(dQuote(x[mis]), collapse = ", "),
      paste0(dQuote(prop), collapse = ", "),
      applicability_problem = applicability_problem
    )
    x[mis]
  }
  invisible(intersect(x, set))
}
