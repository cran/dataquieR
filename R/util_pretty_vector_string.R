#' Prepare a vector four output
#'
#' @param v the vector
#' @param quote function, used for quoting -- `sQuote` or `dQuote`
#' @param n_max maximum number of elements of `v` to display, if not missing.
#'
#' @return the "pretty" collapsed vector as a string.
#'
#' @family string_functions
#' @concept process
#' @keywords internal
util_pretty_vector_string <- function(v, quote = dQuote, n_max = length(v)) {
  util_expect_scalar(
    v,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    allow_na = TRUE)
  util_expect_scalar(n_max, check_type = util_is_numeric_in(min = 1,
                                                            finite = TRUE,
                                                            whole_num = TRUE))
  util_stop_if_not(is.function(quote))
  util_stop_if_not(length(formals(quote)) > 0)
  if (!length(v)) {
    return(quote(""))
  } else {
    if (any(is.na(v))) {
      v[is.na(v)] <- ""
    }
    if (length(v) > n_max) {
      v <- head(quote(v), n_max)
      v <- c(v, "...")
    } else {
      v <- quote(v)
    }
    return(paste0(v, collapse = ", "))
  }
}
