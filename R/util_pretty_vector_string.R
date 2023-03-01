#' Prepare a vector four output
#'
#' @param v the vector
#' @param quote function, used for quoting -- `sQuote` or `dQuote`
#'
#' @return the "pretty" collapsed vector as a string.
util_pretty_vector_string <- function(v, quote = dQuote) {
  util_expect_scalar(
    v,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    allow_na = TRUE)
  util_stop_if_not(is.function(quote))
  util_stop_if_not(length(formals(quote)) > 0)
  if (!length(v)) {
    return(quote(""))
  } else {
    if (any(is.na(v))) {
      v[[is.na(v)]] <- ""
    }
    return(paste0(quote(v), collapse = ", "))
  }
}
