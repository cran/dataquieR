#' Check for repetitive values using the digits 8 or 9 only
#'
#' Values not being finite (see [`is.finite`]) are also reported as missing
#' codes.
#'
#' @param x [`numeric`] vector to test
#' @param n_rules [`numeric`] Only outlying values can be missing
#'                                       codes; at least `n_rules` rules in
#'                                       [`acc_univariate_outlier`] match
#'
#' @return [`logical`] indicates for each value in `x`, if it looks like a
#'                     missing code
#'
#' @seealso [`acc_univariate_outlier`]
#'
util_looks_like_missing <- function(x, n_rules = 3) {
  if (!is.numeric(x)) {
    util_error("%s works only on numeric vectors",
               dQuote("util_looks_like_missing"))
  }
  sysmiss <- !is.finite(x)
  if (all(sysmiss)) {
    return(!logical(length = length(x)))
  }
  x[sysmiss] <- mean(x[!sysmiss], na.rm = TRUE)
  TYPICAL_MISSINGCODES <- c(
    99, 999, 9999, 99999, 999999, 9999999, 999999999
  )

  .x <- abs(x)
  .x <- gsub("8", "9", .x, fixed = TRUE)
  .x <- gsub(".", "", .x, fixed = TRUE)
  .x <- gsub("^0+", "", .x)
  .x <- gsub("0+$", "", .x)
  r <- .x %in% TYPICAL_MISSINGCODES

  return(
    sysmiss | (r &
      (util_tukey(x) +
       util_sixsigma(x) +
       util_hubert(x) +
       util_sigmagap(x) >= n_rules))
  )

}
