#' Check for repetitive values using the digits 8 or 9 only
#'
#' Values not being finite (see [`is.finite`]) are also reported as missing
#' codes. Also, all missing codes must be composed out of the digits 8 and
#' 9 and they must be the largest values of a variable.
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
#' @family metadata_management
#' @concept missing
#' @noRd


util_looks_like_missing <- function(x, n_rules = 1) {
  if (any(prep_dq_data_type_of(x) %in%
          tolower(c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)))) {
    x <- as.numeric(x)
  }
  if (!is.numeric(x)) {
    util_error("%s works only on numeric vectors",
               dQuote("util_looks_like_missing"))
  }
  sysmiss <- !is.finite(x)
  if (all(sysmiss)) {
    return(!logical(length = length(x)))
  }
#  x[sysmiss] <- mean(x[!sysmiss], na.rm = TRUE)
  TYPICAL_MISSINGCODES <- c(
    99, 999, 9999, 99999, 999999, 9999999, 999999999,
    99990:99999, 999990:999999, 9999990:9999999, 999999990:999999999
  )

  .x <- abs(x)
  .x <- gsub("8", "9", .x, fixed = TRUE)
  .x <- gsub(".", "", .x, fixed = TRUE)
  .x <- gsub("^0+", "", .x)
  .x <- gsub("0+$", "", .x)
  r <- .x %in% TYPICAL_MISSINGCODES

  tuk <- util_tukey(x)
  tuk[sysmiss] <- 0
  ssig <- util_3SD(x)
  ssig[sysmiss] <- 0
  hub <- util_hubert(x)
  hub[sysmiss] <- 0
  sigg <- util_sigmagap(x)
  sigg[sysmiss] <- 0
  r <- sysmiss | (r &
                    (tuk +
                       ssig +
                       hub +
                       sigg >= n_rules)
  )
  # only, if there is no number > these numbers not being one of them
  repeat {
    done <- TRUE
    if (any(r[!is.na(r)])) {
      cur_min <- min(abs(x)[r], na.rm = TRUE)
      if (any(abs(x)[!r] > cur_min, na.rm = TRUE)) {
        r <- r & (abs(x) != cur_min)
        done <- FALSE
      }
    }
    if (done) break ;
  }
  return(r)
}
