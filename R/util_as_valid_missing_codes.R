#' Convert `x` to valid missing codes
#'
#' @param x [character] a vector of values
#'
#' @return converted `x`
#' @family robustness_functions
#' @noRd
 util_as_valid_missing_codes <- function(x) {
   # TODO: Use it everywhere, where we still have literally defined conversions, currently.
   r <- lapply(x, as.character)
  which_numeric <-  suppressWarnings(is.na(r) == is.na(as.numeric(r)))
  which_datetime <-  suppressWarnings(is.na(r) ==
                                       is.na(lapply(r, util_parse_date)))
  which_time <-  suppressWarnings(is.na(r) ==
                                        is.na(lapply(r, util_parse_time)))
  which_numeric <- which_numeric & !which_datetime & !which_time
  r[which_numeric] <- as.numeric(r[which_numeric])
  r[which_datetime] <- lapply(x[which_datetime], util_parse_date)
  r[which_time] <- lapply(x[which_time], util_parse_time)
  r[!which_numeric & !which_datetime & !which_time] <- NA_character_
  r
}
