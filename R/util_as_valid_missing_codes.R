#' Convert `x` to valid missing codes
#'
#' @param x [character] a vector of values
#'
#' @return converted `x`
#' @family robustness_functions
#' @keywords internal
 util_as_valid_missing_codes <- function(x) {
   # TODO: Use it everywhere, where we still have literally defined conversions, currently.
   r <- lapply(x, as.character)
  which_numeric <-  suppressWarnings(is.na(r) == is.na(as.numeric(r)))
  which_datetime <-  suppressWarnings(is.na(r) ==
                                       is.na(lapply(r, lubridate::as_datetime)))
  which_numeric <- which_numeric & !which_datetime
  r[which_numeric] <- as.numeric(r[which_numeric])
  r[which_datetime] <- lapply(x[which_datetime], lubridate::as_datetime)
  r[!which_numeric & !which_datetime] <- NA_character_
  r
}
