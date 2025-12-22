#' Check, if `x` contains valid missing codes
#'
#' @param x a vector of missing codes
#'
#' @return a missing code not in `x`
#' @family metadata_management
#' @noRd
util_find_free_missing_code <- function(x) {
  x <- lapply(util_as_valid_missing_codes(x), as.character)
  if (all(suppressWarnings(is.na(x) ==
                                is.na(lapply(x, util_parse_date))))) {
    return(as.character(
      suppressWarnings(max(util_parse_date(x), na.rm = TRUE)) +
        lubridate::days(1)))
  } else if (all(suppressWarnings(is.na(x) ==
                                   is.na(lapply(x, util_parse_time))))) {
    return(util_as_character(
      hms::as_hms(
      suppressWarnings(max(util_parse_time(x), na.rm = TRUE)) +
        hms::hms(hours = 1))))
  } else if (all(suppressWarnings(is.na(x) == is.na(as.numeric(x))))) {
    return(as.character(suppressWarnings(max(as.numeric(x), na.rm = TRUE)) + 1))
  } else { # fallback to datetime
    return(suppressWarnings(do.call("max", c(lapply(x, util_parse_date),
                                list(na.rm = TRUE)))) +
      lubridate::days(1))
  }
}
