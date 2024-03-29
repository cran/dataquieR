#' Check, if `x` contains valid missing codes
#'
#' @param x a vector of missing codes
#'
#' @return a missing code not in `x`
#' @family metadata_management
#' @keywords internal
util_find_free_missing_code <- function(x) {
  x <- lapply(util_as_valid_missing_codes(x), as.character)
  if (all(suppressWarnings(is.na(x) ==
                                is.na(lapply(x, lubridate::as_datetime))))) {
    return(as.character(
      suppressWarnings(max(lubridate::as_datetime(x), na.rm = TRUE)) +
        lubridate::days(1)))
  } else if (all(suppressWarnings(is.na(x) == is.na(as.numeric(x))))) {
    return(as.character(suppressWarnings(max(as.numeric(x), na.rm = TRUE)) + 1))
  } else { # fallback to datetime
    return(suppressWarnings(do.call("max", c(lapply(x, lubridate::as_datetime),
                                list(na.rm = TRUE)))) +
      lubridate::days(1))
  }
}
