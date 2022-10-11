#' Convert factors to label-corresponding numeric values
#'
#' Converts a vector factor aware of numeric values not being scrambled.
#'
#' @param v the vector
#' @param warn if not missing: character with error message stating conversion
#'   error
#'
#' @return the converted vector
util_as_numeric <- function(v, warn) {
  if (is.factor(v) && !is.ordered(v)) {
    r <- suppressWarnings(as.numeric(levels(v))[v])
  } else {
    r <- suppressWarnings(as.numeric(v))
  }
  conversion_errors <- (is.na(v) | "" == trimws(v)) != is.na(r)
  v_conversion_errors <- v[conversion_errors]
  if (length(v_conversion_errors) > 5) {
    v_conversion_errors <- head(v_conversion_errors, 5)
    dot <- ", ..."
  } else {
    dot <- ""
  }
  if (any(conversion_errors)) {
    if (is.factor(v)) {
      util_warning("Could not convert %s%s to numeric values",
                   paste0(dQuote(v_conversion_errors), collapse = ", "), dot,
                   applicability_problem = FALSE)
    }
    if (!missing(warn) && length(warn) == 1 && is.character(warn) &&
        !is.na(warn)) {
      util_warning(warn, paste0(dQuote(v_conversion_errors), collapse = ", "),
                   applicability_problem = FALSE)
    }
  }
  r
}
