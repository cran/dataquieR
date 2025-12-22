#' Data type conversion
#'
#' Utility function to convert a study variable to match the data type given in
#' the metadata, if possible.
#'
#' @param x the value
#' @param type expected data type
#'
#' @return the transformed values (if possible)
#'
#' @noRd
util_data_type_conversion <- function(x, type) {
  if (is.factor(x)) {
    if (isTRUE(as.logical(getOption("dataquieR.old_factor_handling",
                                    dataquieR.old_factor_handling_default)))) {
      # This is for backwards compatibility, but it may be more user friendly to
      # omit this in both adjust_data_type functions
      # noop
    } else {
      x <- as.character(x)
    }
  }
  # look-up list about how to convert
  converts <- setNames(
    list(
      function(x) {
        if (identical(attr(type, "orig_type"), "logical")) {
          as.integer(as.logical(x))
        } else {
          floor(as.numeric(x))
        }
      },
      # as.integer may fail for too large integer numbers
      as.numeric,
      as.character,
      util_parse_date,
      util_as_time_only
    ),
    nm = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$FLOAT,
      DATA_TYPES$STRING,
      DATA_TYPES$DATETIME,
      DATA_TYPES$TIME
    )
  )

  # function to convert to the matching data type
  .as <- try(converts[[type]], silent = TRUE)
  if (length(.as) != 1 || inherits(.as, "try-error")) {
    util_error("%s is not a known data type.", dQuote(type),
               applicability_problem = TRUE)
  }
  # perform data type conversion
  x2 <- suppressWarnings(do.call(.as, list(x)))

  return(x2)
}
