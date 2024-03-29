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
#' @keywords internal
util_data_type_conversion <- function(x, type) {
  # look-up list about how to convert
  converts <- setNames(
    list(
      function(x) { floor(as.numeric(x)) },
      # as.integer may fail for too large integer numbers
      as.numeric,
      as.character,
      lubridate::as_datetime
    ),
    nm = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$FLOAT,
      DATA_TYPES$STRING,
      DATA_TYPES$DATETIME
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
