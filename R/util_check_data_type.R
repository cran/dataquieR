#' Support function to verify the data type of a value
#'
#' Function to verify the data type of a value.
#'
#' @param x the value
#' @param type expected data type
#'
#' @return [logical] whether x is of the expected type
#' @importFrom stats setNames
util_check_data_type <- function(x, type) {
  checks <- setNames(
    list(
      util_is_integer,
      is.numeric,
      is.character,
      lubridate::is.timepoint
    ),
    nm = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$FLOAT,
      DATA_TYPES$STRING,
      DATA_TYPES$DATETIME
    )
  )

  .is <- try(checks[[type]], silent = TRUE)
  if (length(.is) != 1 || inherits(.is, "try-error")) {
    util_error("%s is not a known data type.", dQuote(type))
  }
  .is_or_na <- function(...) all(is.na(...) | .is(...))
  all(vapply(x, .is_or_na, FUN.VALUE = logical(1)))
}
