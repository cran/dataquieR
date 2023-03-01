#' Verify the data type of a value
#'
#' Function to verify the data type of a value.
#'
#' @param x the value
#' @param type expected data type
#' @param check_convertible [logical] also try, if a conversion to the
#'                                    declared data type would work.
#' @param threshold_value [numeric] from=0 to=100. percentage failing
#'                                  conversions allowed if `check_convertible`
#'                                  is `TRUE`.
#' @param return_counts [logical] return the counts instead of logical values
#'                                about threshold violations.
#'
#' @return if `check_convertible` is `FALSE`,
#'         [logical] whether x is of the expected type
#'         if `check_convertible` is `TRUE`
#'         [integer] with the states `0, 1, 2`: 0 = Mismatch, not convertible
#'                                              1 = Match
#'                                              2 = Mismatch, but convertible
#' @importFrom stats setNames
util_check_data_type <- function(x, type, check_convertible = FALSE,
                                 threshold_value = 0, return_counts = FALSE) {
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
    util_error("%s is not a known data type.", dQuote(type),
               applicability_problem = TRUE)
  }
  .is_or_na <- function(...) all(is.na(...) | .is(...))
  result <- all(vapply(x, .is_or_na, FUN.VALUE = logical(1)))

  if (check_convertible && !result) {
    converts <- setNames(
      list(
        as.integer,
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

    .as <- try(converts[[type]], silent = TRUE)
    if (length(.as) != 1 || inherits(.as, "try-error")) {
      util_error("%s is not a known data type.", dQuote(type),
                 applicability_problem = TRUE)
    }
    x2 <- suppressWarnings(lapply(x, .as))
    if (any(!vapply(x, is.na, FUN.VALUE = logical(1))) &&
            all(vapply(x2, is.na, FUN.VALUE = logical(1)))) {
      result <- 0
    } else if (100 * sum(any(vapply(x, is.na, FUN.VALUE = logical(1)) !=
                             vapply(x2, is.na, FUN.VALUE = logical(1)))) /
               sum(!vapply(x, is.na, FUN.VALUE = logical(1))) >
               threshold_value) { # return percentage too -- two columns in heatmap: percentage missmatch, percentage non-convertible
      # percentage convertible?
      result <- 0
    } else {
      result2 <- all(vapply(x2, .is_or_na, FUN.VALUE = logical(1)))
      if (result2) {
        result <- 2
      } else {
        result <- as.integer(result)
      }
    }
  }

  return(result)
}
