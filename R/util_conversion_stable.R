#' Can a vector be converted to  a defined `DATA_TYPE`
#'
#' the function also checks, if the conversion is perfect, or if something
#' is lost (e.g., decimal places), or something is strange (like arbitrary
#' suffixes in a date, just note, that
#' `as.POSIXct("2020-01-01 12:00:00 CET asdf")` does not fail in `R`), but
#' `util_conversion_stable("2020-01-01 12:00:00 CET asdf", DATA_TYPES$DATETIME)`
#' will.
#'
#' *HINT:*
#' `util_conversion_stable(.Machine$integer.max + 1, DATA_TYPES$INTEGER)` seems
#' to work correctly, although `is.integer(.Machine$integer.max + 1)`
#' returns `FALSE`.
#'
#' @param vector [vector] input vector,
#' @param data_type [enum] The type, to what the conversion should be tried.
#' @param return_percentages [logical] return the percentage of stable
#'                                     conversions or matches.
#'
#' @return [numeric] ratio of convertible entries in `vector`
#' @keywords internal
util_conversion_stable <- function(vector, data_type,
                                   return_percentages = FALSE) {
  util_expect_scalar(return_percentages, check_type = is.logical)
  util_expect_scalar(vector,
                     allow_more_than_one = TRUE,
                     allow_na = TRUE,
                     error_message =
                       sprintf(
                         "argument %s needs to be convertible to a string",
                                             sQuote("vector")),
                     check_type = is.character,
                     convert_if_possible = as.character)
  util_match_arg(data_type, DATA_TYPES)
  if (data_type == DATA_TYPES$INTEGER) {
    as_target <- util_data_type_conversion(vector, data_type)
    as_num <- util_data_type_conversion(vector, DATA_TYPES$FLOAT)
    res <-
      (
        (util_empty(as_target) & util_empty(vector)) |
        ((!util_empty(as_target) & !util_empty(vector) & as_target == as_num))
      )
  } else if (data_type == DATA_TYPES$FLOAT) { # scientific notation
    as_target <- util_data_type_conversion(vector, data_type)
    res <- util_empty(vector) == util_empty(as_target)
  } else if (data_type == DATA_TYPES$DATETIME) {
    # FIXME: integrate somehow my_parse_date to util_data_type_conversion
    # as_target <- util_data_type_conversion(string, data_type)
    as_target <- .my_parse_date(vector)
    res <- util_empty(vector) == util_empty(as_target)
  } else if (data_type == DATA_TYPES$STRING) {
    res <- rep(TRUE, length(vector))
  }
  if (return_percentages) {
    res <- sum(res) / length(vector) * 100
  }
  res
}

# x <- c("2009-09-29x", "2012-11-29 CET", "2015-29-12", "2009-09-29x", "2009-09-29x", "2012-11-29 12:00:00 CET", "2015-29-12 13:00:00", "2009-09-29x")
# x2 <- as.character(as.POSIXct(rnorm(1000000, mean = 1000000000, sd = 100000000)))
.my_parse_date <- function(x) {
  x <- trimws(x, "right")
  # remove all OlsonNames, if endswith
  for (n in OlsonNames()) {
    subst <- !is.na(x) & endsWith(x, n)
    l <- nchar(n)
    x[subst] <-
      substr(x[subst], 1, nchar(x[subst]) - l)
  }
  # TODO: can using this in favor of lubridate everywhere remove the dependency from lubridate?
  return(suppressWarnings(readr::parse_datetime(x,
                                                locale =
                                                  readr::locale(
                                                    tz =
                                                      Sys.timezone()))))
}
