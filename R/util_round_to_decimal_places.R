#' Round number of decimal places to 3 if the values are between 0.001 and 9999.999
#' otherwise (if at least one value of the vector is outside this limits)
#' use scientific notation for all the values in a vector
#'
#' @param x a numeric vector to be rounded
#' @param digits a numeric value indicating the number of desired decimal places
#'
#' @family data_management
#' @concept data_management
#' @keywords internal
util_round_to_decimal_places <- function (x, digits = 3) {
  # check if x is numeric and is a vector
  util_expect_scalar(arg_name = x, allow_more_than_one = TRUE,
                     allow_null = TRUE, allow_na = TRUE,
                     check_type = is.numeric)
  # check if digits is numeric and only 1 value
  util_expect_scalar(arg_name = digits, allow_more_than_one = FALSE,
                     allow_null = FALSE, allow_na = FALSE,
                     check_type = is.numeric)
  if (length(x) == 0) return(character(0))
  y <- abs(x)

  if (suppressWarnings(min(y[!is.na(y) & y > 0]) < 0.001 ||
                       max(y[!is.na(y)]) > 9999.999)) {
    y <- formatC(x, format = "e", digits = digits) # HINT: format(nsmall)
  } else {
    y <- as.character(round(x, digits =  digits))
  }
  return(y)
}
# TODO: Discuss
# guess_digits <- function(x, digs = getOption("digits", 4)) {
#   max(nchar(zapsmall(abs(x - floor(x)), digits = digs)) - 2, 0)
# }
#
# round(vector, xtable::xdigits(vector))
# or format() -- this is, what R uses
# format(3.66666654, digits = 2)

