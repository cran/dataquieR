#' Format numeric values with adaptive rounding
#'
#' Round numbers with a reasonable number of decimal places depending on magnitude.
#' Uses fixed notation for typical ranges and switches to scientific notation
#' for very small or large values.
#'
#' @param x a numeric vector to be rounded
#' @param digits a numeric value indicating the number of significant digits
#'
#' @return [character] formatted values
#'
#' @family data_management
#' @concept data_management
#' @noRd
util_round_to_decimal_places <- function (x, digits = 3) {

  util_expect_scalar(arg_name = x, allow_more_than_one = TRUE,
                     allow_null = TRUE, allow_na = TRUE,
                     check_type = is.numeric)

  util_expect_scalar(arg_name = digits, allow_more_than_one = FALSE,
                     allow_null = FALSE, allow_na = FALSE,
                     check_type = is.numeric)

  if (length(x) == 0) return(character(0))

  y_abs <- abs(x)
  finite <- is.finite(y_abs) & y_abs > 0

  use_sci <- FALSE
  if (any(finite)) {
    use_sci <- suppressWarnings(
      min(y_abs[finite]) < 0.001 ||
        max(y_abs[finite]) > 9999.999
    )
  }

  if (use_sci) {
    # minimal change: keep formatC but ensure consistent output
    y <- formatC(x, format = "e", digits = digits)
  } else {
    # NEW: use signif instead of round for better generic behaviour
    s <- signif(x, digits = digits)

    # limit decimal places to avoid long tails (git-friendly small change)
    s <- round(s, digits)

    # avoid scientific notation + trim zeros
    y <- format(s, trim = TRUE, scientific = FALSE)
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

