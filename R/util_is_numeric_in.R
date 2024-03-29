#' Create a predicate function to check for certain numeric properties
#'
#' useful, e.g., for [util_expect_data_frame] and [util_expect_scalar]. The
#' generated function returns on `TRUE` or `FALSE`, even if called with a
#' vector.
#'
#' @param min if given, minimum for numeric values
#' @param max if given, maximum for numeric values
#' @param whole_num if TRUE, expect a whole number
#' @param set if given, a set, the value must be in (see [util_match_arg])
#' @param finite Are `Inf` and `-Inf` invalid values? (FALSE by default)
#'
#' @return a function that checks an `x` for the properties.
#' @examples
#' \dontrun{
#' util_is_numeric_in(min = 0)(42)
#' util_is_numeric_in(min = 43)(42)
#' util_is_numeric_in(max = 3)(42)
#' util_is_numeric_in(whole_num = TRUE)(42)
#' util_is_numeric_in(whole_num = TRUE)(42.1)
#' util_is_numeric_in(set = c(1, 3, 5))(1)
#' util_is_numeric_in(set = c(1, 3, 5))(2)
#' }
#'
#' @family robustness_functions
#' @concept data_management
#' @keywords internal
util_is_numeric_in <- function(min = -Inf,
                               max = +Inf,
                               whole_num = FALSE,
                               finite = FALSE,
                               set = NULL) {
  function(x) {
    if (!is.numeric(x)) {
      return(FALSE)
    }
    if (finite && any(!is.finite(x))) {
      return(FALSE)
    }
    if (whole_num && any(!util_is_integer(x))) {
      return(FALSE)
    }
    if (any(x > max | x < min)) {
      return(FALSE)
    }
    if (!is.null(set)) {
      if (any(!(x %in% set))) {
        return(FALSE)
      }
    }
    TRUE
  }
}
