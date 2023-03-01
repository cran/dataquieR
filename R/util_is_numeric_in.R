#' Create a predicate function to check for certain numeric properties
#'
#' useful, e.g., for [util_expect_data_frame] and [util_expect_scalar].
#'
#' @param min if given, minimum for numeric values
#' @param max if given, maximum for numeric values
#' @param whole_num if TRUE, expect a whole number
#' @param set if given, a set, the value must be in (see [util_match_arg])
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
util_is_numeric_in <- function(min = -Inf,
                               max = +Inf,
                               whole_num = FALSE,
                               set = NULL) {
  function(x) {
    if (!is.numeric(x)) {
      return(FALSE)
    }
    if (whole_num && !util_is_integer(x)) {
      return(FALSE)
    }
    if (x > max || x < min) {
      return(FALSE)
    }
    if (!is.null(set)) {
      if (!(x %in% set)) {
        return(FALSE)
      }
    }
    TRUE
  }
}
