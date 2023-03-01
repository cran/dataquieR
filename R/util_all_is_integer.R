#' convenience function to abbreviate all(util_is_integer(...))
#'
#' @inheritParams util_is_integer
#'
#' @return `TRUE`, if all entries are integer-like, `FALSE` otherwise
util_all_is_integer <- function(x) {
  all(util_is_integer(x))
}
