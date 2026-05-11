#' Sort a vector by order given in some other vector
#'
#' @param x the vector
#' @param order the "order vector
#' @param ... additional arguments passed to `sort`
#'
#' @examples
#' \dontrun{
#' util_sort_by_order(c("a", "b", "a", "c", "d"), letters)
#' }
#'
#' @family summary_functions
#' @concept reporting
#' @noRd

util_sort_by_order <- function(x, order, ...) {
  x <- ordered(x, order)
  as.character(sort(x, ...))
}

#' Get the order of a vector with general order given in some other vector
#'
#' @param x the vector
#' @param order the "order vector
#' @param ... additional arguments passed to `order`
#'
#' @examples
#' \dontrun{
#' util_order_by_order(c("a", "b", "a", "c", "d"), letters)
#' }
#' @family reporting_functions
#' @concept summary
#' @noRd
util_order_by_order <- function(x, order, ...) {
  x <- ordered(x, order)
  order(x, ...)
}

#' Get sub-string left from first `.`
#'
#' @param x the string with a least one `.`
#'
#' @examples
#' \dontrun{
#' util_sub_string_left_from_.(c("a.b", "asdf.xyz", "asdf.jkl.zuio"))
#' }
#'
#' @family string_functions
#' @concept reporting
#' @noRd
util_sub_string_left_from_. <- function(x) {
  pos <- regexpr(".", x, fixed = TRUE)

  util_stop_if_not(
    "there must be at least one dot to separate the col. name from the row name (wolog this is the first dot)" =
      all(pos > 0)
  )

  substring(x, 1L, pos - 1L)
}
#' Get sub-string right from first `.`
#'
#' @param x the string with a least one `.`
#'
#' @examples
#' \dontrun{
#' util_sub_string_right_from_.(c("a.b", "asdf.xyz"))
#' util_sub_string_right_from_.(c("a.b", "asdf.xy.z"))
#' util_sub_string_right_from_.(c("ab", "asdxy.z"))
#' util_sub_string_right_from_.(c("a.b", "asdxy.z."))
#' util_sub_string_right_from_.(c("a.b", "asdxy.z.."))
#' }
#'
#' @family string_functions
#' @concept reporting
#' @noRd
util_sub_string_right_from_. <- function(x) {
  pos <- regexpr(".", x, fixed = TRUE)

  util_stop_if_not(
    "there must be at least one dot to separate the col. name from the row name (wolog this is the first dot)" =
      all(pos > 0)
  )

  substring(x, pos + 1L)
}
