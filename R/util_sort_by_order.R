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
  splitted <- strsplit(x, ".", fixed = TRUE)
  util_stop_if_not(
    "there must be at least one dot to separated the col. name from the row name (wolog this is the first dots)" =
      all(vapply(splitted, length, FUN.VALUE = integer(1)) >= 2))
  #util_stop_if_not(all(grepl(fixed = TRUE, ".", x))) # there must be at least one dot to separated the col. name from the row name (wolog this is the first dots)
  vapply(splitted, `[[`, 1, FUN.VALUE = character(1))
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
#' }
#'
#' @family string_functions
#' @concept reporting
#' @noRd
util_sub_string_right_from_. <- function(x) {
  splitted <- strsplit(x, ".", fixed = TRUE)
  util_stop_if_not(
    "there must be at least one dot to separated the col. name from the row name (wolog this is the first dots)" =
      all(vapply(splitted, length, FUN.VALUE = integer(1)) >= 2))
  #util_stop_if_not(all(grepl(fixed = TRUE, ".", x))) # there must be at least one dot to separated the col. name from the row name (wolog this is the first dots)
  vapply(lapply(splitted, `[`, -1),
         paste0, collapse = ".", FUN.VALUE = character(1))

}
