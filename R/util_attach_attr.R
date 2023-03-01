#' Attach attributes to an object and return it
#'
#' @param x the object
#' @param ... named arguments, each becomes an attributes
#'
#' @return `x`, having the desired attributes attached
util_attach_attr <- function(x, ...) {
  atts <- list(...)
  attributes(x)[names(atts)] <- atts
  x
}
