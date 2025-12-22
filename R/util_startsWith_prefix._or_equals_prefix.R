#' String check for results/combined results
#'
#' detect, if `x` starts with `<prefix>.` or equals `<prefix>`,
#' if results have been combined
#'
#' @param x [character] haystack
#' @param prefix [character] needle
#' @param sep [character] separation string
#'
#' @return [logical] if entries in x start with prefix-DOT/equal to prefix
#'
#' @noRd
util_startsWith_prefix._or_equals_prefix <- function(x, prefix, sep = ".") {
  startsWith(x, paste0(prefix, sep)) || x == prefix
}
