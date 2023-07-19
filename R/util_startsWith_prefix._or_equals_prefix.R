#' String check for results/combined results
#'
#' detect, if `x` starts with `<prefix>.` or equals `<prefix>`,
#' if results have been combined
#'
#' @param x [character] haystack
#' @param prefix [character] needle
#'
#' @return [logical] if entries in x start with prefix-DOT/equal to prefix
util_startsWith_prefix._or_equals_prefix <- function(x, prefix) { # TOOD: Deprecate
  startsWith(x, paste0(prefix, ".")) || x == prefix
}
