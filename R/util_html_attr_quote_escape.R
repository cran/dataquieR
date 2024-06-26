#' escape `"`
#'
#' @param s haystack
#'
#' @return `s` with `"` replaced by `&quot;`
#'
#' @concept reporting
#' @keywords internal

util_html_attr_quote_escape <- function(s) {
  gsub('"', "&quot;", s, fixed = TRUE)
}
