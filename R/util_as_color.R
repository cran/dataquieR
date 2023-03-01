#' convert a character to a specific ordered factor
#'
#' @param x color name
#'
#' @return factor
util_as_color <- function(x) {
  factor(x, levels = c("green", "yellow", "red"), ordered = TRUE)
}
