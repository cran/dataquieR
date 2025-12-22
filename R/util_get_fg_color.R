#' Find a foreground color for a background
#'
#' black or white
#'
#' @param cl colors
#'
#' @return black or white for each cl
#' @seealso [`stackoverflow.com`](https://stackoverflow.com/a/24810681)
#' @noRd
util_get_fg_color <- function(cl) {
  cl <- col2rgb(util_col2rgb(cl), alpha = TRUE)
  brightness <- cl["red", ] * 0.299 + cl["green", ] * 0.587 +
    cl["blue", ] * 0.114
  ifelse(brightness > 160, "#000000", "#ffffff")
}
