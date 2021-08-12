#' Attaches attributes about the recommended minimum
#' absolute sizes to the plot p
#'
#' @param p [ggplot] the plot
#' @param width_em [numeric] len=1. the minimum width hint in `em`
#' @param height_em [numeric] len=1. the minimum height in `em`
#'
#' @return p the modified plot
util_set_size <- function(p, width_em = NA_integer_, height_em = NA_integer_) {
  if (!ggplot2::is.ggplot(p)) {
    util_error("Internal error, p should be a ggplot object")
  }
  attr(p, "width_em") <- width_em
  attr(p, "height_em") <- height_em
  return(p)
}
