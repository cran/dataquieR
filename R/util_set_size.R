#' Attaches attributes about the recommended minimum
#' absolute sizes to the plot p
#'
#' @param p [ggplot2::ggplot] the plot
#' @param width_em [numeric] len=1. the minimum width hint in `em`
#' @param height_em [numeric] len=1. the minimum height in `em`
#'
#' @return p the modified plot
#'
#' @family reporting_functions
#' @concept figure
#' @noRd
util_set_size <- function(p, width_em = NA_integer_, height_em = NA_integer_) {
  if (!util_is_gg(p)) {
    util_error("Internal error, p should be a ggplot object, not: %s",
               util_pretty_vector_string(class(p)))
  }
  attr(p, "width_em") <- width_em
  attr(p, "height_em") <- height_em
  return(p)
}
