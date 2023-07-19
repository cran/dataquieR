#' Plot a `ggplot2` figure without `plotly`
#'
#' @param x [ggplot2] object
#'
#' @return `htmltools` compatible object
util_plot_figure_no_plotly <- function(x) {
  x <- htmltools::plotTag(x, "", width = 640)
  x
}
