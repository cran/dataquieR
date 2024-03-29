#' Plot a `ggplot2` figure without `plotly`
#'
#' @param x [ggplot2] object
#'
#' @return `htmltools` compatible object
#'
#' @keywords internal
util_plot_figure_no_plotly <- function(x) {
  if (capabilities("cairo") && suppressWarnings(util_ensure_suggested("grDevices",
                                                     "render vector graphics",
                                                     err = FALSE))) {
    # x <- htmltools::plotTag(x, "", suppressSize = "xy", attribs =
    #           list(class = "dataquieRfigure"),
    #           device = grDevices::svg, mimeType = "image/svg+xml",
    #           # width = 800, height = 600,
    #           pixelratio = 1/72
    # )
    # # preserveAspectRatio="none"
    # x$attribs$src
    x <- util_plot_svg_to_uri(x)
  } else {
    x <- htmltools::plotTag(x, "", suppressSize = "xy", attribs =
                              list(class = "dataquieRfigure"))#, width = 640)
  }
  x
}
