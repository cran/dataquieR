#' Plot a `ggplot2` figure without `plotly`
#'
#' @param x [ggplot2::ggplot2] object
#' @param sizing_hints `object` additional metadata about the natural figure size
#'
#' @return `htmltools` compatible object
#'
#' @keywords internal
util_plot_figure_no_plotly <- function(x, sizing_hints = NULL) {
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
    w <- 800
    h <- 600
    if (!is.null(sizing_hints)) {
      sizing_hints <- util_finalize_sizing_hints(sizing_hints = sizing_hints)
      if (!is.null(sizing_hints$w_in_cm) &&
          !is.null(sizing_hints$h_in_cm)) {
        w <- sizing_hints$w_in_cm / 2.54 * 72
        h <- sizing_hints$h_in_cm / 2.54 * 72
      }
    }
    x <- util_plot_svg_to_uri(x, w = w, h = h)
  } else {
    x <- htmltools::plotTag(x, "", suppressSize = "xy", attribs =
                              list(class = "dataquieRfigure"))#, width = 640)
  }
  x
}
