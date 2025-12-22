#' Replacement for `htmltools::plotTag`
#'
#' the function is specifically designed for fully scalable `SVG` figures.
#'
#' @param expr plot expression
#' @param w width
#' @param h height
#'
#' `w` and `h` are mostly used for the relation of fixed text sizes to
#' the figure size.
#'
#' @return `htmltools` compatible object
#' @noRd
util_plot_svg_to_uri <- function(expr, w = 800, h = 600) {
  tmpfil <- NULL
  withr::with_tempfile("tmpfil", fileext = ".svg", {
    htmltools::capturePlot(expr = {
      rlang::eval_tidy(expr)
    },
    filename = tmpfil,
    device = grDevices::svg,
    width = w / 72, height = h / 72,
#    pointsize = 12
#    res = 1/72
    )
    .svg <- readLines(tmpfil)
    .svg <- gsub("<svg ", '<svg preserveAspectRatio="none" ', .svg, fixed = TRUE)
    writeLines(.svg, tmpfil)
    b64 <- R.devices::asDataURI(tmpfil,
                         mime = "image/svg+xml")
    htmltools::browsable(htmltools::tags$img(src = b64,
                                             class = "dataquieRfigure"))
  })
}
