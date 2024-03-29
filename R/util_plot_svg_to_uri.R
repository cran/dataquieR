#' Replacement for `htmltools::plotTag`
#'
#' the function is specifically designed for fully scalable `SVG` figures.
#'
#' @param expr plot expression
#'
#' @return `htmltools` compatible object
#' @keywords internal
util_plot_svg_to_uri <- function(expr) {
  tmpfil <- NULL
  withr::with_tempfile("tmpfil", fileext = ".svg", {
    htmltools::capturePlot(expr = {
      rlang::eval_tidy(expr)
    },
    filename = tmpfil,
    device = grDevices::svg,
    width = 800 / 72, height = 600 / 72
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
