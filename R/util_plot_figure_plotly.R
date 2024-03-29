#' Plot a `ggplot2` figure using `plotly`
#'
#' @param x [ggplot2] object
#'
#' @return `htmltools` compatible object
#'
#' @keywords internal
util_plot_figure_plotly <- function(x) {
  withCallingHandlers({
    if (inherits(x, "ggplot") && inherits(x, "patchwork")) {
      x <- htmltools::plotTag(x, "", width = 640) # leave patchworks as is, so far: TODO!!
    } else if (inherits(x, "ggplot") || inherits(x, "ggmatrix")) {
      py <- attr(x, "py")
      if (is.null(py)) {
        x <- util_remove_dataquieR_result_class(x)
        # x <- x + ggplot2::theme(aspect.ratio = NULL) # prevent a warning
        py <- suppressWarnings( # prevent a warning
          force(plotly::ggplotly(x)))#, height = 480, width = 1040)))
        py <- util_adjust_geom_text_for_plotly(py)
      }
      x <- plotly::layout(py,
                          autosize = !FALSE,
                          margin = list(autoexpand = !FALSE,
                                        r = 200,
                                        b = 100)
      )
      x <- plotly::config(x,
                          responsive = TRUE,
                          autosizable = TRUE,
                          fillFrame = TRUE,
                          modeBarButtonsToAdd =
                             list(
                               list(
                                 name = "Limit by Window Size",
                                 icon =
                                   htmlwidgets::JS("Plotly.Icons.drawrect"),
                                 click =
                                   htmlwidgets::JS(
                                     paste(sep = "\n",
                                       "function() {",
                                       "   if (togglePlotlyWindowZoom instanceof Function) {",
                                       "     togglePlotlyWindowZoom()",
                                       "   }",
                                       "}"
                                      )
                                    ))))
    }
  }, # nocov start
  warning = function(cond) { # suppress a waning caused by ggplotly for barplots
    if (startsWith(conditionMessage(cond),
                   "'bar' objects don't have these attributes: 'mode'")) {
      invokeRestart("muffleWarning")
    }
    if (any(grepl("the mode", conditionMessage(cond)))) {
      invokeRestart("muffleWarning")
    }
  },
  message = function(cond) { # suppress a waning caused by ggplotly for barplots
    if (startsWith(conditionMessage(cond),
                   "'bar' objects don't have these attributes: 'mode'")) {
      invokeRestart("muffleMessage")
    }
    if (any(grepl("the mode", conditionMessage(cond)))) {
      invokeRestart("muffleMessage")
    } # nocov end
  })
  x
}
