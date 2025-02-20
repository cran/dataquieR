#' Plot a `ggplot2` figure using `plotly`
#'
#' @param x [ggplot2::ggplot2] object
#' @param sizing_hints `object` additional metadata about the natural figure size
#'
#' @return `htmltools` compatible object
#'
#' @keywords internal
util_plot_figure_plotly <- function(x, sizing_hints = NULL) {
  withCallingHandlers({
    if (inherits(x, "ggplot") && inherits(x, "patchwork")) {
      x <- htmltools::plotTag(x, "", width = 640) # leave patchworks as is, so far: TODO!!
    } else if ((inherits(x, "ggplot") || inherits(x, "ggmatrix")) &&
               !util_is_svg_object(x)) {
      title <- NULL
      subtitle <- NULL
      try({
        title <- ggplot2::ggplot_build(x)$plot$labels$title
      }, silent = TRUE)
      try({
        subtitle <- ggplot2::ggplot_build(x)$plot$labels$subtitle
      }, silent = TRUE)
      py <- attr(x, "py")
      if (is.null(py)) {
        x <- util_remove_dataquieR_result_class(x)
        # x <- x + ggplot2::theme(aspect.ratio = NULL) # prevent a warning
        py <- suppressWarnings( # prevent a warning
          force(plotly::ggplotly(x)))#, height = 480, width = 1040)))
        # TODO: only, if attr(dqr, "dont_util_adjust_geom_text_for_plotly") is not TRUE
        py <- util_adjust_geom_text_for_plotly(py)
      }
      x <- plotly::layout(py,
                          autosize = !FALSE,
                          margin = list(autoexpand = !FALSE,
                                        r = 200,
                                        b = 100)
      )
      if (!is.null(title) && is.null(subtitle)) {
        x <- plotly::layout(x, title = title)
      } else if (!is.null(title) && !is.null(subtitle)) {
        # if (packageVersion("plotly") > "4.10.4") { # should be supported, soon
        #   x <- plotly::layout(x, title = list(text = title,
        #                                       subtitle = list(text = subtitle)))
        # } else {
          x <- plotly::layout(x, title = list(text = paste0(title,
                                                            "<br />",
                                                            "<sub>",
                                                            subtitle,
                                                            "</sub>")))
        # }
      }
      x <- plotly::config(x,
                          responsive = TRUE,
                          autosizable = TRUE,
                          fillFrame = TRUE,
                          modeBarButtonsToAdd =
                             list(
                               list(
                                 name = "Restore initial Size",
                                 icon =
                                   htmlwidgets::JS("PlotlyIconshomeRED()"),
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
    } else {
      x <- util_plot_figure_no_plotly(x = x, sizing_hints = sizing_hints)
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

#' Return the pre-computed `plotly` from a `dataquieR` result
#'
#' @param res the `dataquieR` result
#' @param ... not used
#'
#' @return a `plotly` object
#' @family plotly_shims
#' @concept plotly_shims
#' @keywords internal
util_as_plotly_from_res <- function(res, ...) {
  util_stop_if_not(
    "Internal error, sorry: res w/o PlotlyPlot in util_as_plotly_from_res(), please report" =
      "PlotlyPlot" %in% names(res))
  have_plot_ly = util_ensure_suggested("plotly", "plot interactive figures", err = FALSE)
  util_stop_if_not("Internal error, sorry: plotly uninstalled in util_as_plotly_from_res(), please report" =
                      have_plot_ly)
  py <- res[["PlotlyPlot"]]
  util_stop_if_not("Internal error in util_as_plotly_from_res(), sorry: PlotlyPlot should be a plotly, please report" =
                     inherits(py, "plotly"))
  py
}
