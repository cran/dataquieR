#' Plot a `ggplot2` figure using `plotly`
#'
#' @param x [ggplot2] object
#'
#' @return `htmltools` compatible object
util_plot_figure_plotly <- function(x) {
  withCallingHandlers({
    if (inherits(x, "ggplot") && inherits(x, "patchwork")) {
      x <- htmltools::plotTag(x, "", width = 640) # leave patchworks as is, so far: TODO!!
    } else if (inherits(x, "ggplot")) {
      x <- util_remove_dataquieR_result_class(x)
      x <- x + ggplot2::theme(aspect.ratio = NULL) # prevent a warning
      py <- force(plotly::ggplotly(x, height = 480, width = 1040))
      py <- util_adjust_geom_text_for_plotly(py)
      x <- plotly::layout(py,
                          autosize = FALSE,
                          margin = list(autoexpand = FALSE,
                                        r = 200,
                                        b = 100)
      )
    }
  },
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
    }
  })
  x
}
