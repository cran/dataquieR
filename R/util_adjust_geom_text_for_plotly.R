#' Place all geom_texts also in `plotly` right from the x position
#'
#' @param plotly the `plotly`
#'
#' @return modified `plotly`-built object
util_adjust_geom_text_for_plotly <- function(plotly) {
  util_ensure_suggested("plotly")
  util_stop_if_not(inherits(plotly, "plotly"))
  withCallingHandlers(
    pyb <- plotly::plotly_build(plotly),
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

  no_type <-
    vapply(lapply(pyb$x$data, `[[`, "type"), is.null, FUN.VALUE = logical(1))
  type <- rep(NA_character_, length(no_type))
  type[!no_type] <-
    vapply(pyb$x$data[!no_type], `[[`, "type", FUN.VALUE = character(1))
  type_scatter <- rep(FALSE, length(no_type))
  type_scatter[!no_type] <-
    vapply(type[!no_type], `==`, "scatter", FUN.VALUE = logical(1))

  no_mode <-
    vapply(lapply(pyb$x$data, `[[`, "mode"), is.null, FUN.VALUE = logical(1))
  mode <- rep(NA_character_, length(no_mode))
  mode[!no_mode] <-
    vapply(pyb$x$data[!no_mode], `[[`, "mode", FUN.VALUE = character(1))
  mode_text <- rep(FALSE, length(no_mode))
  mode_text[!no_mode] <-
    vapply(mode[!no_mode], `==`, "text", FUN.VALUE = logical(1))

  # TODO: for ggvenn, we would need textposition = "middle center" - get from hjust and vjust in $hovertext of pyb$x?
  withCallingHandlers(
    plotly::style(pyb, textposition = "right", traces = mode_text & type_scatter),
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
}
