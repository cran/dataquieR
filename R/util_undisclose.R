#' Remove tables referred to by metadata and use `SVG` for most figures
#'
#' @param x an object to un-disclose
#' @param ... further arguments, used for pointing to the `dataquieR_result`
#'            object, if called recursively
#'
#' @return undisclosed object
#' @keywords internal
util_undisclose <- function(x, ...) {
  UseMethod("util_undisclose")
}

#' @export
util_undisclose.default <- function(x, ...) {
  if (is.atomic(x)) {
    return(x)
  }
  util_error("Internal error: object of class %s in report.",
             util_pretty_vector_string(quote = sQuote, class(x)))
}

#' @export
util_undisclose.dataquieR_resultset2 <- function(x, ...) {
  my_tabs <- lapply(setNames(nm = names(attr(x, "referred_tables"))),
                   function(dfn) {
                      data.frame(`NA` = paste(dQuote(dfn), "is not available."),
                                              check.names = FALSE)
                  })

  attr(x, "referred_tables")[] <- my_tabs

  x[] <- lapply(x, util_undisclose, ...)
  return(x)
}

#' @export
util_undisclose.dataquieR_result <- function(x, ...) {
  if (length(setdiff(class(x),
                     c("dataquieR_result", "list", "dataquieR_NULL",
                       "master_result", "Slot"))) > 0) {
    return(NextMethod())
  }
  dataquieR_result <- x
  if ("PlotlyPlot" %in% names(x)) {
    # class plotly
    if (any(endsWith(setdiff(names(x), "PlotlyPlot"), "Plot")) ||
        any(endsWith(setdiff(names(x), "PlotlyPlot"), "PlotList"))) {
      x$PlotlyPlot <- NULL
    } else {

      # ensure, sizing hint sticks at the dqr, only
      fixed <- util_fix_sizing_hints(dqr = dataquieR_result, x = x$PlotlyPlot)

      x$SummaryPlot <- try(util_plotly2svg_object(x$PlotlyPlot,
                                                  sizing_hints =
                                                    attr(fixed$dqr,
                                                         "sizing_hints")),
                            silent = TRUE)
      if (util_is_try_error(x$SummaryPlot)) {
        util_warning(
          c("Could not convert a plotly to an SVG or PNG for",
            "undisclosing data. Will delete an output slot. Maybe, a",
            "suggested package is missing: %s"), sQuote(conditionMessage(
              attr(x$SummaryPlot, "condition")
            )))
        x$SummaryPlot <- NULL
      }
      x$PlotlyPlot <- NULL
    }
  }
  x[] <- lapply(x, util_undisclose, dataquieR_result = dataquieR_result, ...)
  return(x)
}

#' @export
util_undisclose.list <- function(x, ...) {
  x[] <- lapply(x, util_undisclose, ...)
  return(x)
}

#' @export
util_undisclose.Slot <- function(x, ...) {
  if (length(setdiff(class(x),
                     c("dataquieR_result", "list", "dataquieR_NULL",
                       "master_result", "Slot"))) > 0) {
    return(NextMethod())
  }
  x[] <- lapply(x, util_undisclose, ...)

  return(x)
}

#' @export
util_undisclose.gg <- function(x, ...) {
  dataquieR_result <- list(...)[["dataquieR_result"]]
  if (util_is_svg_object(x)) {
    return(x)
  }
  fixed <- util_fix_sizing_hints(dqr = dataquieR_result, x = x)
  return(suppressWarnings(util_plot2svg_object(x, sizing_hints =
                                                 attr(fixed$dqr,
                                                      "sizing_hints"))))
}

#' @export
util_undisclose.ggmatrix_plot_obj <- util_undisclose.gg

#' @export
util_undisclose.ggmatrix_fn_with_params <- util_undisclose.gg

#' @export
util_undisclose.ggplot_built <- util_undisclose.gg

#' @export
util_undisclose.data.frame <- function(x, ...) {
  return(x)
}

#' Remove data disclosing details
#'
#' new function: no warranty, so far.
#'
#' @param x an object to un-disclose, a
#'
#' @return undisclosed object
#' @export
prep_undisclose <- function(x) {
  if (!(inherits(x, "dataquieR_resultset2") ||
        inherits(x, "dataquieR_result"))) {
    util_error("%s works for results or reports, only",
               sQuote("prep_undisclose")
               )
  }
  util_message("%s comes without any warranty, so far",
               sQuote("prep_undisclose"));
  suppressMessages(util_undisclose(x))
}
