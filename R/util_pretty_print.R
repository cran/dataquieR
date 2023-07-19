#' Convert single `dataquieR` result to an `htmltools` compatible object
#'
#' @param dqr [dataquieR_result] an output (indicator) from `dataquieR`
#' @param nm [character] the name used in the report, the alias name of the
#'                       function call plus the variable name
#' @param is_single_var [logical] we are creating a single variable overview
#'                                page or an indicator summary page
#' @param meta_data [meta_data]  the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param use_plot_ly [logical] use `plotly`
#'
#' @return `htmltools` compatible object with rendered `dqr`
util_pretty_print <- function(dqr, nm, is_single_var,
                              meta_data,
                              label_col,
                              use_plot_ly
                              ) { # TODO: ensure that in square2 alias names do not have points
  if (use_plot_ly) {
    plot_figure <- util_plot_figure_plotly
  } else {
    plot_figure <- util_plot_figure_no_plotly
  }
  fkt <- util_map_by_largest_prefix(
    nm,
    haystack = util_all_ind_functions())
  # if (dynGet("fkt") == "acc_loess_observer") browser()
  if (!inherits(dqr, "dataquieR_NULL")) { # if the function did not return NULL or trigger an error
    # is there a filter on the implementations sheet?
    outputs <- util_get_concept_info("implementations", get("function_R")
                                     == fkt, "Reportoutputs")[["Reportoutputs"]]
    if (length(outputs) == 1 && !util_empty(outputs)) {
      outputs <- names(util_parse_assignments(outputs))
      remaining_slots <- intersect(outputs, names(dqr))
      if (length(remaining_slots) == 0) {
        util_message(
          "Removed all outputs of %s according to %s",
          dQuote(fkt),
          sQuote("concept | implementations | Reportoutputs"),
          level = 10
        )
      }
      dqr <- dqr[remaining_slots]
    }

    slot <- # stores the name of the result object returned by the indicator function being displayed
      head(intersect( # check if the preferred slots are in the result names
        preferred_slots, names(dqr)), 1) # take the first available result name according to the preferred slots

    if (fkt %in% c(
      "con_limit_deviations",  # check if we are working with a limits function
      "con_hard_limits",
      "con_soft_limits",
      "con_detection_limits"
    )) {
      if ("SummaryPlotList" %in% names(dqr)) { # add an exception, the histogram is preferred to the table (heat map)
        slot <- "SummaryPlotList"
      }
    }
    if (length(slot) > 0) { # if there is an output, check and collect all warnings/errors/messages
      errors <- attr(dqr, "error") # get and store the errors from the output
      warnings <- attr(dqr, "warning")
      messages <- attr(dqr, "message")

      errors <- errors[!vapply( # do not show errors about calls, that would not be possible at all (e.g., loess for only categorical scales)
        lapply(errors, attr, "intrinsic_applicability_problem"),
        identical,
        TRUE,
        FUN.VALUE = logical(1)
      )]

      warnings <- warnings[!vapply( # do not show errors about calls, that would not be possible at all (e.g., loess for only categorical scales)
        lapply(warnings, attr, "intrinsic_applicability_problem"),
        identical,
        TRUE,
        FUN.VALUE = logical(1)
      )]

      messages <- messages[!vapply( # do not show errors about calls, that would not be possible at all (e.g., loess for only categorical scales)
        lapply(messages, attr, "intrinsic_applicability_problem"),
        identical,
        TRUE,
        FUN.VALUE = logical(1)
      )]

      # extract the condition messages, and ensure that multi-line messages are in a single line
      errors <- vapply(lapply(errors, conditionMessage), paste, #. TODO: Use conditionMessage
                       FUN.VALUE = character(1))
      warnings <- vapply(lapply(warnings, conditionMessage), paste,
                         FUN.VALUE = character(1))
      messages <- vapply(lapply(messages, conditionMessage), paste,
                         FUN.VALUE = character(1))
      x <- dqr[[slot]] # extract the corresponding result according to slot

      x <- util_remove_dataquieR_result_class(x)

      if (endsWith(slot, "PlotList")) {
        if (length(x) > 1) { # check and warn if there is more than one entry in PlotList
          util_warning(
            c("Internal error: %s with > 1 result should not be in a",
              "v2.0 report"),
            sQuote(slot))
          # TODO: Implement a work-around
          # x <- do.call(htmltools::div, x)
          x <- htmltools::div("%s should not contain > 1 result",
                              sQuote(slot))
        } else {
          if (length(x) == 1) {
            x <- x[[1]] # take the single plot from PLotList
          } else {
            x <- NULL
          }
        }
      }
      # check the class of x
      if (inherits(x, "ReportSummaryTable")) {
        cats <- setdiff(colnames(x), c("Variables", "N"))
        ncats <- length(cats)
        vars <- unique(x$Variables)
        nvars <- length(vars)
        if (ncats == 0 && nvars == 0) {
          x <- NULL
        } else if (ncats == 0 || nvars == 0) {
          x <- NULL
        } else if (ncats == 1 && nvars == 1) {
          val <- x[x$Variables == vars, cats, drop = TRUE]
          level_names <- attr(x, "level_names")
          if (!!length(level_names)) {
            val <- level_names[as.character(val)]
          }
          x <- htmltools::p(sprintf("%s -- %s: %s.",
                            cats,
                            dQuote(vars),
                            val
                            ))
        } else {
          x <- print.ReportSummaryTable(x, view = FALSE) # convert to ggplot
        }
      }
      if (inherits(x, "ggplot")) {
        x_is_plot <- TRUE
        as_plotly <- attr(dqr, "as_plotly")
        if (!is.null(as_plotly) && exists(as_plotly, mode = "function"))
          as_plotly <- get(as_plotly, mode = "function")
        if (use_plot_ly && is.function(as_plotly)) {
          withCallingHandlers({
            x <- as_plotly(dqr, height = 480, width = 1040)
            x <- util_adjust_geom_text_for_plotly(x)
            x <- plotly::layout(x,
                                autosize = FALSE,
                                margin = list(autoexpand = FALSE,
                                              r = 200,
                                              b = 100)
            )
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
        } else {
          x <- plot_figure(x)
        }
        # convert to plotly or base 64 plot image
      } else {
        x_is_plot <- FALSE
      }
      if (is.data.frame(x)) {
        # check if dataframe is empty
        if (!prod(dim(x))) {
          x <- NULL
        } else {
          rownames(x) <- NULL # TODO: Check, if this is okay, always
          x <- util_html_table(util_table_rotator(x),
                               meta_data = meta_data,
                               label_col = label_col,
                               output_format = "HTML",
                               dl_fn = nm)
        }
      }
      if (length(x) > 0) {
        if (!inherits(x, "htmlwidget") && # check if output is compatible with htmltools
            !inherits(x, "shiny.tag") &&
            !inherits(x, "shiny.tag.list")) {
          x <- htmltools::p(
            sprintf(paste("Cannot display objects of class(es) %s, yet.",
                          "Please file a feature request."),
                    paste(dQuote(class(x)), collapse = ", ")))
        }
        cll <- attr(dqr, "call")
        if (is.language(cll)) {
          cll <- deparse(cll)
        }
        x <- htmltools::div(
          title = #htmltools::htmlEscape( # this is the text for the hover messages
            "", # paste(errors, warnings, messages, collapse = "\n"),
          #attribute = TRUE),
          class = "dataquieR_result",
          `data-call` = paste0(cll, collapse = "\n"),
          `data-stderr` = paste(errors, warnings, messages, collapse = "\n"),
          x
        )
      } else {
        x <- NULL
      }
    } else {
      x <- NULL
    }
  } else {
    x <- NULL
  }
  if (!is.null(x)) { # create and add tags and links
    if (all(grepl(".", nm, fixed = TRUE))) { # get the full name, which includes a dot
      anchor <- util_generate_anchor_tag(name = nm,
                                         order_context =
                                           ifelse(
                                             is_single_var,
                                             "variable",
                                             "indicator")
      )
      # the link is most easily created here, but therefore in the wrong position, so later it must be moved
      link <-  util_generate_anchor_link(name = nm,
                                         order_context =
                                           ifelse(
                                             is_single_var,
                                             "variable",
                                             "indicator")
      )
    } else {
      anchor <- NULL
      link <- NULL
    }

    # add variable name to pages that are not single_vars and display multiple variable in same page
    if (!is_single_var && all(grepl(".", nm, fixed = TRUE))) {
      caption <- sub("^[^\\.]*\\.", "", nm)
      if (caption != "[ALL]") {
        caption <- htmltools::h5(caption)
      } else {
        caption <- NULL
      }
    } else {
      caption <- NULL
    }
    slot2 <- # stores the name of the result object returned by the indicator function being displayed
      head(intersect( # check if the preferred slots are in the result names
        setdiff(preferred_summary_slots,
                slot), names(dqr)), 1) # take the first available result name according to the preferred slots
    if (length(slot2) < 1) {
      slot2 <- NULL
    } else {
      slot2 <- slot2[[1]]
    }
    if (length(slot2)) {
      y <- dqr[[slot2]]
      if (!x_is_plot)
        y <- NULL
      if (is.data.frame(y)) {
        # check if dataframe is empty
        if (!prod(dim(y))) {
          y <- NULL
        } else {
          if (endsWith(slot2, "Table")) {
            y <- util_make_data_slot_from_table_slot(y)
          }
          rownames(y) <- NULL # TODO: Check, if this is okay, always
          y <- util_html_table(util_table_rotator(y),
                               meta_data = meta_data,
                               label_col = label_col,
                               output_format = "HTML",
                               dl_fn = nm)
        }
      }
      if (length(y) > 0) {
        if (!inherits(y, "htmlwidget") && # check if output is compatible with htmltools
            !inherits(y, "shiny.tag") &&
            !inherits(y, "shiny.tag.list")) {
          y <- htmltools::p(
            sprintf(paste("Cannot display objects of class(es) %s, yet.",
                          "Please file a feature request."),
                    paste(dQuote(class(y)), collapse = ", ")))
        }
        cll <- attr(dqr, "call")
        if (is.language(cll)) {
          cll <- deparse(cll)
        }
        y <- htmltools::div(
          title = #htmltools::htmlEscape( # this is the text for the hover messages
            "", # paste(errors, warnings, messages, collapse = "\n"),
          #attribute = TRUE),
          class = "dataquieR_result",
          `data-call` = paste0(cll, collapse = "\n"),
          `data-stderr` = paste(errors, warnings, messages, collapse = "\n"),
          y
        )
      } else {
        y <- NULL
      }
    } else {
      y <- NULL
    }
    x <- htmltools::tagList(anchor = anchor, link = link, caption, x, y)
    # the link is most easily added here, but still in the wrong position, so later it must be moved
  }
  x
}

# preferred order of the content of the report
preferred_slots <- c("ReportSummaryTable",
                     "SummaryPlot", "SummaryPlotList", "SummaryData",
                     "SummaryTable",
                     "DataframeData", "DataframeTable", "SegmentData",
                     "SegmentTable",
                     "VariableGroupData", "VariableGroupTable")


# preferred order of the content of the report
preferred_summary_slots <- c("SummaryData",
                     "SummaryTable",
                     "DataframeData", "DataframeTable", "SegmentData",
                     "SegmentTable",
                     "VariableGroupData", "VariableGroupTable")

