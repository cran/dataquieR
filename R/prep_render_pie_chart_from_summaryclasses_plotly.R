#' Create a `plotly` pie chart
#'
#' @param data data as returned by `prep_summary_to_classes` but
#'             summarized by one column (currently, we support
#'             `indicator_metric`, `call_names`,
#'              `STUDY_SEGMENT`, and `VAR_NAMES`)
#' @param meta_data [meta_data]
#'
#' @return a `htmltools` compatible object
#'
#' @family summary_functions
#' @export
prep_render_pie_chart_from_summaryclasses_plotly <- function(data, # FIXME: If use_plotly is FALSE? also: support some way for dq_report_by to switch plotly off
                                                      meta_data = "item_level") {
# FIXME: Amend prep_render_pie_chart_from_summaryclasses_ggplot2 to handle this also.
  if (!util_ensure_suggested(c("plotly", "htmltools"), err = FALSE)) {
    return(NULL)
  }

  if (nrow(data) == 0) {
    return(htmltools::browsable(htmltools::HTML("")))
  }

  grouped_by <- setdiff(colnames(data), c("class", "value", "percent", "note"))

  util_stop_if_not(length(grouped_by) == 1)

  groups <- unique(data[[grouped_by]])

  util_stop_if_not(length(groups) > 0)

  if (length(groups) > 1) {
    all_pys <- lapply(setNames(nm = groups), function(g) {
      prep_render_pie_chart_from_summaryclasses_plotly(
        data[data[[grouped_by]] == g, , FALSE], meta_data = meta_data)
    })
    # pys <-
    #   do.call(plotly::subplot, lapply(all_pys, plotly::subplot))
    # py <- plotly::layout(pys,
    #                # title = list(text =
    #                #                res$SummaryPlot$patches$annotation$title),
    #                margin = 0.01)
    ncols <- min(2, ceiling(sqrt(length(all_pys))))
    nrows <- ceiling(length(all_pys) / ncols)

    pys_matrix <- htmltools::tags$table(lapply(
      seq_len(nrows),
      function(rw) {
        if ((rw - 1) * ncols + 1 <= length(all_pys)) { # current row needed?
          cur_tr_as_list <- lapply(seq_len(ncols), function(rw, cl) {
            wch <- (rw - 1) * ncols + cl
            if (wch <= length(all_pys)) {
              o <- htmltools::tags$td(all_pys[wch])
            } else {
              o <- htmltools::tags$td()
            }
            o
          }, rw = rw)
          do.call(htmltools::tags$tr, cur_tr_as_list)
        } else { # should never be reached
          NULL
        }
      }
    ))

    py <- pys_matrix

    py <- htmltools::tags$table(py)
    py <- htmltools::browsable(py)
    return(py)
  }

  py_colors <- util_get_colors()
  py_colors["NA"] <- "lightgrey"
  labs <- util_get_labels_grading_class()
  labs["NA"] <- "Not classified"

  if (all(is.na(data$class))) {
    return(htmltools::browsable(htmltools::HTML("")))
  }

  if (is.factor(data$class)) {
    data$class <- as.integer(gsub("^cat", "", data$class))
  }
  data$class <- factor(data$class,
                       levels = names(py_colors),
                       ordered = TRUE
  )

  data <- data[order(data$class, -data$value, decreasing = TRUE), , FALSE]

  hoverinfo <- "label+percent+value"

  if ("note" %in% colnames(data)) {
    hoverinfo <- paste0(hoverinfo, "+text")
  } else {
    data$note <- ""
  }

  if (length(groups) > 1) { # IDEA: sunburst would maybe work here, if we have a hiearchy in groups, currently, we do not have that.
    # Sunburst:
    # py <- plotly::plot_ly(
    #   ids = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
    #   labels = paste(c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"), 42),
    #   parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
    #   values = c(10, 14, 12, 10, 2, 6, 6, 4, 4),
    #   type = 'sunburst'
    # )
  } else {
    py <- plotly::add_pie(plotly::plot_ly(data),
                          sort = FALSE,
                          rotation = 90,
                          labels = labs[
                            paste(data$class)],
                          values = data$value,
                          hovertext = data$note,
                          #                name = "Segment",
                          #                hoverinfo = "label+percent+name",
                          hoverinfo = hoverinfo,
                          textinfo = 'label+percent+value',
                          marker = list(
                            colors = py_colors[paste(data$class)]))
  }

  if (identical(grouped_by, "indicator_metric")) { # TODO: Hiearchical structure support
    title <- util_translate_indicator_metrics(groups, short = FALSE,
                                              long = FALSE)
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, "call_names")) { # TODO: Hiearchical structure support
    title <- util_map_labels(groups,
                            util_get_concept_info("implementations"),
                            to = "dq_report2_short_title",
                            from = "function_R",
                            ifnotfound = NA_character_)
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character(STUDY_SEGMENT))) { # TODO: Hiearchical structure support
    title <- groups
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character(VAR_NAMES))) {
    util_expect_data_frame(meta_data)
    title <- prep_get_labels(groups,
                             meta_data = meta_data,
                             label_class = "LONG")
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character("function_name"))) {
    title <- vapply(groups, util_alias2caption, long = TRUE,
                    FUN.VALUE = character(1))
    subtitle <- groups
  } else {
    title <- groups
    subtitle <- "percentage of QA classes"
    #subtitle <- NULL
#    util_error("Unkown grouping by %s", sQuote(grouped_by))
  }

  subtitle <- paste(subtitle, sprintf(" -- %d of %d variables classified",
                                        sum(data$value, na.rm = TRUE),
                                        nrow(meta_data)
                                      )) # TODO: Maybe, we should not compute this here, but earlier.

  py <- plotly::layout(py,
               title =
                 list(
                   text =
                     as.character(htmltools::tagList(
                       title,
                       htmltools::tags$sup(subtitle)
                     ))
                 ),
               margin = list(
                 t = 40
               )
            )

  py <- htmltools::span(
    title = groups,
    `data-tippy-always-on` = "true",
    py
  )

  py <- htmltools::browsable(
    py
  )

  py
}
