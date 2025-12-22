#' Create a `ggplot2` pie chart
#'
#' needs `htmltools`
#'
#' @param data data as returned by `prep_summary_to_classes` but
#'             summarized by one column (currently, we support
#'             `indicator_metric`, `STUDY_SEGMENT`, and `VAR_NAMES`)
#' @param meta_data [meta_data]
#'
#' @return a `htmltools` compatible object or `NULL`, if package is missing
#'
#' @family summary_functions
#' @export
prep_render_pie_chart_from_summaryclasses_ggplot2 <- function(data,
                                                      meta_data = "item_level") {
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft("2.1.0.9007",
                              "prep_render_pie_chart_from_summaryclasses_ggplot2()",
                              "plot.dataquieR_summary()")
  }
  if (!util_ensure_suggested(c("htmltools"), err = FALSE)) {
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
      prep_render_pie_chart_from_summaryclasses_ggplot2(
        data[data[[grouped_by]] == g, , FALSE], meta_data = meta_data)
    })
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

  gg_colors <- util_get_colors()
  names(gg_colors) <- util_get_labels_grading_class()[names(gg_colors)]

  if (all(is.na(data$class))) {
    return(htmltools::browsable(htmltools::HTML("")))
  }

  if (is.factor(data$class)) {
    data$class <- as.integer(gsub("^cat", "", data$class))
  }
  data$class <- factor(data$class,
                       levels = names(util_get_labels_grading_class()),
                       labels = util_get_labels_grading_class(),
                       ordered = TRUE
  )

  data <- data[order(data$class, -data$value, decreasing = TRUE), , FALSE]


  p <- ggplot(data, aes(x = 1,
                        y = value,
                        fill = class)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(
      aes(x = 1.1,
          label = scales::percent(value / sum(value))),
      position = ggplot2::position_stack(vjust = 0.5),
      check_overlap = TRUE
    ) +
    ggplot2::coord_polar("y", start = 0, clip = "off") +
    scale_fill_manual(values = gg_colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   # legend.position = "none",
                   legend.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank())

  if (identical(grouped_by, "indicator_metric")) { # TODO: Hiearchical structure support
    title <- util_translate_indicator_metrics(groups, short = FALSE,
                                              long = FALSE)
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, "call_names")) { # TODO: Hiearchical structure support
    fnms <- util_cll_nm2fkt_nm(groups)
    if (nchar(fnms) < nchar(groups)) {
      suff <- gsub("^.*_", ": ", groups)
      substr(suff, 3, 3) <- toupper(substr(suff, 3, 3))
    } else {
      suff <- ""
    }
    title <- paste0(util_map_labels(fnms,
                                    util_get_concept_info("implementations"),
                                    to = "dq_report2_short_title",
                                    from = "function_R",
                                    ifnotfound = NA_character_), suff)
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

  p <- p + ggtitle(
    title, subtitle
  )

  htmltools::plotTag(width = 400, height = 400,
    p, alt = "A summary of data quality assessments"
  )

}
