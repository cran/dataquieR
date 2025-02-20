#' Create a `ggplot2` pie chart
#'
#' @param data data as returned by `prep_summary_to_classes` but
#'             summarized by one column (currently, we support
#'             `indicator_metric`, `STUDY_SEGMENT`, and `VAR_NAMES`)
#' @param meta_data [meta_data]
#'
#' @return a [ggplot2::ggplot2] plot
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

  if (nrow(data) == 0) {
    x <- ggplot() +
              annotate("text", x = 0, y = 0, label = "Empty result.") +
              theme(
                axis.line = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background = element_blank()
              )
            x <- util_set_size(x)
            return(x)
  }

  grouped_by <- setdiff(colnames(data), c("class", "value", "percent", "note"))

  util_stop_if_not(length(grouped_by) == 1)

  groups <- unique(data[[grouped_by]])

  util_stop_if_not(length(groups) > 0)

  if (length(groups) > 1) {
    return(lapply(setNames(nm = groups), function(g) {
      prep_render_pie_chart_from_summaryclasses_ggplot2(
        data[data[[grouped_by]] == g, , FALSE], meta_data = meta_data)
    }))
  }

  gg_colors <- util_get_colors()
  names(gg_colors) <- util_get_labels_grading_class()[names(gg_colors)]

  if (is.factor(data$class)) {
    data$class <- as.integer(gsub("^cat", "", data$class))
  }
  data$class <- factor(data$class,
                       levels = names(util_get_labels_grading_class()),
                       labels = util_get_labels_grading_class(),
                       ordered = TRUE
  )

  data <- data[order(data$class, -data$value, decreasing = TRUE), , FALSE]


  p <- ggplot(data, aes(x = "",
                        y = value,
                        fill = class,
                        label = scales::percent(value / sum(value)))
              ) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::coord_polar("y", start = 0) +
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

  p
}
