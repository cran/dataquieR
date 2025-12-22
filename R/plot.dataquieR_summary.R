#' Plot a `dataquieR` summary
#'
#' @inheritParams util_filter_repsum
#' @param x the `dataquieR` summary, see [summary()] and [dq_report2()]
#' @param y not yet used
#' @param ... not yet used
#' @param filter if given, this filters the summary, e.g.,
#'         `filter = call_names == "com_qualified_item_missingness"`
#' @param dont_plot suppress the actual plotting, just return a printable
#'                   object derived from `x`
#' @param stratify_by column to stratify the summary, may be one string.
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#'
#' @return invisible html object
#' @export
#'
plot.dataquieR_summary <- function(x, y, ..., filter, dont_plot = FALSE,
                                   stratify_by,
                                   vars_to_include = "study",
                                   disable_plotly = FALSE) {
  if (!disable_plotly) {
    util_ensure_suggested(pkg = c("plotly"),
                          goal = "generate interactive HTML-summaries.")
  }

  util_stop_if_not("y is not used for plotting summaries" = missing(y))
  util_ensure_suggested(pkg = c("htmltools",
                                "DT", "rmarkdown",
                                "markdown"),
                        goal = "generate plain HTML-summaries.")

  x <- util_reclassify_dataquieR_summary(x)

  repsum <- x
  indicator_metric <- NULL
  function_name <- NULL
  util_expect_scalar(dont_plot, check_type = is.logical)
  if (missing(stratify_by)) {
    stratify_by <- character(0)
  } else {
    util_expect_scalar(stratify_by, check_type = is.character)
  }
  this <- attr(repsum, "this")

  suitable_vars_sum <- util_filter_repsum(this$result,
                                          vars_to_include,
                                          this$meta_data,
                                          this$rownames_of_report,
                                          this$label_col)
  rownames_of_report <- attr(suitable_vars_sum, "rownames_of_report")

  # suppressMessages(this$result %>%
  #                    dplyr::filter(!startsWith(as.character(indicator_metric), "CAT_")) %>%
  #                    dplyr::filter(!startsWith(as.character(indicator_metric), "MSG_")) %>%
  #                    dplyr::filter(!is.na(class)) %>%
  #                    dplyr::group_by(function_name, class) %>%
  #                    dplyr::summarise(value = length(function_name)) ->
  #                    summ_per_fkt_fig)

  suppressMessages(suitable_vars_sum %>%
                     dplyr::filter(!startsWith(as.character(indicator_metric), "CAT_")) %>%
                     dplyr::filter(!startsWith(as.character(indicator_metric), "MSG_")) ->
                     all_per_variable_all_issue_classes_except_errors)

  if (!missing(filter)) {
    cl <-
      rlang::call2(dplyr::filter,
                   .data = all_per_variable_all_issue_classes_except_errors,
                   substitute(filter))
    all_per_variable_all_issue_classes_except_errors <- eval(cl,
                                                             envir = parent.frame())
  }

  if (nrow(all_per_variable_all_issue_classes_except_errors) == 0) {
    return(htmltools::HTML(""))
  }

  if (!all(stratify_by %in%
           colnames(all_per_variable_all_issue_classes_except_errors))) {
    util_error("Cannot stratify summary by %s, I don't know, what %s are.",
               util_pretty_vector_string(setdiff(stratify_by,
                  colnames(all_per_variable_all_issue_classes_except_errors))),
               util_pretty_vector_string(setdiff(stratify_by,
                colnames(all_per_variable_all_issue_classes_except_errors))))
  }

  all_per_variable_all_issue_classes_except_errors %>%
    dplyr::filter(!is.na(value)) %>%
    # dplyr::filter(!is.na(class)) %>%
    # dplyr::group_by(VAR_NAMES, !!stratify_by) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(VAR_NAMES, stratify_by)))) %>%
    dplyr::summarise(class =
                       suppressWarnings(
                         util_as_cat(max(util_as_cat(class), na.rm = TRUE)))) ->
    worst_per_variable
  plot_tab <- worst_per_variable

  plot_tab %>%
    # dplyr::group_by(class) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("class", stratify_by)))) %>%
    dplyr::summarise(value = length(VAR_NAMES),
                     note = util_pretty_vector_string(
                       n_max = 3,
                       suppressWarnings(prep_get_labels(VAR_NAMES,
                                                        max_len = 15,
                                                        label_class = "SHORT",
                                                        meta_data =
                                                          this$meta_data)))) ->
    sum_plot_tab

  if (length(stratify_by) == 0) {
    sum_plot_tab %>%
      dplyr::mutate(X =
                      sprintf(
                        "%d variables: %d classified by indicators",
                        length(rownames_of_report),
                        length(unique(plot_tab$VAR_NAMES))
                      ),
                    class = as.integer(class)) ->
      sum_plot_tab
  }
  # summ_per_fkt_fig %>%
  #   dplyr::mutate(dimension = c(int = "Integrity",
  #                               acc = "Accuracy",
  #                               con = "Consistency",
  #                               com = "Completeness")[
  #                                 substr(function_name, 1, 3)]) %>%
  #   dplyr::group_by(dimension, class) %>%
  #   dplyr::summarise(value = sum(value)) -> summ_per_dim_fig

  #  sum_worst_per_variable$note <-

#  if (length(unique(sum_plot_tab$X)) > 0) {
  if (!disable_plotly) {
    summaryplots <-
      prep_render_pie_chart_from_summaryclasses_plotly(
        sum_plot_tab,
        meta_data = this$meta_data
      )
  } else {
    summaryplots <-
      prep_render_pie_chart_from_summaryclasses_ggplot2(
        sum_plot_tab,
        meta_data = this$meta_data
      )
  }
  # } else {
  #   summaryplots <- htmltools::tags$p(sprintf(
  #     "None of the %d variables was classified", length(rownames_of_report)))
  # }

  if (!inherits(summaryplots, "htmlwidget") &&
      !inherits(summaryplots, "html") &&
      !inherits(summaryplots, "shiny.tag") &&
      !inherits(summaryplots, "shiny.tag.list")) {
    if (!all(vapply(summaryplots, inherits, "htmlwidget",
                    FUN.VALUE = logical(1)) |
             vapply(summaryplots, inherits, "html",
                    FUN.VALUE = logical(1)) |
             vapply(summaryplots, inherits, "shiny.tag",
                    FUN.VALUE = logical(1)) |
             vapply(summaryplots, inherits, "shiny.tag.list",
                    FUN.VALUE = logical(1)))) {
      util_error(c("Internal error: Not all summaryplots are html htmlwidgets",
                   "or shiny.tags / shiny.tag.lists. Sorry, and please report",
                   "this bug. Thank you."))
    }
    summaryplots <- do.call(htmltools::tagList, summaryplots)
  }

  r <- htmltools::browsable(summaryplots)

  if (!dont_plot) {
    print(r)
  } else {
   invisible(r)
  }
}
