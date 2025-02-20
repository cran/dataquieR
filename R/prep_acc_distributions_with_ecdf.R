#' Utility function to plot a combined figure for distribution checks
#'
#' @description
#' Data quality indicator checks "Unexpected location" with histograms and
#' plots of empirical cumulative distributions for the subgroups.
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable list] the name of the measurement variable
#' @param group_vars [variable list] the name of the observer, device or
#'                                   reader variable
#' @param n_group_max maximum number of categories to be displayed individually
#'                  for the grouping variable (`group_vars`, devices / examiners)
#' @param n_obs_per_group_min minimum number of data points per group to create
#'                  a graph for an individual category of the `group_vars` variable
#'
#' @return A `SummaryPlot`.
#'
#' @importFrom ggplot2 ggplot_build xlim
prep_acc_distributions_with_ecdf <- function(resp_vars = NULL,
                                             group_vars = NULL,
                                             study_data,
                                             label_col,
                                             item_level = "item_level",
                                             meta_data = item_level,
                                             meta_data_v2,
                                             n_group_max = getOption("dataquieR.max_group_var_levels_in_plot", dataquieR.max_group_var_levels_in_plot_default),
                                             n_obs_per_group_min = getOption("dataquieR.min_obs_per_group_var_in_plot", dataquieR.min_obs_per_group_var_in_plot_default)) {
  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          #.apply_factor_metadata = TRUE, # can be omitted in favor of .apply_factor_metadata_inadm
                          .apply_factor_metadata_inadm = TRUE
  )

  util_correct_variable_use(resp_vars,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 10,
                            need_scale = "interval | ratio"
  )

  util_correct_variable_use(group_vars,
                            allow_more_than_one = FALSE,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 2,
                            need_type = "!float",
                            need_scale = "nominal | ordinal"
  )

  util_expect_scalar(n_group_max,
                     check_type = util_is_numeric_in(min = 2,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  util_expect_scalar(n_obs_per_group_min,
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  part1 <- acc_distributions(resp_vars = resp_vars,
                             study_data = study_data,
                             meta_data = meta_data,
                             label_col = label_col,
                             flip_mode = "noflip")
  p1 <- part1$SummaryPlotList[[resp_vars]]
  xx <- ggplot_build(p1)$layout$panel_scales_x[[1]]$range$range
  xx_exp <- xx
  xx_exp[1] <- xx_exp[1] - 0.05 * (xx_exp[2] - xx_exp[1])
  xx_exp[2] <- xx_exp[2] + 0.05 * (xx_exp[2] - xx_exp[1])
  suppressMessages(p1 <- p1 +
    xlim(xx_exp))
  part2 <- acc_distributions_ecdf(resp_vars = resp_vars, group_vars = group_vars,
                                  study_data = study_data,
                                  label_col = label_col,
                                  meta_data = meta_data,
                                  n_group_max = n_group_max,
                                  n_obs_per_group_min = n_obs_per_group_min)
  suppressMessages(p2 <- part2$SummaryPlotList[[resp_vars]] +
    xlim(xx_exp))

  P <- p1 + p2 +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'A')

  return(list(SummaryPlot = util_set_size(P)))
}

#' @family plotly_shims
#' @concept plotly_shims
#' @keywords internal
util_as_plotly_prep_acc_distributions_with_ecdf <- function(res, ...) {
  if (length(res$SummaryPlot) != 1) {
    return(plotly::ggplotly(ggplot2::ggplot() +
                              ggplot2::annotate("text", x = 0, y = 0,
                                                label =
                                                  sprintf(paste("Internal error: I should  have exactly 1 result, if",
                                                                "calling plotly for a dq_report2 otuput. I have %d."),
                                                          length(res$SummaryPlot))) +
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
                              )))
  }
  res$SummaryPlot <-
    util_remove_dataquieR_result_class(res$SummaryPlot)
  # use res$SummaryPlot, not something from the enclosure
  # of the result, that may contain study data.
  util_ensure_suggested("plotly")

  if (inherits(res$SummaryPlot, "patchwork")) {
    py1 <- try(plotly::ggplotly(res$SummaryPlot[[1]],
                                ...), silent = TRUE)
    py2 <- try(plotly::ggplotly(res$SummaryPlot[[2]],
                                ...), silent = TRUE)
    util_stop_if_not(!inherits(py1, "try-error"))
    util_stop_if_not(!inherits(py2, "try-error"))
    # https://plotly.com/r/subplots/#subplots-with-shared-yaxes
    plotly::subplot(
      plotly::add_annotations( # https://stackoverflow.com/a/59191142
        py1,
        text = "A",
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      ),
      plotly::add_annotations( # https://stackoverflow.com/a/59191142
        py2,
        text = "B",
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      ),
      nrows = 2,
      shareX = TRUE)
  } else {
    py <- plotly::ggplotly(res$SummaryPlot, ...)
    plotly::layout(py, xaxis = list(tickangle = "auto"))
  }
}
