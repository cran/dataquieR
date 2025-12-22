#' Plots and checks for distributions for categorical variables
#'
#' @description
#' This function creates distribution plots for categorical variables.
#'
#' [Descriptor]
#'
#' @details
#' To complete
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the name of the measurement variable
#' @param group_vars [variable] the name of the observer, device or
#'                                   reader variable
#' @param n_cat_max maximum number of categories to be displayed individually
#'                  for the categorical variable (`resp_vars`)
#' @param n_group_max maximum number of categories to be displayed individually
#'                  for the grouping variable (`group_vars`, devices / examiners)
#' @param n_data_min minimum number of data points to create a time course plot
#'                   for an individual category of the `resp_vars` variable
#'
#' @return A [list] with:
#'   - `SummaryPlot`: [ggplot2::ggplot] for the response variable in
#'                    `resp_vars`.
#'
#' @seealso
#' [Online Documentation](https://dataquality.qihs.uni-greifswald.de/)
acc_cat_distributions <- function(resp_vars = NULL,
                                  group_vars = NULL,
                                  study_data,
                                  label_col,
                                  item_level = "item_level",
                                  meta_data = item_level,
                                  meta_data_v2,
                                  n_cat_max =
                                    getOption("dataquieR.max_cat_resp_var_levels_in_plot",
                                              dataquieR.max_cat_resp_var_levels_in_plot_default),
                                  n_group_max =
                                    getOption("dataquieR.max_group_var_levels_in_plot",
                                              dataquieR.max_group_var_levels_in_plot_default),
                                  n_data_min =
                                    getOption("dataquieR.min_time_points_for_cat_resp_var",
                                              dataquieR.min_time_points_for_cat_resp_var_default)) {
  # preps ----------------------------------------------------------------------
  # map metadata to study data
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata_inadm = TRUE)

  # If no response variable is defined, all suitable variables will be selected.
  if (length(resp_vars) == 0) {
    util_message(
      c("All variables defined to be nominal and ordinal in the metadata are used",
        "by acc_cat_distribution."),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE)
    resp_vars <- meta_data[[label_col]][meta_data[[SCALE_LEVEL]] %in%
                                          c(SCALE_LEVELS$NOMINAL,
                                            SCALE_LEVELS$ORDINAL)]
    resp_vars <- intersect(resp_vars, colnames(ds1))
    if (length(resp_vars) == 0) {
      util_error("No suitable variables were defined for acc_cat_distributions.",
                 applicability_problem = TRUE)  #TODO: maybe intrinsic
    }
  }

  # set up grouping variables, if needed
  if (all(is.na(group_vars)) ||
      length(group_vars) == 0) {
    group_vars <- NULL
  } else {
    util_correct_variable_use(group_vars,
                              allow_null = TRUE,
                              allow_more_than_one = FALSE,
                              allow_any_obs_na = TRUE,
                              allow_all_obs_na = FALSE,
                              need_type = "!float",
                              need_scale = "nominal | ordinal"
    )

    if (length(group_vars) > 0) {
      # The grouping variable(s) should not be included as response variable(s).
      if (any(group_vars %in% resp_vars)) {
        resp_vars <- resp_vars[-which(resp_vars %in% group_vars)]
        util_warning(paste("Removed grouping variable from response variables",
                           "for acc_cat_distributions."),
                     applicability_problem = TRUE)
      }
      if (length(resp_vars) == 0) {

        my_message <- util_error

        if (.called_in_pipeline) {
          my_message <- util_error
        } else {
          my_message <- util_warning
        }

        my_message("No variables left to analyse for acc_cat_distributions.",
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
        x <- util_create_lean_ggplot(
          ggplot() +
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
        )
        x <- util_set_size(x)

        return(list(SummaryPlot = x))
      }
    }
  }

  util_correct_variable_use(resp_vars,
                            allow_more_than_one = FALSE,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 2,
                            need_scale = "nominal|ordinal"
  )

  # create the plot
  p <- util_plot_categorical_vars(resp_vars = resp_vars,
                                  group_vars = group_vars,
                                  study_data = study_data,
                                  meta_data = meta_data,
                                  n_cat_max = n_cat_max,
                                  n_group_max = n_group_max,
                                  n_data_min = n_data_min)

  rvlab <- prep_get_labels(resp_vars,
                           item_level = meta_data,
                           label_col = label_col,
                           resp_vars_match_label_col_only = TRUE,
                           label_class = "LONG")

  gvlab <- prep_get_labels(group_vars,
                           item_level = meta_data,
                           label_col = label_col,
                           resp_vars_match_label_col_only = TRUE,
                           label_class = "LONG")

  # p <- p + ggplot2::ggtitle(rvlab,
  #                           sprintf("Effects of %s",
  #                                   gvlab)
  # )
  p <- p + util_create_lean_ggplot(
    ylab(gvlab), # + xlab(rvlab)
    gvlab = gvlab
  )
  #  p <- util_set_size(p)

  as_plotly <- attr(p, "as_plotly")
  attr(p, "as_plotly") <- NULL
  dont_util_adjust_geom_text_for_plotly <-
    attr(p, "dont_util_adjust_geom_text_for_plotly")
  attr(p, "dont_util_adjust_geom_text_for_plotly") <- NULL

  return(util_attach_attr(list(SummaryPlot = p),
                          as_plotly = as_plotly,
                          dont_util_adjust_geom_text_for_plotly =
                            dont_util_adjust_geom_text_for_plotly))
}
