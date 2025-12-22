#' ECDF plots for distribution checks
#'
#' @description
#' Data quality indicator checks "Unexpected location" and "Unexpected
#' proportion" if a grouping variable is included: Plots of
#' empirical cumulative distributions for the subgroups.
#'
#' [Descriptor]
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#' @param resp_vars [variable list] the names of the measurement variables
#' @param group_vars [variable list] the name of the observer, device or
#'                                   reader variable
#' @param n_group_max maximum number of categories to be displayed individually
#'                  for the grouping variable (`group_vars`, devices / examiners)
#' @param n_obs_per_group_min minimum number of data points per group to create
#'                  a graph for an individual category of the `group_vars` variable
#'
#' @return A [list] with:
#'   - `SummaryPlotList`: [list] of [ggplot2::ggplot]s for each response variable in
#'                    `resp_vars`.
#'
#' @importFrom ggplot2 ggplot aes stat_ecdf scale_x_continuous scale_x_datetime
#'                     theme_minimal theme element_text element_blank
#'                     labs
#' @importFrom rlang .data
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_ecdf <- function(resp_vars = NULL,
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

  # If no response variable is defined, all suitable variables will be selected.
  if (length(resp_vars) == 0) {
    util_message(
      c("All variables with interval or ratio scale according to the metadata",
        "are used by acc_distributions_ecdf."),
      applicability_problem = TRUE, intrinsic_applicability_problem = TRUE)
    resp_vars <- meta_data[[label_col]][meta_data$SCALE_LEVEL %in%
                                          c("interval", "ratio")]
    resp_vars <- intersect(resp_vars, colnames(ds1))
    if (length(resp_vars) == 0) {
      util_error("No suitable variables were defined for acc_distributions_ecdf.",
                 applicability_problem = TRUE)
    }
  }

  util_correct_variable_use(resp_vars,
                            allow_more_than_one = TRUE,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 10,
                            do_not_stop = ifelse(length(resp_vars) > 1,
                                                 TRUE, FALSE),
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

  # The grouping variable should not be included as response variable.
  if (any(group_vars %in% resp_vars)) {
    resp_vars <- resp_vars[-which(resp_vars %in% group_vars)]
    util_warning(paste("Removed grouping variable from response variables",
                       "for acc_distributions_ecdf."),
                 applicability_problem = TRUE)
  }
  if (length(resp_vars) == 0) {
    util_error("No variables left to analyse for acc_distributions_ecdf.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  util_expect_scalar(n_group_max,
                     check_type = util_is_numeric_in(min = 2,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  util_expect_scalar(n_obs_per_group_min,
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  # Which variables are of type 'datetime'?
  is_datetime_var <- vapply(resp_vars, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] ==
      DATA_TYPES$DATETIME
  }, FUN.VALUE = logical(1))

  # omit observations with missing grouping variable
  ds1 <- ds1[!(is.na(ds1[[group_vars]])), , drop = FALSE]

  ds1[[group_vars]] <- as.factor(ds1[[group_vars]]) # there is only one
  # grouping variable, we have ensured this by allow_more_than_one = FALSE

  tab_gr <- table(ds1[[group_vars]])

  # exclude levels of the grouping variable with too few observations
  if (any(tab_gr) < n_obs_per_group_min) {
    keep_gr <- names(tab_gr)[which(tab_gr >= n_obs_per_group_min)]
    levels(ds1[[group_vars]])[which(!levels(ds1[[group_vars]]) %in%
                                      keep_gr)] <- NA
    ds1 <- ds1[!(is.na(ds1[[group_vars]])), , drop = FALSE]
    tab_gr <- table(ds1[[group_vars]])
  }

  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.",
               applicability_problem = TRUE)
  }

  # collapse 'rare' groups to reduce the number of levels, if needed
  if (length(tab_gr) > n_group_max) {
    tab_gr <- tab_gr[order(tab_gr, decreasing = TRUE)]
    keep_gr <- names(tab_gr)[1:n_group_max]
    levels(ds1[[group_vars]])[which(!levels(ds1[[group_vars]])
                                    %in% keep_gr)] <- "other"
    # new category 'other' should always be the last one
    lvl_gr <-
      c(levels(ds1[[group_vars]])[which(levels(ds1[[group_vars]])
                                        %in% keep_gr)],
        "other")
    ds1[[group_vars]] <- as.character(ds1[[group_vars]])
    ds1[[group_vars]] <- factor(ds1[[group_vars]], levels = lvl_gr)
  }

  # find suitable labels
  lbg <- paste0(prep_get_labels(group_vars,
                                item_level = meta_data,
                                label_col = label_col,
                                resp_vars_match_label_col_only = TRUE,
                                label_class = "SHORT"))

  is_datetime_var <- vapply(resp_vars, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] ==
      DATA_TYPES$DATETIME
  }, FUN.VALUE = logical(1))

  # plot -----------------------------------------------------------------------
  ref_env <- environment()

  txtspec <- element_text(
    colour = "black", hjust = .5,
    vjust = .5, face = "plain", size = 10
  )

  plot_list <- lapply(setNames(nm = resp_vars), function(rv) {

    # find suitable labels
    lbr <- paste0(prep_get_labels(rv,
                                  item_level = meta_data,
                                  label_col = label_col,
                                  resp_vars_match_label_col_only = TRUE,
                                  label_class = "SHORT"))

    # omit NAs from data to prevent ggplot2 warning messages
    ds1 <- ds1[!(is.na(ds1[[rv]])), , drop = FALSE]

    .ds00 <- ds1[, c(rv, group_vars), drop = FALSE]

    pp <- util_create_lean_ggplot(
      ggplot(data = .ds00,
             aes(x = .data[[rv]], colour = .data[[group_vars]])) +
        stat_ecdf(geom = "step") +
        labs(x = "", y = paste0("ECDF: ", lbr, " (by ", lbg, ")")) +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec,
          legend.title = element_blank()
        ),
      .ds00 = .ds00,
      rv = rv,
      group_vars = group_vars,
      lbr = lbr,
      lbg = lbg,
      txtspec = txtspec
    )

    if (!is_datetime_var[[rv]]) {
      pp <- pp + util_create_lean_ggplot(
        scale_x_continuous(expand = expansion(mult = 0.1))
      )
    } else {
      pp <- pp + util_create_lean_ggplot(
        scale_x_datetime(expand = expansion(mult = 0.1))
      )
    }

    if (util_ensure_suggested("colorspace",
                              "use the colorspace color scale",
                              err = FALSE)) {
      pp <- pp + util_create_lean_ggplot(
          colorspace::scale_color_discrete_sequential(palette = "Plasma",
                                                      na.value = "grey")
      )
    }

    util_set_size(pp)
  })

  return(list(SummaryPlotList = plot_list))
}

