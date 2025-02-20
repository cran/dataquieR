#' Utility function for smoothed longitudinal trends from logistic regression models
#'
#' This function is under development. It computes a logistic regression for
#' binary variables and visualizes smoothed time trends of the residuals by
#' LOESS or GAM. The function can also be called for non-binary outcome
#' variables. These will be transformed to binary variables, either using
#' user-specified groups in the metadata columns `RECODE_CASES` and/or
#' `RECODE_CONTROL` (see `util_dichotomize`), or it will attempt to recode the
#' variables automatically. For nominal variables, it will consider the most
#' frequent category as 'cases' and every other category as 'control', if there
#' are more than two categories. Nominal variables with only two distinct values
#' will be transformed by assigning the less frequent category to 'cases' and
#' the more frequent category to 'control'. For variables of other statistical
#' data types, values inside the interquartile range are considered as
#' 'control', values outside this range as 'cases'. Variables with few
#' different values are transformed in a simplified way to obtain two groups.
#'
#' [Descriptor]
#'
#' @param resp_vars  [variable] the name of the (binary) measurement variable
#' @param group_vars [variable] the name of the observer, device or
#'                              reader variable
#' @param co_vars [variable list] a vector of co-variables, e.g. age and sex for
#'                              adjustment
#' @param time_vars  [variable] the name of the variable giving the time
#'                              of measurement
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                              attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                              with labels of variables
#' @param min_obs_in_subgroup [integer] from=0. This optional argument specifies
#'                       the minimum number of observations that is required to
#'                       include a subgroup (level) of the `group_var` in the
#'                       analysis. Subgroups with less observations are
#'                       excluded.
#' @param resolution [integer] the maximum number of time points used for
#'                             plotting the trend lines
#' @param plot_format [enum] AUTO | COMBINED | FACETS | BOTH. Return the plot
#'                             as one combined plot for all groups or as
#'                             facet plots (one figure per group). `BOTH` will
#'                             return both variants, `AUTO` will decide based
#'                             on the number of observers.
#' @param n_group_max [integer] maximum number of categories to be displayed
#'                  individually for the grouping variable (`group_vars`,
#'                  devices / examiners)
#' @param enable_GAM [logical] Can LOESS computations be replaced by general
#'                  additive models to reduce memory consumption  for large
#'                  datasets?
#' @param exclude_constant_subgroups [logical] Should subgroups with constant
#'                  values be excluded?
#'
#' @return a [list] with:
#'   - `SummaryPlotList`: a plot.
#'
#' @importFrom ggplot2 ggplot aes scale_color_manual xlab ylab
#'                     geom_line facet_wrap theme_minimal ggtitle theme
#'                     element_blank expand_limits
#' @importFrom stats as.formula lm loess predict na.omit glm binomial poisson sd
#'                   cov var runif
#' @keywords internal
util_acc_loess_bin <- function(
    resp_vars,
    label_col = NULL,
    study_data,
    item_level = "item_level",
    group_vars = NULL,
    time_vars,
    co_vars = NULL,
    min_obs_in_subgroup = 30,
    resolution = 80,
    plot_format = getOption("dataquieR.acc_loess.plot_format",
                            dataquieR.acc_loess.plot_format_default),
    meta_data = item_level,
    n_group_max = getOption("dataquieR.max_group_var_levels_in_plot",
                            dataquieR.max_group_var_levels_in_plot_default),
    enable_GAM = getOption("dataquieR.GAM_for_LOESS",
                           dataquieR.GAM_for_LOESS.default),
    exclude_constant_subgroups = getOption("dataquieR.acc_loess.exclude_constant_subgroups",
                                           dataquieR.acc_loess.exclude_constant_subgroups.default)) {
  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE)

  # correct variable use?
  # (checked before, but included here to catch implementation errors)
  util_correct_variable_use("resp_vars",
                            need_scale = "!na",
                            allow_all_obs_na = FALSE)
  util_correct_variable_use("group_vars",
                            need_scale = "nominal | ordinal",
                            allow_all_obs_na = TRUE,
                            allow_na = TRUE,
                            allow_null = TRUE)
  util_correct_variable_use("time_vars",
                            need_type = DATA_TYPES$DATETIME,
                            need_scale = "interval | ratio",
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 3)
  util_correct_variable_use("co_vars",
                            allow_more_than_one = TRUE,
                            allow_all_obs_na = FALSE,
                            allow_na = TRUE,
                            allow_null = TRUE)

  # support time course plots without (sub-)groups
  if (is.null(group_vars) || all(util_empty(group_vars))) {
    # create a dummy grouping variable that is not yet contained in ds1
    group_vars <- "dummy_group"
    while (group_vars %in% colnames(ds1)) {
      group_vars <- paste0("dummy_group",
                           ceiling(runif(n = 1, min = 1, max = ncol(ds1) * 2)),
                           sep = "_")
    }
    ds1[[group_vars]] <- 1
    plot_title <- paste("Time course plot for", resp_vars)
    # The dummy variable should not be mentioned in the title of the plot.
  } else {
    plot_title <- paste("Effects of", group_vars, "in", resp_vars)
  }

  if (is.null(co_vars)) {
    co_vars <- character(0)
  }
  co_vars <- na.omit(co_vars)

  # omit missing values and unnecessary variables
  n_prior <- nrow(ds1)
  ds1 <- ds1[, c(resp_vars, time_vars, group_vars, co_vars)]
  if (grepl("dummy_group", group_vars)) {
    # Only mention the 'dummy_group' in the message if it contributes any
    # missing values, otherwise do not mention it.
    if (any(is.na(ds1[complete.cases(ds1[, c(time_vars, co_vars)]),
                      group_vars]))) {
      msg_part1 <- paste0(c(group_vars, co_vars), collapse = ", ")
    } else {
      msg_part1 <- paste0(co_vars, collapse = ", ")
    }
  } else {
    msg_part1 <- paste0(c(group_vars, co_vars), collapse = ", ")
  }
  ds1 <- ds1[complete.cases(ds1[, c(time_vars, group_vars, co_vars)]), ]
  n_post <- nrow(ds1)
  msg <- NULL
  if (n_post < n_prior) {
    msg <- paste0(
      "Due to missing values in ",
      ifelse(nchar(msg_part1) > 0, paste0(msg_part1, " or "), ""),
      time_vars, ", N = ", n_prior - n_post,
      " observations were excluded. "
    )
  }
  n_prior <- n_post
  ds1 <- ds1[complete.cases(ds1), ]
  n_post <- nrow(ds1)
  if (n_post < n_prior) {
    msg <- paste0(
      msg, "Due to missing values in ", resp_vars, ", N = ",
      n_prior - n_post, " observations were excluded",
      ifelse(nchar(msg) > 0, " additionally.", "."))
  }
  if (nchar(msg) > 0) {
    util_message(trimws(msg),
                 applicability_problem = FALSE)
  }

  # dichotomize resp_vars
  if (!("RECODE_CASES" %in% colnames(meta_data))) {
    meta_data[[RECODE_CASES]] <- ""
  }
  if (!("RECODE_CONTROL" %in% colnames(meta_data))) {
    meta_data[[RECODE_CONTROL]] <- ""
  }
  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])

  if (var_prop$NDistinct == 1) {
    util_error("The response variable is constant after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  } else {
    if (util_empty(meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars]) &
        util_empty(meta_data[[RECODE_CONTROL]][meta_data[[label_col]] == resp_vars])) {
      # => the recoding is not defined in the metadata. dataquieR will try to
      # define a helpful categorization into two groups:
      if (meta_data[[SCALE_LEVEL]][meta_data[[label_col]] == resp_vars] ==
          SCALE_LEVELS$NOMINAL) {
        count_nom <- util_table_of_vct(ds1[[resp_vars]])

        if (var_prop$NDistinct == 2) {
          # less frequent category as 'cases'
          count_nom <- count_nom[which(count_nom[, 2] > 0), ]
          count_nom <- count_nom[order(count_nom[, 2], decreasing = FALSE), ]
          meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars] <-
            paste(count_nom[1, 1])
        } else {
          # more than two groups: dataquieR will use the most frequent category
          # as 'cases', the remaining categories as 'control'.
          count_nom <- count_nom[order(count_nom[, 2], decreasing = TRUE), ]
          meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars] <-
            paste(count_nom[1, 1])
        }
      } else {
        # Automatic recoding will be based on quartiles
        qu_rvs <- quantile(as.numeric(ds1[[resp_vars]]), type = 1)
        val_lab_applied <- is.factor(ds1[[resp_vars]])
        if (length(unique(qu_rvs)) < 3) {
          # the value that occurs predominantly will become the 'control'
          tab_qu <- util_table_of_vct(qu_rvs)
          tab_qu <- tab_qu[order(tab_qu[, 2], decreasing = TRUE), ]
          if (val_lab_applied) {
            meta_data[[RECODE_CONTROL]][meta_data[[label_col]] == resp_vars] <-
              as.character(tab_qu[1,1])
          } else {
            meta_data[[RECODE_CONTROL]][meta_data[[label_col]] == resp_vars] <-
              paste0("[", tab_qu[1,1], ";", tab_qu[1,1], "]")
          }
        } else if (var_prop$NDistinct < 10) {
          # If there are few values (more than two), consider the
          # upper quartile as 'cases'.
          if (val_lab_applied) {
            meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars] <-
              paste(levels(ds1[[resp_vars]])[qu_rvs[4]:qu_rvs[5]],
                    collapse = " | ")
          } else {
            meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars] <-
              paste0("[", qu_rvs[4], ";", qu_rvs[5], "]")
          }
        } else {
          # Values below Q1 and/or above Q3 as ('extreme') cases,
          # values between Q1 and Q3 (i.e., central 50% of values) as control
          if (val_lab_applied) {
            meta_data[[RECODE_CONTROL]][meta_data[[label_col]] == resp_vars] <-
              paste(levels(ds1[[resp_vars]])[qu_rvs[2]:qu_rvs[4]],
                    collapse = " | ")
          } else {
            meta_data[[RECODE_CONTROL]][meta_data[[label_col]] == resp_vars] <-
              paste0("[", qu_rvs[2], ";", qu_rvs[4], "]")
          }
        }
      }
    }
  }

  rvs_bin <- util_dichotomize(
    study_data = ds1[, resp_vars, drop = FALSE],
    meta_data = meta_data,
    label_col = label_col)
  rvs_bin_note <- attr(rvs_bin, "Dichotomization")[[resp_vars]]
  ds1[[resp_vars]] <- unlist(rvs_bin)
  ds1 <- ds1[complete.cases(ds1), ]

  # convert group_vars to factor (needed for example for the dummy group)
  ds1[[group_vars]] <- factor(ds1[[group_vars]])

  # TODO: use util_check_group_levels
  # too few observations per level?
  # check which groups do not have enough observations or time points
  tab_groups <- table(ds1[[group_vars]])
  groups_below_min_obs <- names(tab_groups)[tab_groups < min_obs_in_subgroup]
  tab_groups_tp <- vapply(levels(ds1[[group_vars]]), FUN.VALUE = numeric(1),
                          FUN = function(gr) {
                            length(unique(ds1[[time_vars]][
                              ds1[[group_vars]] == gr]))
                          })
  groups_with_few_tp <- names(tab_groups_tp)[tab_groups_tp < 3]
  if (length(groups_below_min_obs) > 0 | length(groups_with_few_tp) > 0) {
    to_excl <- unique(c(groups_below_min_obs, groups_with_few_tp))
    util_message(paste("Levels of the group_var with too few observations",
                       "were discarded",
                       paste0("(level",
                              ifelse(length(to_excl) > 1, "s ", " "),
                              paste(to_excl, collapse = ", "),
                              ").")
    ),
    applicability_problem = FALSE)
    # exclude levels with few observations or time points
    ds1 <- subset(ds1,
                  ds1[[group_vars]] %in%
                    setdiff(levels(ds1[[group_vars]]), to_excl))
    # drop unused levels
    ds1[[group_vars]] <- factor(ds1[[group_vars]])
  }

  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.",
               applicability_problem = TRUE)
  }

  if (exclude_constant_subgroups) {
    lvl_to_exclude <- levels(ds1[[group_vars]])[
      vapply(levels(ds1[[group_vars]]), FUN.VALUE = logical(1), function(gr) {
        check1 <- util_dist_selection(
          ds1[[resp_vars]][which(as.character(ds1[[group_vars]]) == gr)]
        )
        check1$NDistinct < 2 ||
          check1$PropZeroes > 0.95 ||
          check1$PropZeroes < 0.05
      })
    ]
    if (length(lvl_to_exclude) > 0) {
      util_message(paste("Levels of the group_var with constant values",
                         "were discarded",
                         paste0("(level",
                                ifelse(length(lvl_to_exclude) > 1, "s ", " "),
                                paste(lvl_to_exclude, collapse = ", "),
                                ").")
      ),
      applicability_problem = FALSE)
      ds1 <- subset(ds1,
                    ds1[[group_vars]] %in%
                      setdiff(levels(ds1[[group_vars]]), lvl_to_exclude))
      # drop unused levels
      ds1[[group_vars]] <- factor(ds1[[group_vars]])
    }
  }

  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.",
               applicability_problem = TRUE)
  }

  # collapse 'rare' groups to reduce the number of levels, if needed
  tab_groups <- table(ds1[[group_vars]])
  if (length(tab_groups) > n_group_max) {
    tab_groups <- tab_groups[order(tab_groups, decreasing = TRUE)]
    keep_gr <- names(tab_groups)[1:n_group_max]
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

  if (length(levels(ds1[[group_vars]])) < 2) {
    plot_format <- "COMBINED"
  }

  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.",
               applicability_problem = FALSE)
  }

  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  if (var_prop$NDistinct > 2) {
    util_error("The response variable is not binary after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  if (var_prop$PropZeroes > 0.9 || var_prop$PropZeroes < 0.1) {
    util_error("The response variable contains too few cases/controls.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  # order data by time and groups
  # (for plotting and for the moving window calculations)
  ds1 <- ds1[order(ds1[[time_vars]], ds1[[group_vars]]), ]
  # reduce time points according to the resolution, if needed
  tp_seq <- unique(ds1[[time_vars]])
  tp_round_seq <- util_optimize_sequence_across_time_var(
    time_var_data = tp_seq,
    n_points = resolution)
  ds1[["ROUND_TIME"]] <- suppressWarnings(as.POSIXct(
    lubridate::round_date(ds1[[time_vars]], unit = tp_round_seq)))

  # store a numeric version of the original time variable for later calculations
  ds1$time_vars_num <- suppressWarnings(as.numeric(ds1[[time_vars]]))

  # Modelling group-wise trends ------------------------------------------------
  # adjust response for covariables (if any) using a linear model
  if (length(co_vars) > 0) {
    fmla <- as.formula(paste0(paste0(util_bQuote(resp_vars), "~"),
                              paste0(util_bQuote(co_vars),
                                     collapse = " + ")))
    logfit1 <- glm(fmla, data = ds1, family = binomial(link = "logit"))
    # store residuals
    # These values will be used for LOESS fits. In this way, we fit LOESS after
    # adjusting the response for the covariables.
    ds1$Residuals <- ds1[[resp_vars]] - logfit1$fitted.values
    # Memory consumption
    rm(logfit1)
  } else {
    ds1$Residuals <- ds1[[resp_vars]]
  }

  # fit LOESS/GAM for each group separately
  grouped_ds1 <- split(ds1, ds1[[group_vars]])
  processed_grouped_ds1 <- lapply(grouped_ds1, function(data_i) {
    if (var(data_i[["Residuals"]]) == 0) { # constant for this subgroup
      df_i <- unique(data_i[, c("Residuals", "ROUND_TIME")])
      fit_vals <- df_i[["Residuals"]]
      data_i_seq <- df_i[["ROUND_TIME"]]
    } else if (max(tab_groups) > 1000 &&
        util_ensure_suggested("mgcv",
                              "use GAM from mgcv instead of loess for lower memory consumption",
                              err = FALSE) &&
        enable_GAM) {
      # If there are too many observations, switch to GAM instead of LOESS
      # because of memory consumption (if available).
      fit_i <- mgcv::gam(Residuals ~ s(time_vars_num, bs = "cs"),
                         method = "REML",
                         data = data_i)
      # To plot the trend line at the time points in `tp_round_seq`
      # (restricted to those values that lie within the observed time period
      # for this group), we need fitted values at these time points.
      data_i_seq <- tp_round_seq[
        (which(tp_round_seq == min(data_i[["ROUND_TIME"]]))):
          (which(tp_round_seq == max(data_i[["ROUND_TIME"]])))]
      data_i_seq_num <- suppressWarnings(as.numeric(data_i_seq))
      data_i_seq_num <- as.data.frame(data_i_seq_num)
      colnames(data_i_seq_num) <- "time_vars_num"

      fit_vals <- mgcv::predict.gam(fit_i, data_i_seq_num)
    } else { # LOWESS
      # calculate smoothing parameter for data_i
      max_smooth <- round(1 / log10(length(unique(data_i[[time_vars]]))), 2)
      max_smooth <- max(0.5, # max_smooth should be greater than or equal to 0.5
                        # max_smooth should not be greater than 1
                        # (happens if there are few time points)
                        min(max_smooth, 1),
                        na.rm = TRUE)
      # fit LOWESS for data_i
      fit_i <- suppressWarnings(
        lowess(x = data_i[["ROUND_TIME"]],
               y = data_i[["Residuals"]],
               f = max_smooth))
      fit_i_df <- unique(as.data.frame(fit_i))
      fit_vals <- fit_i_df$y
      data_i_seq <- as.POSIXct(fit_i_df$x)
    }

    pred_df <- data.frame(TIME = data_i_seq,
                          FITTED_VALUE = fit_vals,
                          GROUP = rep(data_i[[group_vars]][1],
                                      length(data_i_seq)))
    return(res_round_tp = pred_df[which(!is.na(pred_df$FITTED_VALUE)), ])
  })
  # https://stackoverflow.com/a/39838759
  fit_groups <- dplyr::bind_rows(processed_grouped_ds1)
  # Memory consumption
  rm(grouped_ds1)

  # Plotting ------------------------------------------------------------------
  if (length(co_vars) > 0) {
    if (length(co_vars) < 10) {
      subtitle <- sprintf("adjusted for %s", paste0(co_vars, collapse = ", "))
    } else {
      subtitle <- sprintf("adjusted for %d variables", length(co_vars))
    }
  } else {
    subtitle <- ""
  }

  if (length(levels(ds1[[group_vars]])) <= 8) {
    hex_code <- c(
      "#56B4E9", "#E69F00", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
    )
    names(hex_code) <- as.character(levels(ds1[[group_vars]]))
  } else {
    hex_code <- NULL
  }

  y_min <- mean(ds1$Residuals) - sd(ds1$Residuals)
  y_max <- mean(ds1$Residuals) + sd(ds1$Residuals)

  # Facet-Grids for categorical variable (observer/device)
  p1 <- ggplot(fit_groups,
               aes(x = .data$TIME,
                   y = .data$FITTED_VALUE,
                   color = .data$GROUP)) + {
                     if (!is.null(hex_code)) {
                       scale_color_manual(values = hex_code)
                     }} +
    xlab(rvs_bin_note) +
    ylab("") +
    geom_line() +
    facet_wrap(~ .data$GROUP, ncol = 2) + #TODO: What about this ~?
    expand_limits(y = c(y_min, y_max)) +
    theme_minimal() +
    ggtitle(plot_title, subtitle) +
    theme(legend.title = element_blank())

  # combined plot
  p2 <- ggplot(fit_groups,
               aes(x = .data$TIME,
                   y = .data$FITTED_VALUE,
                   group = .data$GROUP,
                   color = .data$GROUP)) + {
                     if (!is.null(hex_code)) {
                       scale_color_manual(values = hex_code)
                     }} +
    xlab(rvs_bin_note) +
    ylab("") +
    geom_line() +
    expand_limits(y = c(y_min, y_max)) +
    theme_minimal() +
    ggtitle(plot_title, subtitle)

  if (length(levels(ds1[[group_vars]])) > 1) {
    p2 <- p2 + theme(legend.title = element_blank())
  } else {
    p2 <- p2 + theme(legend.position = "none")
  }

  p1 <- util_set_size(p1,
                      width_em = 45,
                      height_em = length(levels(ds1[[group_vars]])) * 15 / 2)
  p2 <- util_set_size(p2, 30, 15)

  pl <- list(
    Loess_fits_facets = p1,
    Loess_fits_combined = p2
  )

  if (length(plot_format) != 1 || !is.character(plot_format)) {
    plot_format <- "NOT character(1) STRING AT ALL"
  }

  # Add attribute with size hints to the combined plot
  if (!is.null(pl[["Loess_fits_combined"]])) {
    obj1 <- ggplot2::ggplot_build(pl[["Loess_fits_combined"]])
    min_point_line <- min(util_rbind(data_frames_list = obj1$data)$y, na.rm = TRUE)
    max_point_line <- max(util_rbind(data_frames_list = obj1$data)$y, na.rm = TRUE)
    n_groups <- length(unique(util_rbind(data_frames_list = obj1$data)$group))
    min_time <- min(util_rbind(data_frames_list = obj1$data)$x, na.rm = TRUE)
    max_time <- max(util_rbind(data_frames_list = obj1$data)$x, na.rm = TRUE)
    rm(obj1)
  }

  if (plot_format == "BOTH") {
    return(list(SummaryPlotList = pl))
  } else if (plot_format == "COMBINED") {

    return(util_attach_attr(
      list(SummaryPlotList = setNames(pl["Loess_fits_combined"],
                                      nm = resp_vars)),
      sizing_hints = list(
        figure_type_id = "dot_loess",
        range = max_point_line - min_point_line,
        no_char_y = max(nchar(c(round(max_point_line, digits = 2),
                                round(min_point_line, digits = 2)))),
        n_groups = n_groups
      )))
  } else if (plot_format == "FACETS") {
    return(list(SummaryPlotList = setNames(pl["Loess_fits_facets"],
                                           nm = resp_vars)))
  } else if (plot_format != "AUTO") {
    util_message("Unknown %s: %s -- will switch to default value AUTO.",
                 dQuote("plot_format"), dQuote(plot_format),
                 applicability_problem = TRUE)
  }
#  if (length(levels(ds1[[group_vars]])) < 15) {
    selection <- "Loess_fits_combined"
#  } else {
#    selection <- "Loess_fits_facets"
#  }
  pl <- pl[selection]
  names(pl) <- resp_vars

  return(util_attach_attr(
    list(SummaryPlotList = pl),
    sizing_hints = list(
      figure_type_id = "dot_loess",
      range = max_point_line - min_point_line,
      no_char_y = max(nchar(c(round(max_point_line, digits = 2),
                              round(min_point_line, digits = 2)))),
      n_groups = n_groups
    )))
  # TODO: This function can only be called with single resp_vars, not with a
  # vector of resp_vars. Should the output be renamed to 'SummaryPlot'?
}
