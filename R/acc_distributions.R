#' Plots and checks for distributions
#'
#' @description
#' Data quality indicator checks "Unexpected location" and "Unexpected
#' proportion" with histograms and, if a grouping variable is included, plots of
#' empirical cumulative distributions for the subgroups.
#'
#' @details
#' # Algorithm of this implementation:
#'
#' - If no response variable is defined, select all variables of type float or
#' integer in the study data.
#' - Remove missing codes from the study data (if defined in the metadata).
#' - Remove measurements deviating from (hard) limits defined in the metadata
#' (if defined).
#' - Exclude variables containing only `NA` or only one unique value (excluding
#' `NA`s).
#' - Perform check for "Unexpected location" if defined in the metadata (needs a
#' LOCATION_METRIC (mean or median) and LOCATION_RANGE (range of expected values
#' for the mean and median, respectively)).
#' - Perform check for "Unexpected proportion" if defined in the metadata (needs
#' PROPORTION_RANGE (range of expected values for the proportions of the
#' categories)).
#' - Plot histogram(s).
#' - If group_vars is specified by the user, distributions within group-wise
#' ecdf are presented.
#'
#' @export
#'
#' @param resp_vars [variable list] the names of the measurement variables
#' @param group_vars [variable list] the name of the observer, device or
#'                                   reader variable
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param plot_ranges [logical] Should the plot show ranges and results from the
#'                              data quality checks? (default: TRUE)
#' @param check_param [enum] any | location | proportion. Which type of check
#'                         should be conducted (if possible): a check on the
#'                         location of the mean or median value of the study
#'                         data, a check on proportions of categories, or either
#'                         of them if the necessary metadata is available.
#' @param flip_mode [enum] default | flip | noflip | auto. Should the plot be
#'                         in default orientation, flipped, not flipped or
#'                         auto-flipped. Not all options are always supported.
#'                         In general, this con be controlled by
#'                         setting the `roptions(dataquieR.flip_mode = ...)`. If
#'                         called from `dq_report`, you can also pass
#'                         `flip_mode` to all function calls or set them
#'                         specifically using `specific_args`.
#'
#' @return A [list] with:
#'   - `SummaryTable`: [data.frame] containing data quality checks for
#'                     "Unexpected location" (`FLG_acc_ud_loc`) and "Unexpected
#'                     proportion" (`FLG_acc_ud_prop`) for each response
#'                     variable in `resp_vars`.
#'   - `SummaryData`: a [data.frame] containing data quality checks for
#'                    "Unexpected location" and / or "Unexpected proportion"
#'                    for a report
#'   - `SummaryPlotList`: [list] of [ggplot]s for each response variable in
#'                    `resp_vars`.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_bar geom_vline stat_ecdf
#'                     geom_segment scale_x_continuous scale_color_manual
#'                     coord_flip theme_minimal theme element_text element_blank
#'                     labs ylab
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom grDevices hcl.colors
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions <- function(resp_vars = NULL, group_vars = NULL,
                              study_data, meta_data,
                              label_col,
                              check_param = c("any", "location", "proportion"),
                              plot_ranges = TRUE,
                              flip_mode = "noflip") {

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  # If no response variable is defined, all suitable variables will be selected.
  if (length(resp_vars) == 0) {
    util_message(
      c("All variables defined to be integer or float in the metadata are used",
        "by acc_distributions."),
      applicability_problem = TRUE, intrinsic_applicability_problem = TRUE)
    resp_vars <- meta_data[[label_col]][meta_data$DATA_TYPE %in%
                                          c("integer", "float")]
    resp_vars <- intersect(resp_vars, colnames(ds1))
    if (length(resp_vars) == 0) {
      util_error("No suitable variables were defined for acc_distributions.",
                 applicability_problem = TRUE)
    }
  }

  # set up grouping variables, if needed
  if (all(is.na(group_vars))) {
    group_vars <- NULL
  } else {
    util_correct_variable_use("group_vars",
                              allow_null = TRUE,
                              allow_more_than_one = FALSE,
                              allow_any_obs_na = TRUE,
                              allow_all_obs_na = FALSE,
                              need_type = "!float"
    )
    # TODO: Still needed? Utility function?
    if (length(group_vars) > 0) {
      # all labelled variables
      levlabs <- meta_data$VALUE_LABELS[meta_data[[label_col]] %in% group_vars]

      if (any(grepl("=", levlabs) | is.na(levlabs))) {
        # any variables without labels?
        if (any(is.na(levlabs))) {
          util_warning(paste0(
            "Variables ", paste0(group_vars[is.na(levlabs)], collapse = ", "),
            " have no assigned labels and levels and can therefore not be ",
            "used as grouping variables in acc_distributions."
          ), applicability_problem = TRUE)
        }

        # only variables with labels
        gvs_ll <- group_vars[!is.na(levlabs)]
        gvs_ll <- gvs_ll[match(gvs_ll,
                               meta_data[[label_col]][
                                 meta_data[[label_col]] %in% group_vars])]
        levlabs <- levlabs[!is.na(levlabs)]

        for (i in seq_along(gvs_ll)) {
          ds1[[gvs_ll[i]]] <- util_assign_levlabs(
            variable = ds1[[gvs_ll[i]]],
            string_of_levlabs = levlabs[i],
            splitchar = SPLIT_CHAR,
            assignchar = " = "
          )
        }
      }

      # The grouping variable(s) should not be included as response variable(s).
      if (any(group_vars %in% resp_vars)) {
        resp_vars <- resp_vars[-which(resp_vars %in% group_vars)]
        util_warning(paste("Removed grouping variable from response variables",
                           "for acc_distributions."),
                     applicability_problem = TRUE)
      }
      if (length(resp_vars) == 0) {
        util_warning("No variables left to analyse for acc_distributions.",
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
        return(list(SummaryTable = list(),
                    SummaryPlotList = list(),
                    SummaryData = list()))
      }
    }
  }

  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 2,
                            do_not_stop = ifelse(length(resp_vars) > 1,
                                                 TRUE, FALSE),
                            need_type = "integer | float"
  )

  # Which parameter of the distribution should be checked?
  dq_param <- match.arg(check_param)

  # DQ indicator "Unexpected location" requires metadata for the range of
  # expected values for the location parameter and the location metric.
  # DQ indicator "Unexpected proportion" requires metadata for the range of
  # expected percentages for the categories.
  if (!any(grepl("_RANGE", colnames(meta_data)))) {
    util_message(paste("Metadata does not contain columns for expected ranges",
                       "of location or proportions. So acc_distributions can",
                       "generate plots, but can not perform data quality",
                       "checks."), applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    # create empty metadata list
    rvs_meta <- list("Metric" = setNames(nm = resp_vars,
                                         rep(NA, length(resp_vars))),
                     "Range" = setNames(nm = resp_vars,
                                        rep(NA, length(resp_vars))))
  } else {
    if (dq_param == "any") {
      # split variables for location and proportion checks and call
      # acc_distributions recursively
      rvs_meta_prop <- suppressMessages(
        util_prep_proportion_check(
          resp_vars = resp_vars,
          meta_data = meta_data,
          study_data = ds1,
          report_problems = "message"
        )
      )
      rvs_with_prop <- names(rvs_meta_prop$Range)[which(
        !is.na(rvs_meta_prop$Range))]
      rvs_meta_loc <- suppressMessages(
        util_prep_location_check(
          resp_vars = resp_vars,
          meta_data = meta_data,
          report_problems = "message"
        )
      )
      rvs_with_loc <- names(rvs_meta_loc$Range)[which(
        !is.na(rvs_meta_loc$Range))]
      rvs_with_none <-
        setdiff(resp_vars, c(rvs_with_prop, rvs_with_loc))

      if (length(intersect(rvs_with_prop, rvs_with_loc)) > 0) {
        # For some variables, we might have metadata for location AND for
        # proportion checks, which does not make sense.
        util_warning(
          paste0("For ",
                 paste(intersect(rvs_with_prop, rvs_with_loc), collapse = ", "),
                 ", metadata for both, location and proportion checks, are ",
                 "given. This is considered an error and only proportion ",
                 "checks will be performed."),
          applicability_problem = FALSE)
        rvs_with_loc <- setdiff(rvs_with_loc, rvs_with_prop)
      }

      if (length(rvs_with_loc) > 0) {
        res_loc <- acc_distributions(
          resp_vars = rvs_with_loc,
          group_vars = group_vars, study_data = study_data,
          meta_data = meta_data, label_col = label_col,
          check_param = "location", plot_ranges = plot_ranges,
          flip_mode = flip_mode)
      } else {
        res_loc <- NULL
      }

      res_merged <- res_loc

      if (length(rvs_with_prop) > 0) {
        res_prop <- acc_distributions(
          resp_vars = rvs_with_prop,
          group_vars = group_vars, study_data = study_data,
          meta_data = meta_data, label_col = label_col,
          check_param = "proportion", plot_ranges = plot_ranges,
          flip_mode = flip_mode)
        if (!is.null(res_merged)) {
          res_merged$SummaryTable <- merge(res_merged$SummaryTable,
                                           res_prop$SummaryTable,
                                           all = TRUE, sort = FALSE)
          res_merged$SummaryData <- merge(res_merged$SummaryData,
                                          res_prop$SummaryData,
                                          all = TRUE, sort = FALSE)
          res_merged$SummaryPlotList <- c(res_merged$SummaryPlotList,
                                          res_prop$SummaryPlotList)
        } else {
          res_merged <- res_prop
        }
      }

      if (length(rvs_with_none) > 0) {
        util_message(paste0("For ", paste(rvs_with_none, collapse = ", "),
                            ", there is no metadata on expected location or",
                            " expected proportions available."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
        res_no_meta <- suppressWarnings(acc_distributions(
          resp_vars = rvs_with_none,
          group_vars = group_vars, study_data = study_data,
          meta_data = meta_data, label_col = label_col,
          check_param = "location", plot_ranges = plot_ranges,
          flip_mode = flip_mode))
        if (!is.null(res_merged)) {
          res_merged$SummaryTable <- merge(res_merged$SummaryTable,
                                           res_no_meta$SummaryTable,
                                           all = TRUE, sort = FALSE)
          res_merged$SummaryData <- merge(res_merged$SummaryData,
                                          res_no_meta$SummaryData,
                                          all = TRUE, sort = FALSE)
          res_merged$SummaryPlotList <- c(res_merged$SummaryPlotList,
                                          res_no_meta$SummaryPlotList)
        } else {
          res_merged <- res_no_meta
        }
      }

      # order merged results to match the order of `resp_vars`
      if (length(resp_vars) > 1) {
        res_merged$SummaryTable <- res_merged$SummaryTable[
          match(resp_vars, res_merged$SummaryTable$Variables), ]
        res_merged$SummaryData <- res_merged$SummaryData[
          match(resp_vars, res_merged$SummaryData$Variables), ]
        res_merged$SummaryPlotList <- res_merged$SummaryPlotList[resp_vars]
      }

      return(res_merged)

    } else if (dq_param == "location") {
      rvs_meta <- util_prep_location_check(resp_vars = resp_vars,
                                           meta_data = meta_data,
                                           report_problems = "warning")
    } else {
      rvs_meta <- util_prep_proportion_check(resp_vars = resp_vars,
                                             meta_data = meta_data,
                                             study_data = ds1,
                                             report_problems = "warning")
    }
  }

  rvs_with_meta <- names(rvs_meta$Range)[which(!is.na(rvs_meta$Range))]

  # data quality indicator checks ----------------------------------------------
  if (dq_param == "location") {
    dq_check_list <- lapply(setNames(nm = resp_vars), function(rv) {
      rv_data <- ds1[[rv]]
      loc_func <- rvs_meta$Metric[[rv]]
      if (!is.na(loc_func)) {
        loc_val <- do.call(loc_func, args = list(x = rv_data, na.rm = TRUE))
        loc_check <- !(redcap_env$`in`(loc_val, rvs_meta$Range[[rv]]))
      } else {
        loc_val <- NA
        loc_check <- NA
      }

      loc_res <- setNames(nm = c("Variables",
                                 paste(c("FLG","VAL"), "acc_ud_loc", sep = "_"),
                                 "loc_func",
                                 "loc_range"),
                          list(rv,
                               loc_check,
                               loc_val,
                               loc_func,
                               util_find_var_by_meta(resp_vars = rv,
                                                     target = "LOCATION_RANGE",
                                                     meta_data = meta_data)))
      loc_res
    })
  } else if (dq_param == "proportion") {
    dq_check_list <- lapply(setNames(nm = resp_vars), function(rv) {
      rv_data <- ds1[[rv]]
      # compute proportions for variable `rv` in the study data
      prop_table <- table(rv_data)
      prop_table <- prop_table/length(rv_data[which(!is.na(rv_data))]) * 100
      # If some categories were expected but not observed, we have to add zeroes
      # to the table.
      if (!all(names(rvs_meta$Range[[rv]]) %in% names(prop_table))) {
        nonmiss_cat <- names(prop_table)
        miss_cat <- setdiff(names(rvs_meta$Range[[rv]]), nonmiss_cat)
        prop_table <- setNames(c(prop_table, rep(0, length(miss_cat))),
                               nm = c(nonmiss_cat, miss_cat))
      }
      # order categories as in value labels list
      prop_table <- prop_table[names(rvs_meta$Range[[rv]])]
      # check for each category of the variable whether the proportion lies
      # within the expected range
      prop_in_range <- mapply(as.list(prop_table),
                              rvs_meta$Range[[rv]],
                              FUN = function(pp, int) {
                                if (inherits(int, "interval")) {
                                  redcap_env$`in`(pp, int)
                                } else {
                                  NA
                                }
                              })
      # Which categories of the variable should be flagged, if any?
      cat_flg <- names(prop_in_range)[which(!prop_in_range)]
      if (length(cat_flg) == 0) cat_flg <- NA

      prop_res <-  setNames(nm = c(
        "Variables",
        paste(c("FLG", "VAL"), "acc_ud_prop", sep = "_"),
        "prop_range",
        "flg_which"),
        list(rv,
             any(!(prop_in_range[which(!is.na(prop_in_range))])),
             paste(
               paste(names(prop_table), round(prop_table, 1), sep = " = "),
               collapse = " | "),
             util_find_var_by_meta(resp_vars = rv,
                                   target = "PROPORTION_RANGE",
                                   meta_data = meta_data),
             cat_flg))
      prop_res
    })

    # remove the list of flagged categories to arrange results as dataframe
    flg_cat_prop <- lapply(dq_check_list, function(x) { x[["flg_which"]] })
    dq_check_list <- lapply(dq_check_list, function(x) {
      x[-which(names(x) == "flg_which")]
    })
  } else {
    dq_check_list <- list()
  }

  res_dq_check <- do.call(rbind.data.frame, dq_check_list)
  rownames(res_dq_check) <- NULL

  # summary table --------------------------------------------------------------
  res_out <- res_dq_check[, 1:3]
  # GRADING for backwards compatibility
  res_out$GRADING <- as.numeric(res_out[, 2])
  res_out$GRADING[which(is.na(res_out$GRADING))] <- 0

  # summary data ---------------------------------------------------------------
  if (dq_param == "location") {
    res_view <- data.frame("Variables" = res_dq_check$Variables,
                           "Measure of location" = res_dq_check$loc_func,
                           "Value" = res_dq_check$VAL_acc_ud_loc,
                           "Range of expected values" =
                             res_dq_check$loc_range,
                           "Flag" = res_dq_check$FLG_acc_ud_loc,
                           check.names = FALSE)
  } else if (dq_param == "proportion") {
    res_view <- data.frame("Variables" = res_dq_check$Variables,
                           "Proportions" = res_dq_check$VAL_acc_ud_prop,
                           "Range of expected values" =
                             res_dq_check$prop_range,
                           "Flag" = res_dq_check$FLG_acc_ud_prop,
                           check.names = FALSE)
  }

  # plot -----------------------------------------------------------------------
  ref_env <- environment()
  plot_list <- lapply(setNames(nm = resp_vars), function(rv) {
    # omit NAs from data to prevent ggplot2 warning messages
    ds1 <- ds1[!(is.na(ds1[[rv]])), , drop = FALSE]

    # Should the plot be a histogram? If not, it will be a bar chart.
    if ("VALUE_LABELS" %in% colnames(meta_data)) {
      plot_histogram <-  !all(util_is_integer(ds1[[rv]])) ||
        (length(unique(ds1[[rv]])) > 30 &&
           !(dq_param == "proportion" && rv %in% rvs_with_meta) &&
           util_empty(meta_data$VALUE_LABELS[which(meta_data[[label_col]] ==
                                                     rv)]))
    } else {
      plot_histogram <- !all(util_is_integer(ds1[[rv]])) ||
        (length(unique(ds1[[rv]])) > 30 &&
           !(dq_param == "proportion" && rv %in% rvs_with_meta))
    }
    # A histogram will be plotted for continuous variables, including numeric
    # non-integer variables and integer variables with more than 30 values.
    # Discrete variables will be plotted in a bar chart.
    # If value labels or expected proportion ranges are given in the metadata,
    # then the plot should be a bar chart, even if the variable is an integer
    # variable with more than 30 different values.

    txtspec <- element_text(
      colour = "black", hjust = .5,
      vjust = .5, face = "plain"
    )

    col_bars <- "grey"
    blue_red <- hcl.colors(10, "Plasma")[c(1,6)]
    if (plot_histogram) {
      # histogram --------------------------------------------------------------
      minimum <- min(ds1[[rv]])
      maximum <- max(ds1[[rv]])
      xlims <- c(minimum, maximum)

      # compute bin breaks
      bin_breaks <- util_optimize_histogram_bins(
        x = ds1[[rv]],
        iqr_bw = IQR(ds1[[rv]]),
        n_bw = length(ds1[[rv]]),
        min_plot = minimum,
        max_plot = maximum,
        nbins_max = 100
      )
      breaks_x <- bin_breaks$within

      # plot histogram
      p <- ggplot(data = ds1[, rv, drop = FALSE], aes(x = .data[[rv]])) +
        geom_histogram(breaks = breaks_x,
                       fill = col_bars,
                       color = col_bars) +
        scale_x_continuous(expand = expansion(mult = 0.1),
                           name = paste0(rv))

    } else {
      # bar chart --------------------------------------------------------------
       if (dq_param == "proportion" & rv %in% rvs_with_meta) {
        col_bars_sep <- setNames(nm = names(rvs_meta$Range[[rv]]),
                                 rep(col_bars,
                                     length(names(rvs_meta$Range[[rv]]))))
        if (plot_ranges) {
          # if bars outside of expected ranges shall be highlighted
          col_bars_sep[flg_cat_prop[[rv]]] <- blue_red[2]
        }
        col_bars <- col_bars_sep
        rv_fact <- factor(ds1[[rv]], levels = names(col_bars))
        count_tab <- table(rv_fact)
        col_bars <- col_bars[names(count_tab)[which(count_tab > 0)]]
        # bar for 0 counts has 0 height and needs no color
        # (would cause a ggplot error)
        exp_bars <- as.numeric(names(col_bars_sep)) # expected bars
      } else {
        exp_bars <- sort(unique(ds1[[rv]]))
      }

      if ("VALUE_LABELS" %in% colnames(meta_data) &&
          !util_empty(meta_data$VALUE_LABELS[which(meta_data[[label_col]] ==
                                                   rv)])) {
        ds1[[rv]] <- factor(ds1[[rv]], levels = exp_bars)
        val_lab <- util_parse_assignments(
          meta_data[[VALUE_LABELS]][which(meta_data[[label_col]] == rv)])
        if (all(levels(ds1[[rv]]) %in% names(val_lab))) {
          # TODO: Should the other be discarded?
          levels(ds1[[rv]]) <- vapply(levels(ds1[[rv]]),
                                      function(old_lev) {
                                        val_lab[[old_lev]]
                                      },
                                      FUN.VALUE = character(1))
          exp_bars <- levels(ds1[[rv]])
        }
      }

      p <- ggplot(data = ds1[, rv, drop = FALSE],
                  aes(x = .data[[rv]])) +
        geom_bar(fill = col_bars)

      if (is.factor(ds1[[rv]])) {
        p <- p +
          scale_x_discrete(name = paste0(rv),
                           breaks = as.character(exp_bars),
                           drop = FALSE,
                           expand = expansion(add = 0.5, mult = 0.1))
      } else if (length(unique(ds1[[rv]])) <= 5) {
        # If there are only few bars, print a label for each bar at the x-axis.
        p <- p +
          scale_x_continuous(breaks = exp_bars,
                             expand = expansion(mult = 0.1),
                             name = paste0(rv),
                             limits = range(exp_bars) + c(-0.5, 0.5))
      } else {
        p <- p +
          scale_x_continuous(expand = expansion(mult = 0.1),
                             name = paste0(rv),
                             limits = range(exp_bars) + c(-0.5, 0.5))
      }
    }

    # add ranges (if needed) ---------------------------------------------------
    if (plot_ranges) {
      # location
      if (dq_param == "location" & rv %in% rvs_with_meta) {
        # lower limit
        ll <- rvs_meta$Range[[rv]][["low"]]
        ll <- ifelse(is.infinite(ll), NA, ll)
        if (!is.na(ll)) {
          p <- p + geom_vline(xintercept = ll,
                              linetype = 2,
                              col = blue_red[1])
        }
        # upper limit
        ul <- rvs_meta$Range[[rv]][["upp"]]
        ul <- ifelse(is.infinite(ul), NA, ul)
        if (!is.na(ul)) {
          p <- p + geom_vline(xintercept = ul,
                              linetype = 2,
                              col = blue_red[1])
        }
        # add mean or median as line
        loc_val <- dq_check_list[[rv]][["VAL_acc_ud_loc"]]
        if (!is.na(loc_val)) {
          loc_flg <- dq_check_list[[rv]][["FLG_acc_ud_loc"]]
          if (!is.na(loc_flg) & !loc_flg) {
            loc_col <- blue_red[1]
          } else {
            loc_col <- blue_red[2]
          }
          p <- p + geom_vline(xintercept = loc_val,
                              col = loc_col)
        }
      }
      # proportions
      if (dq_param == "proportion" & rv %in% rvs_with_meta) {
        # add lines for each category (if available)
        for (pp in seq_along(rvs_meta$Range[[rv]])) {
          if (inherits(rvs_meta$Range[[rv]][[pp]], "interval")) {
            if (is.factor(ds1[[rv]])) {
              pp_num <- pp
            } else {
              pp_num <- as.numeric(names(rvs_meta$Range[[rv]])[pp])
            }
            # lower limit
            ll <- rvs_meta$Range[[rv]][[pp]][["low"]]
            ll <- ifelse(is.infinite(ll), NA, ll)
            if (!is.na(ll)) {
              pp_coord <- data.frame(x1 = pp_num - 0.5,
                                     x2 = pp_num + 0.5,
                                     y1 = ll / 100 * nrow(ds1),
                                     y2 = ll / 100 * nrow(ds1))
              p <- p + geom_segment(aes(x = .data[["x1"]],
                                        xend = .data[["x2"]],
                                        y = .data[["y1"]],
                                        yend = .data[["y2"]]),
                                    data = pp_coord,
                                    linetype = 2,
                                    col = blue_red[1])
            }
            # upper limit
            ul <- rvs_meta$Range[[rv]][[pp]][["upp"]]
            ul <- ifelse(is.infinite(ul), NA, ul)
            if (!is.na(ul)) {
              pp_coord <- data.frame(x1 = pp_num - 0.5,
                                     x2 = pp_num + 0.5,
                                     y1 = ul / 100 * nrow(ds1),
                                     y2 = ul / 100 * nrow(ds1))
              p <- p + geom_segment(aes(x = .data[["x1"]],
                                        xend = .data[["x2"]],
                                        y = .data[["y1"]],
                                        yend = .data[["y2"]]),
                                    data = pp_coord,
                                    linetype = 2,
                                    col = blue_red[1])
            }
          }
        }
      }
    }

    fli <- util_coord_flip(p = p, ref_env = ref_env)

    p <- p +
      fli +
      # TODO: estimate w and h, if p is not using discrete axes
      ylab("") +
      theme_minimal() +
      theme(
        title = txtspec,
        axis.text.x = txtspec,
        axis.text.y = txtspec,
        axis.title.x = txtspec,
        axis.title.y = txtspec
      )

    # ecdf by group_vars -------------------------------------------------------
    if (length(group_vars) > 0 & plot_histogram) {
      ds1[[group_vars]] <- as.factor(ds1[[group_vars]]) # there is only one
      # grouping variable, we have ensured this by allow_more_than_one = FALSE
      pp <- ggplot(data = ds1[, c(rv, group_vars), drop = FALSE],
                   aes(x = .data[[rv]], colour = .data[[group_vars]])) +
        fli +
        stat_ecdf(geom = "step") +
        labs(x = "", y = paste0("ECDF: ", rv, " (by ",
                                paste0(group_vars, collapse = ", "), ")")) +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec,
          legend.title = element_blank()
        )

      if (util_ensure_suggested("colorspace",
                                "use the colorspace color scale",
                                err = FALSE)) {
        pp <- pp +
          colorspace::scale_color_discrete_sequential(palette = "Plasma",
                                                      na.value = "grey")
      }

      P <- p + pp +
        plot_layout(ncol = 1) +
        plot_annotation(tag_levels = 'A')
    } else {
      P <- p
    }

    util_set_size(P)
  })

  return(util_attach_attr(list(SummaryTable = res_out,
              SummaryData = res_view,
              SummaryPlotList = plot_list),
         as_plotly = "util_as_plotly_acc_distributions"))
}

#' Plots and checks for distributions -- Location
#' @inherit acc_distributions
#' @export
#' @seealso
#' - [acc_distributions]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_loc <- function(resp_vars = NULL,
                                  study_data,
                                  meta_data,
                                  label_col,
                                  check_param = "location",
                                  plot_ranges = TRUE,
                                  flip_mode = "noflip") {
  rvs_meta <- util_prep_location_check(resp_vars = resp_vars,
                                       meta_data = meta_data,
                                       report_problems = "error")
  acc_distributions(
    resp_vars = resp_vars, group_vars = NULL, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    check_param = "location", plot_ranges = plot_ranges,
    flip_mode = flip_mode
  )
}

#' Plots and checks for distributions -- Location, ECDF
#' @inherit acc_distributions
#' @export
#' @seealso
#' - [acc_distributions]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_loc_ecdf <- function(resp_vars = NULL,
                                       group_vars = NULL,
                                  study_data,
                                  meta_data,
                                  label_col,
                                  check_param = "location",
                                  plot_ranges = TRUE,
                                  flip_mode = "noflip") {
  rvs_meta <- util_prep_location_check(resp_vars = resp_vars,
                                       meta_data = meta_data,
                                       report_problems = "error")
  acc_distributions(
    resp_vars = resp_vars, group_vars = group_vars, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    check_param = "location", plot_ranges = plot_ranges,
    flip_mode = flip_mode
  )
}

#' Plots and checks for distributions -- Proportion
#' @inherit acc_distributions
#' @export
#' @seealso
#' - [acc_distributions]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_prop <- function(resp_vars = NULL,
                                   study_data,
                                   meta_data,
                                   label_col,
                                   check_param = "proportion",
                                   plot_ranges = TRUE,
                                   flip_mode = "noflip") {
  md_prep <- util_prep_proportion_check(
    resp_vars = resp_vars,
    meta_data = meta_data,
    study_data = study_data,
    report_problems = "error"
  )

  acc_distributions(
    resp_vars = resp_vars, group_vars = NULL, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    check_param = "proportion", plot_ranges = plot_ranges,
    flip_mode = flip_mode
  )
}

util_has_no_group_vars <- function(resp_vars,
                                   meta_data = "item_level",
                                   label_col = LABEL) {
  util_expect_scalar(resp_vars)
  util_expect_data_frame(meta_data)
  columns <- grep("^GROUP_VAR_.*", colnames(meta_data), value = TRUE)
  resp_vars <- util_find_var_by_meta(resp_vars,
                        meta_data = meta_data,
                        label_col = label_col)
  if (is.na(resp_vars)) {
    return(TRUE)
  }
  rowSums(!util_empty(meta_data[meta_data[["VAR_NAMES"]] == resp_vars,
            columns, FALSE])) == 0
}

#' Plots and checks for distributions -- only
#' @inherit acc_distributions
#' @export
#' @seealso
#' - [acc_distributions]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_only <- function(resp_vars = NULL,
                                   study_data,
                                   meta_data,
                                   label_col,
                                   flip_mode = "noflip") {
  loc_avail <- !inherits(try(util_prep_location_check(resp_vars = resp_vars,
                                       meta_data = meta_data,
                                       report_problems = "error"),
                   silent = TRUE), "try-error")
  if (loc_avail) {
    util_error("%s is already in a distribution location check plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  prop_avail <- !inherits(try(util_prep_proportion_check(
                    resp_vars = resp_vars,
                    meta_data = meta_data,
                    study_data = study_data,
                    report_problems = "error"
                  ), silent = TRUE), "try-error")
  if (prop_avail) {
    util_error("%s is already in a distribution proportion check plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  if (!util_has_no_group_vars(resp_vars = resp_vars,
                         meta_data = meta_data,
                         label_col = label_col)) {
    util_error("%s is already in a ecdf/distribution plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  acc_distributions(
    resp_vars = resp_vars, group_vars = NULL, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    flip_mode = flip_mode
  )
}

#' Plots and checks for distributions -- only, but with ecdf
#' @inherit acc_distributions
#' @export
#' @seealso
#' - [acc_distributions]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_only_ecdf <- function(resp_vars = NULL,
                                   study_data,
                                   group_vars = NULL,
                                   meta_data,
                                   label_col,
                                   flip_mode = "noflip") {
  prep_prepare_dataframes()
  util_correct_variable_use(group_vars)
  loc_avail <- !inherits(try(util_prep_location_check(resp_vars = resp_vars,
                                                      meta_data = meta_data,
                                                      report_problems = "error"),
                             silent = TRUE), "try-error")
  if (loc_avail) {
    util_error("%s is already in a distribution location check plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  prop_avail <- !inherits(try(util_prep_proportion_check(
    resp_vars = resp_vars,
    meta_data = meta_data,
    study_data = study_data,
    report_problems = "error"
  ), silent = TRUE), "try-error")
  if (prop_avail) {
    util_error("%s is already in a distribution proportion check plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  acc_distributions(
    resp_vars = resp_vars, group_vars = group_vars, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    flip_mode = flip_mode
  )
}

util_as_plotly_acc_distributions <- function(res, ...) {
  if (length(res$SummaryPlotList) != 1) {
    return(plotly::ggplotly(ggplot2::ggplot() +
                              ggplot2::annotate("text", x = 0, y = 0,
                                                label =
  sprintf(paste("Internal error: I should  have exactly 1 result, if",
        "calling plotly for a dq_report2 otuput. I have %d."),
        length(res$SummaryPlotList))) +
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
  res$SummaryPlotList <-
    util_remove_dataquieR_result_class(res$SummaryPlotList)
  # use res$SummaryPlot, not something from the enclosure
  # of the result, that may contain study data.
  util_ensure_suggested("plotly")

  res$SummaryPlot <- res$SummaryPlotList[[1]]

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
    plotly::ggplotly(res$SummaryPlot, ...)
  }
}
