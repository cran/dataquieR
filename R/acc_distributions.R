#' Plots and checks for distributions
#'
#' @description
#' Data quality indicator checks "Unexpected location" and "Unexpected
#' proportion" with histograms.
#'
#' [Indicator]
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
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable list] the names of the measurement variables
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
#'   - `SummaryPlotList`: [list] of [ggplot2::ggplot]s for each response variable in
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
acc_distributions <- function(resp_vars = NULL,
                              study_data,
                              label_col,
                              item_level = "item_level",
                              check_param = c("any", "location", "proportion"),
                              plot_ranges = TRUE,
                              flip_mode = "noflip",
                              meta_data = item_level,
                              meta_data_v2) {
  # preps ----------------------------------------------------------------------
  # map metadata to study data
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          #.apply_factor_metadata = TRUE, # can be omitted in favor of .apply_factor_metadata_inadm
                          .apply_factor_metadata_inadm = TRUE
  )

  # If no response variable is defined, all suitable variables will be selected.
  if (length(resp_vars) == 0) {
    util_message(
      c("All variables defined to be integer or float in the metadata are used",
        "by acc_distributions."), # TODO: extend for nominal and ordinal variables
      applicability_problem = TRUE, intrinsic_applicability_problem = TRUE)
    resp_vars <- meta_data[[label_col]][meta_data$DATA_TYPE %in%
                                          c("integer", "float")]
    resp_vars <- intersect(resp_vars, colnames(ds1))
    if (length(resp_vars) == 0) {
      util_error("No suitable variables were defined for acc_distributions.",
                 applicability_problem = TRUE)
    }
  }

  util_correct_variable_use(resp_vars,
                            allow_more_than_one = TRUE,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            min_distinct_values = 2,
                            do_not_stop = ifelse(length(resp_vars) > 1,
                                                 TRUE, FALSE),
                            need_scale = "!na"
  )

  # Which parameter of the distribution should be checked?
  dq_param <- util_match_arg(check_param)

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
      scale_level <- meta_data[[SCALE_LEVEL]][match(resp_vars,
                                                    meta_data[[label_col]])]
      if (any(scale_level %in% c("interval", "ratio"))) {
        rvs_loc <- resp_vars[scale_level %in%
                               c("interval", "ratio")]
        res_loc <- suppressWarnings(acc_distributions(
          resp_vars = rvs_loc, study_data = study_data,
          meta_data = meta_data, label_col = label_col,
          check_param = "location", plot_ranges = plot_ranges,
          flip_mode = flip_mode))

        res_merged <- res_loc
      }

      if (any(!(scale_level %in% c("interval", "ratio")))) {
        rvs_prop <- resp_vars[!(scale_level %in%
                                  c("interval", "ratio"))]
        res_prop <- suppressWarnings(acc_distributions(
          resp_vars = rvs_prop, study_data = study_data,
          meta_data = meta_data, label_col = label_col,
          check_param = "proportion", plot_ranges = plot_ranges,
          flip_mode = flip_mode))
        if (exists("res_merged")) {
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
                                           report_problems = "warning",
                                           label_col = label_col)
    } else {
      rvs_meta <- util_prep_proportion_check(resp_vars = resp_vars,
                                             meta_data = meta_data,
                                             ds1 = ds1,
                                             report_problems = "warning",
                                             label_col = label_col)
    }
  }

  rvs_with_meta <- names(rvs_meta$Range)[which(!is.na(rvs_meta$Range))]

  # Which variables are of type 'datetime'?
  is_datetime_var <- vapply(resp_vars, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] %in%
      c(DATA_TYPES$DATETIME)
  }, FUN.VALUE = logical(1))

  # Which variables are of type 'time'?
  is_time_var <- vapply(resp_vars, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] %in%
      c(DATA_TYPES$TIME)
  }, FUN.VALUE = logical(1))

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
                                 #                                paste(c("FLG","VAL"), "acc_ud_loc", sep = "_"),
                                 "FLG_acc_ud_loc", "values_from_data",
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
      prop_table <- prop_table[union(names(rvs_meta$Range[[rv]]),
                                     names(prop_table))]
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
        #        paste(c("FLG", "VAL"), "acc_ud_prop", sep = "_"),
        "FLG_acc_ud_prop", "values_from_data",
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
    flg_cat_prop <- list()
    dq_check_list <- list()
  }

  res_dq_check <- do.call(rbind.data.frame, dq_check_list)
  rownames(res_dq_check) <- NULL

  # summary table --------------------------------------------------------------
  if(ncol(res_dq_check)==0) { # in case there is no check for location or range, it provides a table without columns
    res_out <- res_dq_check
    res_view <- res_dq_check
  } else{
    res_out <- res_dq_check[, 1:4]
    # GRADING for backwards compatibility
    res_out$GRADING <- as.numeric(res_out[, 2])
    res_out$GRADING[which(is.na(res_out$GRADING))] <- 0
  }

  # summary data ---------------------------------------------------------------
  if (dq_param == "location") {
    res_view <- data.frame("Variables" = res_dq_check$Variables,
                           "Measure of location" = res_dq_check$loc_func,
                           "Value" = res_dq_check$values_from_data,
                           "Range of expected values" =
                             res_dq_check$loc_range,
                           "Flag" = res_dq_check$FLG_acc_ud_loc,
                           check.names = FALSE)
  } else if (dq_param == "proportion") {
    res_view <- data.frame("Variables" = res_dq_check$Variables,

                           "Proportions" = res_dq_check$values_from_data,
                           "Range of expected values" =
                             res_dq_check$prop_range,
                           "Flag" = res_dq_check$FLG_acc_ud_prop,
                           check.names = FALSE)
  }

  # plot -----------------------------------------------------------------------
  ref_env <- environment()
  plot_list <- lapply(setNames(nm = resp_vars), function(rv) {

    # find suitable lables
    lb <- paste0(prep_get_labels(rv,
                                 item_level = meta_data,
                                 label_col = label_col,
                                 resp_vars_match_label_col_only = TRUE,
                                 label_class = "SHORT"))

    # omit NAs from data to prevent ggplot2 warning messages
    ds1 <- ds1[!(is.na(ds1[[rv]])), , drop = FALSE]

    # Should the plot be a histogram? If not, it will be a bar chart.
    plot_histogram <-
      meta_data[[SCALE_LEVEL]][which(meta_data[[label_col]] == rv)] %in%
      c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$RATIO)

    txtspec <- element_text(
      colour = "black", hjust = .5,
      vjust = .5, face = "plain", size = 10
    )

    col_bars <- "#2166AC"
    blue_red <- hcl.colors(10, "Plasma")[c(1,6)]

    if (plot_histogram) {
      # histogram --------------------------------------------------------------
      .dt <- ds1[, rv, drop = FALSE]
      rv_datetime <- is_datetime_var[rv]
      rv_time <- is_time_var[rv]
      p <- util_create_lean_ggplot(
        util_histogram(plot_data = .dt,
                       is_datetime = rv_datetime,
                       is_time = rv_time) +
          xlab(lb),
        .dt = .dt,
        rv_datetime = rv_datetime,
        lb = lb,
        rv_time = rv_time
      )

      obj1 <- ggplot2::ggplot_build(p)
      no_char_y <- max(nchar(util_gg_get(obj1, "data")[[1]]$ymax))
      no_char_x <- 4

    } else {
      # bar chart --------------------------------------------------------------
      if (!is.factor(ds1[[rv]])) {
        lvls <- union(names(rvs_meta$Range[[rv]]), unique(ds1[[rv]]))
      } else {
        lvls <- levels(ds1[[rv]])
      }
      plot_data <- data.frame("cat" = factor(lvls, levels = lvls),
                              "count" = unlist(lapply(lvls, function(ll) {
                                length(which(ds1[[rv]] == ll)) })),
                              "col" = col_bars)
      if (plot_ranges & any(!is.na(flg_cat_prop[[rv]]))) {
        # if bars outside of expected ranges shall be highlighted
        plot_data[plot_data[["cat"]] %in% flg_cat_prop[[rv]], "col"] <- blue_red[2]
      }

      p <- util_bar_plot(plot_data = plot_data,
                         cat_var = "cat",
                         num_var = "count",
                         fill_var = "col",
                         colors = unique(plot_data[, "col"]),
                         relative = FALSE, show_numbers = FALSE, flip = FALSE)
      p <- p %lean+% util_create_lean_ggplot(xlab(lb), lb = lb)

      obj1 <- ggplot2::ggplot_build(p)
      no_char_y <- max(nchar(util_gg_get(obj1, "data")[[1]]$ymax))
      no_char_x <- max(nchar(lvls))
    }

    # add ranges (if needed) ---------------------------------------------------
    if (plot_ranges) {
      # location
      if (dq_param == "location" & rv %in% rvs_with_meta) {
        # lower limit
        ll <- rvs_meta$Range[[rv]][["low"]]
        ll <- ifelse(is.infinite(ll), NA, ll)
        if (!is.na(ll)) {
          p <- p %lean+% util_create_lean_ggplot(geom_vline(xintercept = ll,
                              linetype = 2,
                              col = blue_red[1]),
                              ll = ll,
                              blue_red = blue_red)
        }
        # upper limit
        ul <- rvs_meta$Range[[rv]][["upp"]]
        ul <- ifelse(is.infinite(ul), NA, ul)
        if (!is.na(ul)) {
          p <- p %lean+% util_create_lean_ggplot(geom_vline(xintercept = ul,
                              linetype = 2,
                              col = blue_red[1]),
                              blue_red = blue_red,
                              ul = ul)
        }
        # add mean or median as line
        loc_val <- dq_check_list[[rv]][["values_from_data"]]
        if (!is.na(loc_val)) {
          loc_flg <- dq_check_list[[rv]][["FLG_acc_ud_loc"]]
          if (!is.na(loc_flg) & !loc_flg) {
            loc_col <- blue_red[1]
          } else {
            loc_col <- blue_red[2]
          }
          p <- p %lean+% util_create_lean_ggplot(geom_vline(xintercept = loc_val,
                              col = loc_col),
                              loc_col = loc_col,
                              loc_val = loc_val)
        }
      }
      # proportions
      if (dq_param == "proportion" & rv %in% rvs_with_meta) {
        # add lines for each category (if available)
        # Create a list of small data frames, one per interval category
        x1 <- NULL # to
        x2 <- NULL # make
        y1 <- NULL # R CMD check
        y2 <- NULL # happy
        segments_list <- lapply(
          seq_along(rvs_meta$Range[[rv]]),
          function(pp) {
            interval_pp <- rvs_meta$Range[[rv]][[pp]]
            if (!inherits(interval_pp, "interval")) return(NULL)

            # Determine numeric position for this category
            pp_num <- if (is.factor(ds1[[rv]])) {
              pp
            } else {
              as.numeric(names(rvs_meta$Range[[rv]])[pp])
            }

            # Collect lower and upper limit frames
            out <- list()

            ll <- interval_pp[["low"]]
            if (!is.infinite(ll) && !is.na(ll)) {
              out[[length(out) + 1]] <- data.frame(
                x1 = pp_num - 0.5,
                x2 = pp_num + 0.5,
                y1 = ll  / 100 * nrow(ds1),
                y2 = ll  / 100 * nrow(ds1)
              )
            }

            ul <- interval_pp[["upp"]]
            if (!is.infinite(ul) && !is.na(ul)) {
              out[[length(out) + 1]] <- data.frame(
                x1 = pp_num - 0.5,
                x2 = pp_num + 0.5,
                y1 = ul  / 100 * nrow(ds1),
                y2 = ul  / 100 * nrow(ds1)
              )
            }

            # Return a single data frame or NULL if nothing to draw
            if (length(out) == 0) return(NULL)
            util_rbind(data_frames_list = out)
          }
        )

        # Remove NULL entries and combine into one data frame
        segments_list <- Filter(Negate(is.null), segments_list)
        all_segments  <- if (length(segments_list) > 0) {
          util_rbind(data_frames_list = segments_list)
        } else {
          NULL
        }

        # If we have any segment data, add them in one lean call
        if (!is.null(all_segments)) {
          p <- p %lean+% util_create_lean_ggplot(
            {
              # Add all segments in one geom_segment layer
              geom_segment(
                aes(
                  x    = x1,
                  xend = x2,
                  y    = y1,
                  yend = y2
                ),
                data     = all_segments,
                linetype = 2,
                col      = blue_red[1]
              )
            },
            all_segments = all_segments,
            aes          = ggplot2::aes,
            blue_red = blue_red,
            geom_segment = ggplot2::geom_segment
          )
        }
      }
    }

    fli <- util_coord_flip(p = p, ref_env = ref_env)
    is_flipped <- inherits(fli, "CoordFlip")

    p <- util_lazy_add_coord(p, fli) %lean+%
      # TODO: estimate w and h, if p is not using discrete axes
      ylab("") %lean+%
      theme_minimal() %lean+%
      theme(
        title = txtspec,
        axis.text.x = txtspec,
        axis.text.y = txtspec,
        axis.title.x = txtspec,
        axis.title.y = txtspec
      )

    # Adust the following, if changing the bar chart plots above:
    #    if (length(P$layers) == 1 &&
    #        inherits(P$layers[[1]]$geom, "GeomBar")) {
    # bar chart sizing_hints attributes, can also be attached to the
    # figures, alone
    #        .tb <- table(P$data)
    #       attr(P, "sizing_hints") <- list(
    #         figure_type_id = "bar_chart",
    #         rotated = is_flipped,
    #         number_of_bars = dim(.tb),
    #         min_bar_height = min(.tb),
    #         max_bar_height = max(.tb)
    #       )
    #   }

    util_set_size(p)
  })
  #TODO: get here the values for the attributes from the plot
  # Add sizing hints only if resp_vars is one
  if (length(resp_vars)==1) {
    #sizing information
    if (!is.null(plot_list[[resp_vars]])) {
      obj1 <- ggplot2::ggplot_build(plot_list[[resp_vars]])
      #in case of ecdf
      if (length(util_gg_get(obj1, "layout")$facet_params$.possible_columns) == 2) {
        min_bar_height <- 0
        max_bar_height <- 0
        range_bar <- 400 #an intermediate size plot in height
      } else {
        # in case of a single ditribution plot
        min_bar_height <- min(util_rbind(data_frames_list =
                                           util_gg_get(obj1, "data"))$ymax,
                              na.rm = TRUE)
        max_bar_height <- max(util_rbind(data_frames_list =
                                           util_gg_get(obj1, "data"))$ymax,
                              na.rm = TRUE)
        range_bar <- max_bar_height - min_bar_height

        if(min_bar_height == Inf ||
           min_bar_height == -Inf ||
           max_bar_height == Inf ||
           max_bar_height == -Inf ) {
          range_bar <- 400 #an intermediate size plot in height
        }
      }

      rotated <- unique(util_rbind(data_frames_list =
                                     util_gg_get(obj1, "data"))$flipped_aes)
      rotated <-  rotated[!is.na(rotated)]
      if (inherits(ds1[[resp_vars]], "POSIXct")) {
        number_of_bars <- 10
      } else {
        #    number_of_bars <- nrow(util_rbind(data_frames_list =
        # util_gg_get(obj1, "data")$data))
        #fix number in case of acc_distrib_prop o loc
        #    if (check_param == "proportion" || check_param == "location") {
        #      number_of_bars <- nrow(obj1$data[[1]])
        #    }
        number_of_bars <- nrow(util_gg_get(obj1, "data")[[1]])

        # fix number in case there are categories not listed in the obj1,
        # example OBS_BP_0 in ship example data
        if (is.factor(ds1[[resp_vars]])) {
          no_levels <- length(levels(ds1[[resp_vars]]))
          number_of_bars <- max(c(number_of_bars,no_levels))
        }
      }

      no_char_y <- max(nchar(obj1$data[[1]]$ymax))

      if(meta_data[meta_data[[label_col]] == resp_vars,
                   SCALE_LEVEL, drop = FALSE] %in% c("ratio", "interval")) {
        no_char_x <- 4
      } else {
        exp_bars <- levels(ds1[[resp_vars]])
        no_char_x <- max(nchar(exp_bars))
      }
      rm(obj1)
    }
    return(util_attach_attr(list(SummaryTable = res_out,
                                 SummaryData = res_view,
                                 SummaryPlotList = plot_list),
                            sizing_hints = list(figure_type_id = "bar_chart",
                                                rotated = rotated,
                                                number_of_bars = number_of_bars,
                                                range = range_bar,
                                                no_char_x = no_char_x,
                                                no_char_y = no_char_y),
                            as_plotly = "util_as_plotly_acc_distributions"))
  } else {
    return(util_attach_attr(list(SummaryTable = res_out,
                                 SummaryData = res_view,
                                 SummaryPlotList = plot_list),
                            as_plotly = "util_as_plotly_acc_distributions"))
  }


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
                                  label_col = VAR_NAMES,
                                  item_level = "item_level",
                                  check_param = "location",
                                  plot_ranges = TRUE,
                                  flip_mode = "noflip",
                                  meta_data = item_level,
                                  meta_data_v2) {
  util_maybe_load_meta_data_v2()
  rvs_meta <- util_prep_location_check(resp_vars = resp_vars,
                                       meta_data = meta_data,
                                       report_problems = "error",
                                       label_col = label_col)
  acc_distributions(
    resp_vars = resp_vars, study_data = study_data,
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
                                   label_col,
                                   item_level = "item_level",
                                   check_param = "proportion",
                                   plot_ranges = TRUE,
                                   flip_mode = "noflip",
                                   meta_data = item_level,
                                   meta_data_v2) {
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          #.apply_factor_metadata = TRUE, # can be omitted in favor of .apply_factor_metadata_inadm
                          .apply_factor_metadata_inadm = TRUE
                          )
  md_prep <- util_prep_proportion_check(
    resp_vars = resp_vars,
    meta_data = meta_data,
    ds1 = ds1,
    report_problems = "error",
    label_col = label_col
  )

  acc_distributions(
    resp_vars = resp_vars, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    check_param = "proportion", plot_ranges = plot_ranges,
    flip_mode = flip_mode
  )
}

#' Plots and checks for distributions -- only
#'
#' [Descriptor]
#'
#' @inherit acc_distributions
#' @export
#' @seealso
#' - [acc_distributions]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions_only <- function(resp_vars = NULL,
                                   study_data,
                                   label_col = VAR_NAMES,
                                   item_level = "item_level",
                                   flip_mode = "noflip",
                                   meta_data = item_level,
                                   meta_data_v2) {
  util_maybe_load_meta_data_v2()
  loc_avail <- !inherits(try(util_prep_location_check(resp_vars = resp_vars,
                                       meta_data = meta_data,
                                       report_problems = "error",
                                       label_col = label_col),
                   silent = TRUE), "try-error")
  if (.called_in_pipeline && loc_avail) {
    util_error("%s is already in a distribution location check plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE, # can be omitted in favor of .apply_factor_metadata_inadm
                          .apply_factor_metadata_inadm = TRUE)
  prop_avail <- !inherits(try(util_prep_proportion_check(
                    resp_vars = resp_vars,
                    meta_data = meta_data,
                    ds1 = ds1,
                    report_problems = "error",
                    label_col = label_col
                  ), silent = TRUE), "try-error")
  if (.called_in_pipeline && prop_avail) {
    util_error("%s is already in a distribution proportion check plot figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  scl_lvl <- meta_data[[SCALE_LEVEL]][meta_data[[label_col]] == resp_vars]
  if (.called_in_pipeline && scl_lvl %in% c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$ORDINAL)) {
    util_error("%s is already in a distribution plot (cat.) figure",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  acc_distributions(
    resp_vars = resp_vars, study_data = study_data,
    meta_data = meta_data, label_col = label_col,
    flip_mode = flip_mode
  )
}


#' @family plotly_shims
#' @concept plotly_shims
#' @noRd
util_as_plotly_acc_distributions <- function(res, ...) {
  if (length(res$SummaryPlotList) != 1) {
    return(util_ggplotly(ggplot2::ggplot() +
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

  py <- util_ggplotly(res$SummaryPlot, ...)
  plotly::layout(py, xaxis = list(tickangle = "auto"))
}
