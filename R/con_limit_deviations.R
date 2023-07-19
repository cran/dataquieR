#' Detects variable values exceeding limits defined in metadata
#'
#' @description
#'
#' Inadmissible numerical values can be of type integer or float. This
#' implementation requires the definition of intervals in the metadata to
#' examine the admissibility of numerical study data.
#'
#' This helps identify inadmissible measurements according to
#' hard limits (for multiple variables).
#'
#' @details
#' ### Algorithm of this implementation:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Interpretation of variable specific intervals as supplied in the metadata.
#'  - Identification of measurements outside defined limits. Therefore two
#'    output data frames are generated:
#'    - on the level of observation to flag each deviation, and
#'    - a summary table for each variable.
#'  - A list of plots is generated for each variable examined for limit
#'    deviations. The histogram-like plots indicate respective limits as well
#'    as deviations.
#'  - Values exceeding limits are removed in a data frame of modified study data
#'
#' For [con_detection_limits], The default for the limits argument differs and
#' is here "DETECTION_LIMITS"
#'
#' @export
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param limits [enum] HARD_LIMITS | SOFT_LIMITS | DETECTION_LIMITS. what
#'                                             limits from metadata to check for
#'
#' @inheritParams acc_distributions
#'
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual coord_flip labs
#'                     theme_minimal theme geom_bar geom_vline annotate
#' @importFrom stats setNames IQR complete.cases
#'
#' @return a list with:
#'   - `FlaggedStudyData` [data.frame] related to the study data by a 1:1
#'                                   relationship, i.e. for each observation is
#'                                   checked whether the value is below or above
#'                                   the limits.
#'   - `SummaryTable` [data.frame] summarizes limit deviations for each
#'                                 variable.
#'   - `SummaryPlotList` [list] of [ggplot]s The plots for each variable are
#'                            either a histogram (continuous) or a
#'                            barplot (discrete).
#'   - `ModifiedStudyData` [data.frame]  If the function identifies limit
#'                                     deviations, the respective values are
#'                                     removed in `ModifiedStudyData.`
#'   - `ReportSummaryTable`: heatmap-like data frame about limit violations
#'
#' @seealso
#' - [con_detection_limits]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
con_limit_deviations <- function(resp_vars = NULL, label_col, study_data,
                                 meta_data, limits = c(
                                   "HARD_LIMITS", "SOFT_LIMITS",
                                   "DETECTION_LIMITS"
                                 ),
                                 flip_mode = "flip") {

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes()

  # Which type of limits should be assessed?
  LIMITS <- toupper(match.arg(limits))
  infix <- unlist(strsplit(limits, "_"))[1]

  if (!(LIMITS %in% colnames(meta_data))) {
    util_error(paste0("The function con_limit_deviations was called to check ",
                      limits, ", but the metadata does not contain a column ",
                      "named ", LIMITS, "."),
               applicability_problem = TRUE)
  }

  # If resp_vars were not specified, all variables with metadata for the
  # selected type of limits will be considered (if any).
  # (backwards compatibility)
  if (length(resp_vars) == 0) {
    if (all(util_empty(meta_data[[LIMITS]]))) {
      util_error(paste0("No variables with defined ", LIMITS, "."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    } else {
      util_message(paste0("All variables with ", LIMITS,
                          " in the metadata are used."),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      resp_vars <- meta_data[[label_col]][!(util_empty(meta_data[[LIMITS]])) &
                                            meta_data$DATA_TYPE != "string"]
    }
  } else {
    # If resp_vars were specified, we have to check whether the required
    # metadata is available.
    if (all(util_empty(meta_data[[LIMITS]][meta_data[[label_col]] %in%
                                           resp_vars]))) {
      util_error(paste0("No variables with defined ", LIMITS, "."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }
    rvs_with_lim <- meta_data[[label_col]][!(util_empty(meta_data[[LIMITS]])) &
                                        meta_data[[label_col]] %in% resp_vars]
    if (length(rvs_with_lim) < length(unique(resp_vars))) {
      util_message(paste0("The variables ",
                          resp_vars[!(resp_vars %in% rvs_with_lim)],
                          " have no defined limits."),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
    }
    resp_vars <- rvs_with_lim
  }
  # If resp_vars were specified or selected automatically, we have to check
  # whether the given limits can be interpreted.
  md_lim <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                           target = LIMITS,
                                           meta_data = meta_data),
                     nm = resp_vars)
  md_lim_int <- lapply(md_lim, util_parse_interval)
  rvs_with_lim_err <- names(md_lim_int)[which(is.na(md_lim_int))]
  if (length(rvs_with_lim_err) > 0) {
    util_warning(paste0("The ", tolower(infix), " limits for ",
                        paste(rvs_with_lim_err, collapse = ", "),
                        " can not be interpreted as interval."),
                 applicability_problem = TRUE)
    resp_vars <- setdiff(resp_vars, rvs_with_lim_err)
  }

  # Stop if no resp_vars are left after these checks.
  if (length(resp_vars) == 0) {
    util_error("No variables left, no limit checks possible.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_null = TRUE,
                            allow_any_obs_na = TRUE,
                            allow_all_obs_na = FALSE,
                            need_type = "integer|float|datetime")

  # If resp_vars contains variables of type 'datetime', they have to be
  # transformed to POSIXct. First, we identify these variables:
  # TODO: Should this be done within prep_prepare_dataframes?
  datetime_vars <-
    vapply(resp_vars,
           function(rv) {
             meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] ==
               DATA_TYPES$DATETIME
           },
           FUN.VALUE = logical(1))
  # If 'datetime' variables are given as numeric, we have to specify an origin.
  # Otherwise (Date, character, ...), 'origin' will be ignored by as.POSIXct.
  ds1[, resp_vars[datetime_vars]] <-
    lapply(ds1[, resp_vars[datetime_vars], drop = FALSE],
           as.POSIXct,
           origin = min(as.POSIXct(Sys.Date()), 0))

  # compare values with limit intervals ----------------------------------------
  sd_lim <-
    mapply(ds1[, resp_vars, drop = FALSE],
           md_lim_int,
           SIMPLIFY = FALSE,
           FUN = function(col_rv, int) {
             col_out <- rep(NA, length(col_rv))
             if (inherits(int, "interval")) {
               if (int$inc_l) {
                 below <- col_rv < int$low
               } else {
                 below <- col_rv <= int$low
               }
               within <- redcap_env$`in`(col_rv, int)
               if (int$inc_u) {
                 above <- col_rv > int$upp
               } else {
                 above <- col_rv >= int$upp
               }
               col_out[which(below)] <- "below"
               col_out[which(within)] <- "within"
               col_out[which(above)] <- "above"
               if (!identical(which(is.na(col_out)), which(is.na(col_rv)))) {
                 util_warning("Limit evaluation resulted in new NAs.")
               }
             }
             factor(col_out, levels = c("below", "within", "above"))
          })
  # sd_lim is a named list which contains for each variable a vector with
  # indicators "above", "within", "below" or NA for each observation (i.e.,
  # vector of length `nrow(ds1)`)

  # get frequency counts of values below, within and above limits
  freq_lim_viol <-
    lapply(setNames(nm = resp_vars),
           function(rv) {
             as.list(table(sd_lim[[rv]]))
           })
  # freq_lim_viol is a list with each element being a list with entries named
  # "below", "within" and "above" reporting the respective frequency counts for
  # each of the variables

  # reference environment for utility function to switch coordinates
  ref_env <- environment()

  # plot -----------------------------------------------------------------------
  plot_list <- lapply(setNames(nm = resp_vars), function(rv) {
    # get limits and range of data values for variable `rv`
    # lower limit
    ll <- md_lim_int[[rv]][["low"]]
    ll <- ifelse(is.infinite(ll), NA, ll)
    # The check above can switch ll from POSIXct to numeric, undo:
    if (datetime_vars[[rv]] & is.numeric(ll)) {
      ll <- as.POSIXct(ll, origin = min(as.POSIXct(Sys.Date()), 0))
    }
    # upper limit
    ul <- md_lim_int[[rv]][["upp"]]
    ul <- ifelse(is.infinite(ul), NA, ul)
    if (datetime_vars[[rv]] & is.numeric(ul)) {
      ul <- as.POSIXct(ul, origin = min(as.POSIXct(Sys.Date()), 0))
    }
    # range of data values with and without limits
    max_data <- max(ds1[[rv]], na.rm = TRUE)
    min_data <- min(ds1[[rv]], na.rm = TRUE)
    max_plot <- max(c(max_data, ul), na.rm = TRUE)
    min_plot <- min(c(min_data, ll), na.rm = TRUE)

    # set up data for plotting, omit NAs to prevent warnings
    rv_data <- data.frame("values" = ds1[[rv]],
                          "section" = sd_lim[[rv]])
    rv_data <- rv_data[complete.cases(rv_data), ]

    # Should the plot be a histogram? If not, it will be a bar chart.
    # (similar to acc_distributions)
    # A histogram will be plotted for datetime variables and continuous
    # variables, including numeric non-integer variables and integer variables
    # with more than 30 different values (without NAs, and only if there are no
    # value labels given).
    if ("VALUE_LABELS" %in% colnames(meta_data)) {
      plot_histogram <- datetime_vars[[rv]] ||
        !all(util_is_integer(ds1[[rv]])) ||
        (length(unique(rv_data$values)) > 30 &&
           util_empty(meta_data$VALUE_LABELS[which(meta_data[[label_col]] == rv)]))
    } else {
      plot_histogram <- datetime_vars[[rv]] ||
        !all(util_is_integer(ds1[[rv]])) ||
        length(unique(rv_data$values)) > 30
    }

    txtspec <- element_text(
      colour = "black", # size = 16,
      hjust = .5, vjust = .5, face = "plain"
    ) # angle = 0,

    # define plot colors for the different sections
    out_cols <- c(
      above = "#B2182B",
      within = "#2166AC",
      below = "#B2182B"
    )


    if (plot_histogram) {
      # histogram: compute bin breaks ------------------------------------------
      # calculate bandwidth according to Freedman-Diaconis based on the
      # data within limits, but limit the total number of bins to avoid
      # rendering problems and to obtain a readable figure
      rv_within <- rv_data$values[which(rv_data$section == "within")]
      min_within <- min(c(rv_within, ll), na.rm = TRUE)
      max_within <- max(c(rv_within, ul), na.rm = TRUE)

      bin_breaks <- util_optimize_histogram_bins(
        x = rv_data$values,
        iqr_bw = IQR(rv_within),
        n_bw = length(rv_within),
        min_within = min_within,
        max_within = max_within,
        min_plot = min_plot,
        max_plot = max_plot,
        nbins_max = 100)

      # histogram: create plot -------------------------------------------------
      # prepare limit lines
      if (is.na(ll)) { # line at minimum value
        ll_line <- geom_vline(xintercept = min_data,
                              color = "#999999",
                              alpha = 1,
                              linetype = "dotted")
      } else {
        ll_line <- geom_vline(xintercept = ll,
                              color = "#B2182B",
                              alpha = 1)
      }
      if (is.na(ul)) { # line at maximum value
        ul_line <- geom_vline(xintercept = max_data,
                              color = "#999999",
                              alpha = 1,
                              linetype = "dotted")
      } else {
        ul_line <- geom_vline(xintercept = ul,
                              color = "#B2182B",
                              alpha = 1)
      }

      # define limits for plotting
      if (!datetime_vars[[rv]]) {
        myxlim <- c(floor(min_plot), ceiling(max_plot))
      } else {
        myxlim <- c(min_plot, max_plot)
        #myxlim <- as.POSIXct(myxlim, origin = min(as.POSIXct(Sys.Date()), 0))
      }

      p <- ggplot(data = rv_data, aes(x = .data[["values"]],
                                      fill = .data[["section"]])) +
        # plot data below limits
        geom_histogram(
          data = subset(rv_data, get("section") == "below"),
          breaks = bin_breaks$below
        ) +
        # plot data within limits
        geom_histogram(
          data = subset(rv_data, get("section") == "within"),
          breaks = bin_breaks$within
        ) +
        # plot data above limits
        geom_histogram(
          data = subset(rv_data, get("section") == "above"),
          breaks = bin_breaks$above
        ) +
        # add line for lower limit
        ll_line +
        # add line for upper limit
        ul_line +
        util_coord_flip(ref_env = ref_env, xlim = myxlim) +
        # TODO: estimate w and h, if p is not using discrete axes
        scale_fill_manual(values = out_cols,
                          breaks = names(out_cols),
                          labels = paste(names(out_cols), # TODO: labels do not work with plotly
                                         tolower(infix),
                                         "limits:",
                                         freq_lim_viol[[rv]][names(out_cols)]),
                          name = "Number of values") +
        labs(x = "", y = paste0(rv)) +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec
        )
    } else {
      # bar chart --------------------------------------------------------------
      # prepare limit lines (move by 0.5 to prevent overlap)
      if (is.na(ll)) { # line at minimum value
        ll_line <- geom_vline(xintercept = min_data - 0.5,
                              color = "#999999",
                              alpha = 1,
                              linetype = "dotted")
      } else {
        ll_line <- geom_vline(xintercept = ll - 0.5,
                              color = "#B2182B",
                              alpha = 1)
      }
      if (is.na(ul)) { # line at maximum value
        ul_line <- geom_vline(xintercept = max_data + 0.5,
                              color = "#999999",
                              alpha = 1,
                              linetype = "dotted")
      } else {
        ul_line <- geom_vline(xintercept = ul + 0.5,
                              color = "#B2182B",
                              alpha = 1)
      }
      # include space for the moved limits (or wide bars) to the plotting range
      max_plot <- max_plot + 0.5
      min_plot <- min_plot - 0.5

      p <- ggplot(data = rv_data, aes(x = .data[["values"]],
                                      fill = .data[["section"]])) +
        geom_bar() +
        # add line for lower limit
        ll_line +
        # add line for upper limit
        ul_line +
        util_coord_flip(ref_env = ref_env,
                        xlim = c(min_plot,
                                 max_plot)) +
        # TODO: estimate w and h, if p is not using discrete axes
        scale_fill_manual(values = out_cols,
                          breaks = names(out_cols),
                          labels = paste(names(out_cols),
                                         tolower(infix),
                                         "limits:",
                                         freq_lim_viol[[rv]][names(out_cols)]),
                          name = "Number of values") +
        labs(x = "", y = paste0(rv)) +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec
        )
    }

    suppressWarnings({
      # NA observations can be removed, but should not be checked twice, see
      # above.
      # https://stackoverflow.com/a/51795017
      bp <- ggplot_build(p)
      w <- 2 * length(bp$layout$panel_params[[1]]$x$get_labels())
      if (w == 0) {
        w <- 10
      }
      w <- w + 10 +
        max(nchar(bp$layout$panel_params[[1]]$y$get_labels()),
            na.rm = TRUE
        )
      h <- 2 * length(bp$layout$panel_params[[1]]$y$get_labels())
      if (h == 0) {
        h <- 10
      }
      h <- h + 20

      p <- util_set_size(p, width_em = w, height_em = h)
    })

    return(p)
  }) # end lapply plot_list

  # flagged study data ---------------------------------------------------------
  fsd_list <- lapply(setNames(nm = resp_vars), function(rv) {
    fsd <- ds1[, rv, drop = FALSE]
    col_below <- paste0(rv, "_below_", tolower(infix))
    col_above <- paste0(rv, "_above_", tolower(infix))
    fsd[[col_below]] <- ifelse(as.character(sd_lim[[rv]]) == "below", 1, 0)
    fsd[[col_above]] <- ifelse(as.character(sd_lim[[rv]]) == "above", 1, 0)
    return(fsd)
  })

  fsd <- cbind(ds1[, setdiff(colnames(ds1), resp_vars), drop = FALSE],
               do.call(cbind.data.frame,
                 c(unname(fsd_list), list(stringsAsFactors = FALSE))))

  # modified study data --------------------------------------------------------
  msdf <- ds1
  # remove violations of value limits
  for (rv in resp_vars) { # TODO: Will we still compute this? we have now prep_prepare_dataframes for this.
    to_remove <- which(as.character(sd_lim[[rv]]) %in% c("below", "above"))
    if (length(to_remove) > 0) {
      msdf[[rv]][to_remove] <- NA
      if (!.called_in_pipeline) util_message(paste0(
        "N = ", length(to_remove), " values in ", rv,
        " lie outside ", tolower(infix), " limits and were removed."
      ),
      applicability_problem = FALSE
      )
    }
  }

  # summary table --------------------------------------------------------------
  sumtab <- lapply(setNames(nm = resp_vars), function(rv) {
    df_out <- list(
      rv,
      ifelse(datetime_vars[[rv]], NA,
             freq_lim_viol[[rv]][["below"]] + freq_lim_viol[[rv]][["above"]]),
      ifelse(datetime_vars[[rv]], NA,
             round((freq_lim_viol[[rv]][["below"]] +
                      freq_lim_viol[[rv]][["above"]]) / nrow(ds1) * 100,
                   digits = 2)),
      ifelse(datetime_vars[[rv]], NA,
             ifelse(freq_lim_viol[[rv]][["below"]] +
                      freq_lim_viol[[rv]][["above"]] > 0, TRUE, FALSE)),
      ifelse(datetime_vars[[rv]],
             freq_lim_viol[[rv]][["below"]] + freq_lim_viol[[rv]][["above"]],
             NA),
      ifelse(datetime_vars[[rv]],
             round((freq_lim_viol[[rv]][["below"]] +
                      freq_lim_viol[[rv]][["above"]]) / nrow(ds1) * 100,
                   digits = 2),
             NA),
      ifelse(datetime_vars[[rv]],
             ifelse(freq_lim_viol[[rv]][["below"]] +
                      freq_lim_viol[[rv]][["above"]] > 0, TRUE, FALSE),
             NA)
    )
    df_out <- as.data.frame(df_out, stringsAsFactors = FALSE)
    if (LIMITS == "SOFT_LIMITS") {
      colnames(df_out) <- c("Variables",
                            "NUM_con_rvv_unum",
                            "PCT_con_rvv_unum",
                            "FLG_con_rvv_unum",
                            "NUM_con_rvv_utdat",
                            "PCT_con_rvv_utdat",
                            "FLG_con_rvv_utdat")
      # GRADING for backwards compatibility
      df_out$GRADING <- as.numeric(df_out$FLG_con_rvv_unum |
                                     df_out$FLG_con_rvv_utdat)
    } else {
      colnames(df_out) <- c("Variables",
                            "NUM_con_rvv_inum",
                            "PCT_con_rvv_inum",
                            "FLG_con_rvv_inum",
                            "NUM_con_rvv_itdat",
                            "PCT_con_rvv_itdat",
                            "FLG_con_rvv_itdat")
      # GRADING for backwards compatibility
      df_out$GRADING <- as.numeric(df_out$FLG_con_rvv_inum |
                                     df_out$FLG_con_rvv_itdat)
    }
    df_out$GRADING <- ifelse(is.na(df_out$GRADING), 0, df_out$GRADING)
    return(df_out)
  })
  sumtab <- do.call(rbind.data.frame,
                    c(sumtab,
                      stringsAsFactors = FALSE, deparse.level = 0,
                      make.row.names = FALSE))

  # summary data ---------------------------------------------------------------
  sumdat <- lapply(setNames(nm = resp_vars), function(rv) {
    df_out <- list(
      rv,
      freq_lim_viol[[rv]][["below"]],
      round(freq_lim_viol[[rv]][["below"]] / nrow(ds1) * 100,
            digits = 2),
      freq_lim_viol[[rv]][["above"]],
      round(freq_lim_viol[[rv]][["above"]] / nrow(ds1) * 100,
            digits = 2),
      ifelse(freq_lim_viol[[rv]][["below"]] + freq_lim_viol[[rv]][["above"]] > 0,
             1, 0)
    )
   df_out <- as.data.frame(df_out, stringsAsFactors = FALSE)
   colnames(df_out) <- c("Variables",
                         paste0("Below ", tolower(infix), " limits (N)"),
                         paste0("Below ", tolower(infix), " limits (%)"),
                         paste0("Above ", tolower(infix), " limits (N)"),
                         paste0("Above ", tolower(infix), " limits (%)"),
                         "GRADING")
   return(df_out)
  })
  sumdat <- do.call(rbind.data.frame,
                    c(sumdat,
                      stringsAsFactors = FALSE, deparse.level = 0,
                      make.row.names = FALSE))

  # report summary table -------------------------------------------------------
  heatmap_tab <- sumdat[, c(1, which(grepl("(N)", colnames(sumdat),
                                           fixed = TRUE)))]
  colnames(heatmap_tab) <- gsub(" (N)", "", colnames(heatmap_tab),
                                fixed = TRUE)
  heatmap_tab$N <- vapply(resp_vars, function(rv) sum(!(is.na(fsd[[rv]]))),
                          FUN.VALUE = integer(1))
  class(heatmap_tab) <- union("ReportSummaryTable", class(heatmap_tab))

  return(util_attach_attr(list(
    FlaggedStudyData = fsd,
    SummaryTable = sumtab,
    SummaryData = sumdat,
    ReportSummaryTable = heatmap_tab,
    SummaryPlotList = plot_list,
    ModifiedStudyData = msdf
  ), as_plotly = "util_as_plotly_con_limit_deviations"))
}

#' Detects variable values exceeding detection limits
#' @inherit con_limit_deviations
#' @export
#' @seealso
#' - [con_limit_deviations]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
con_detection_limits <- function(resp_vars = NULL, label_col, study_data,
                                 meta_data, limits = c(
                                   "DETECTION_LIMITS",
                                   "HARD_LIMITS", "SOFT_LIMITS"
                                 ),
                                 flip_mode = "flip") {
  con_limit_deviations(
    resp_vars = resp_vars, label_col = label_col, study_data = study_data,
    meta_data = meta_data, limits = match.arg(limits), flip_mode = flip_mode
  )
}

#' Detects variable values exceeding soft limits
#' @inherit con_limit_deviations
#' @export
#' @seealso
#' - [con_limit_deviations]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
con_soft_limits <- function(resp_vars = NULL, label_col, study_data,
                                 meta_data, limits = c(
                                   "SOFT_LIMITS",
                                   "DETECTION_LIMITS",
                                   "HARD_LIMITS"
                                 ),
                            flip_mode = "flip") {
  con_limit_deviations(
    resp_vars = resp_vars, label_col = label_col, study_data = study_data,
    meta_data = meta_data, limits = match.arg(limits), flip_mode = flip_mode
  )
}

#' Detects variable values exceeding hard limits
#' @inherit con_limit_deviations
#' @export
#' @seealso
#' - [con_limit_deviations]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
con_hard_limits <- function(resp_vars = NULL, label_col, study_data,
                            meta_data, limits = c(
                              "HARD_LIMITS",
                              "SOFT_LIMITS",
                              "DETECTION_LIMITS"
                            ),
                            flip_mode = "flip") {
  con_limit_deviations(
    resp_vars = resp_vars, label_col = label_col, study_data = study_data,
    meta_data = meta_data, limits = match.arg(limits), flip_mode = flip_mode
  )
}


util_as_plotly_con_limit_deviations <- function(res, ...) {
  res$SummaryPlot <- util_remove_dataquieR_result_class(res$SummaryPlot)
  util_ensure_suggested("plotly")
  p <- res$SummaryPlotList
  util_stop_if_not(length(p) == 1)
  p <- p[[1]]
  lb <- ggplot_build(p)$plot$scales$scales[[1]]$get_labels(1)
  cl <- ggplot_build(p)$plot$scales$scales[[1]]$palette(1)
  br <- ggplot_build(p)$plot$scales$scales[[1]]$breaks
  py <- plotly::ggplotly(p)
  .lbs <- setNames(lb, nm = br)
  for (i in seq_along(py$x$data)) {
    nm <- py$x$data[[i]]$name
    if (nm %in% names(.lbs)) {
      py$x$data[[i]]$name <- .lbs[[nm]]
    }
  }
  py <- plotly::layout(py, legend = list(traceorder = "reversed"))
  # for (i in seq_len(length(lb))) {
  #   lb <- ggplot_build(p)$plot$scales$scales[[1]]$get_labels(i)
  #   cl <- ggplot_build(p)$plot$scales$scales[[1]]$palette(i)
  #   # suppressWarnings(py <- plotly::style(py,
  #   #                                       # marker = list(
  #   #                                       #   color = cl[[br[[i]]]]
  #   #                                       # ),
  #   #                                      name = lb[[i]],
  #   #                                      traces = i))
  # }
  py
}
