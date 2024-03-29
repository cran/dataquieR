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
#' [Indicator]
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
#' @param return_flagged_study_data [logical] return `FlaggedStudyData` in the
#'                                            result
#'
#' @inheritParams acc_distributions
#'
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual coord_flip labs
#'                     theme_minimal theme geom_bar geom_vline annotate
#'                     scale_linetype_manual
#' @importFrom stats setNames IQR complete.cases
#' @importFrom grDevices colorRampPalette gray.colors
#'
#' @return a list with:
#'   - `FlaggedStudyData` [data.frame] related to the study data by a 1:1
#'                                   relationship, i.e. for each observation is
#'                                   checked whether the value is below or above
#'                                   the limits. Optional, see
#'                                   `return_flagged_study_data`.
#'   - `SummaryTable` [data.frame] summarizes limit deviations for each
#'                                 variable.
#'   - `SummaryPlotList` [list] of [ggplot]s The plots for each variable are
#'                            either a histogram (continuous) or a
#'                            barplot (discrete).
#'   - `ReportSummaryTable`: heatmap-like data frame about limit violations
#'
#' @seealso
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
con_limit_deviations <- function(resp_vars = NULL, label_col, study_data,
                                 meta_data, limits = NULL,
                                 flip_mode = "noflip",
                                 return_flagged_study_data = FALSE) {
  # make R CMD check happy
  Limits <- NULL
  Number <- NULL
  Section <- NULL
  all_of <- NULL
  n  <- NULL
  n_viol <- NULL

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes()

  util_expect_scalar(return_flagged_study_data, check_type = is.logical)

  # Which limits should be assessed?
  if (!is.null(limits) && !(all(limits %in% colnames(meta_data)))) {
    limits_miss <- limits[!(limits %in% colnames(meta_data))]
    util_warning(paste0("The limit deviation check cannot be performed for ",
                        paste(dQuote(limits_miss), collapse = ", "),
                        ". There is no matching column in the metadata."),
                 applicability_problem = TRUE)
  }
  known_limits <- unlist(
    WELL_KNOWN_META_VARIABLE_NAMES[intersect(
      grep("LIMIT", WELL_KNOWN_META_VARIABLE_NAMES),
      which(vapply(WELL_KNOWN_META_VARIABLE_NAMES,
                   function(ll) {
                     attr(ll, "var_att_required") != "technical"
                   },
                   FUN.VALUE = logical(1)))
    )]
  )
  # Which limit columns are given in the metadata?
  given_limits <- colnames(meta_data)[which(colnames(meta_data) %in%
                                              c(known_limits, limits))]
  LIMITS <- given_limits # shorter notation, and 'given_limits' could be
  # used to define the color scheme

  if (length(LIMITS) > 0) {
    # Empty metadata columns can be ignored.
    empty_limit_column <- apply(meta_data[, LIMITS, drop = FALSE], 2,
                                function(cc) { all(util_empty(cc)) })
    if (any(empty_limit_column)) {
      LIMITS <- names(empty_limit_column)[!empty_limit_column]
    }
  }

  if (length(LIMITS) == 0) {
    util_message(paste0("Hint: If your metadata contains a column with limits ",
                        "for which the limit deviation check should be ",
                        "performed, try to pass the name of the column ",
                        "to the ", sQuote("limits"), " argument."))
    util_error(paste0("The limit deviation check cannot be performed without ",
                      "a metadata column specifying suitable limits."),
               applicability_problem = TRUE)
  }

  if (length(resp_vars) == 0) {
    # If resp_vars were not specified, all variables with limits given in the
    # metadata will be considered (if any).
    # Which metadata rows contain some information in the limits columns?
    any_limits <- apply(meta_data[, LIMITS, drop = FALSE], 1,
                        function(rr) { !all(util_empty(rr)) })
    if (all(!any_limits)) {
      util_error("No variables with defined limits.",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    } else {
      util_message(paste0("All variables for which limits are specified ",
                          "in the metadata are used."),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      resp_vars <- meta_data[[label_col]][any_limits &
                                            meta_data$DATA_TYPE != "string"]
      if (length(resp_vars) > 0) {
        # Exclude variables that contain only NA entries in the study data.
        rvs_all_na <- apply(ds1[, resp_vars, drop = FALSE], 2,
                            function(cc) { all(is.na(cc)) })
        if (any(rvs_all_na)) {
          resp_vars <- names(rvs_all_na)[!rvs_all_na]
        }
      }
    }
  } else {
    util_correct_variable_use("resp_vars",
                              allow_more_than_one = TRUE,
                              allow_all_obs_na = FALSE,
                              need_type = "integer|float|datetime")
    # If resp_vars were specified, we have to check whether the required
    # metadata is available.
    any_limits <- vapply(resp_vars, FUN.VALUE = logical(1), function(rvs) {
      !all(util_empty(meta_data[meta_data[[label_col]] == rvs,
                                LIMITS, drop = FALSE]))
    })
    if (all(!any_limits)) {
      util_error("No limits specified.",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    } else {
      rvs_with_lim <- names(any_limits)[any_limits]
      if (length(rvs_with_lim) < length(unique(resp_vars))) {
        util_message(paste0("No limits specified for ",
                            paste(resp_vars[!(resp_vars %in% rvs_with_lim)],
                                  collapse = ", "),
                            "."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
      }
      resp_vars <- rvs_with_lim
    }
  }

  # In both cases ('resp_vars' were specified by the user or
  # selected automatically), we have to check whether the given limits can
  # be interpreted as intervals.
  md_lim_int <- setNames(
    nm = resp_vars,
    lapply(resp_vars, function(rvs) {
      lim <- meta_data[meta_data[[label_col]] == rvs, LIMITS, drop = FALSE]
      lim_int <- lapply(lim, util_parse_interval)
      is_interval <- vapply(lim_int, FUN.VALUE = logical(1),
                            function(int) { inherits(int, "interval") })
      wrong_limits <- !is_interval != as.vector(is.na(lim))
      if (any(wrong_limits)) {
        util_warning(
          paste0("The limits given in",
                 paste(sQuote(names(lim)[wrong_limits]), collapse = ", "),
                 " for ", sQuote(rvs), " cannot be interpreted as interval."),
          applicability_problem = TRUE)
      }
      if (all(!is_interval)) {
        return(NA)
      } else {
        return(lim_int[is_interval])
      }
    }))

  # Variables that do not have any interpretable intervals can be dropped.
  drop_rvs <- is.na(md_lim_int)
  if (all(drop_rvs)) {
    util_error("No feasible check for limit deviations.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  if (any(drop_rvs)) {
    resp_vars <- setdiff(resp_vars, names(md_lim_int)[drop_rvs])
  }

  # Which variables are of type 'datetime'?
  is_datetime_var <- vapply(resp_vars, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] ==
      DATA_TYPES$DATETIME
  }, FUN.VALUE = logical(1))

  # We will need the interval notation later also for the segments below and
  # above limits, not only within:
  md_lim_all_segments <- setNames(
    nm = resp_vars,
    lapply(resp_vars, function(rv) {
      lapply(md_lim_int[[rv]], function(int) {
        ll <- int
        ll$upp <- int$low
        ll$inc_u <- !int$inc_l
        ll$low <- as.numeric(-Inf)
        ll$inc_l <- FALSE
        ul <- int
        ul$upp <- as.numeric(Inf)
        ul$inc_u <- FALSE
        ul$low <- int$upp
        ul$inc_l <- !int$inc_u
        return(list(
          "below" = ll,
          "within" = int,
          "above" = ul
        ))
      })
    }))

  # compare values with limit intervals ----------------------------------------
  sd_lim <- setNames(
    nm = resp_vars,
    lapply(resp_vars, function(rvs) {
      col_rv <- ds1[, rvs]
      lim <- md_lim_int[[rvs]]
      col_int <- lapply(lim, function(int) {
        col_out <- rep(NA, length(col_rv))
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
        return(factor(col_out, levels = c("below", "within", "above")))
      })
      return(col_int)
    })
  )

  # If there is a HARD_LIMITS check, the remaining limit deviations should
  # only be checked after excluding the values that violate the HARD_LIMITS.
  sd_lim_HL_check <- setNames(
    nm = resp_vars,
    lapply(resp_vars, function(rvs) {
      sd_lim_rvs <- sd_lim[[rvs]]
      if ("HARD_LIMITS" %in% names(sd_lim_rvs) && length(sd_lim_rvs) > 1) {
        hl_viol_ind <- which(sd_lim_rvs[["HARD_LIMITS"]] %in%
                               c("below", "above"))
        if (length(hl_viol_ind) > 0) {
          other_lim <- setdiff(names(sd_lim_rvs), "HARD_LIMITS")
          sd_lim_rvs[other_lim] <- lapply(other_lim, function(ol) {
            sd_lim_rvs[[ol]][hl_viol_ind] <- NA
            return(sd_lim_rvs[[ol]])
          })
        }
      }
      return(sd_lim_rvs)
    })
  )

  sd_n_obs <- setNames(
    nm = resp_vars,
    lapply(resp_vars, function(rvs) {
      lapply(sd_lim_HL_check[[rvs]], function(cc) {
        sum(!is.na(cc))
      })
    })
  )

  # get frequency counts of values below, within and above limits
  freq_lim_viol <- setNames(
    nm = resp_vars,
    lapply(resp_vars, function(rvs) {
      sd_lim_rvs <- sd_lim_HL_check[[rvs]]
      tab_rvs <- lapply(sd_lim_rvs, function(res_int) {
        t1 <- table(res_int)
        setNames(nm = names(t1), as.vector(t1))
      })
      t(as.data.frame(tab_rvs))
    }))

  # reference environment for utility function to switch coordinates
  ref_env <- environment()

  # plot -----------------------------------------------------------------------
  # color scheme for bars within or outside of limits
  base_col <-
    colorRampPalette(
      colors = c("#2166AC","#fdd49e","#fc8d59","#d7301f","#B2182B","#7f0000"))(
                         length(given_limits) + 1)
  # line types for limits (apart from HARD_LIMITS)
  base_lty <- c(2, 4:6)
  # text design
  spec_txt <- element_text(
    colour = "black",
    hjust = .5, vjust = .5, face = "plain"
  )
  # fixed design for lines indicating hard limits
  spec_lines <- data.frame(
    limits = "HARD_LIMITS",
    color = "#B2182B",
    lty = 1)
  # add designs for other limits as needed
  given_lim_wo_hard <- setdiff(given_limits, "HARD_LIMITS")
  n_spec_lim <- length(given_lim_wo_hard)
  if (n_spec_lim > 0) {
    spec_lines <- rbind(
      spec_lines,
      data.frame(
        limits = given_lim_wo_hard,
        color = rep(gray.colors(n = ceiling(n_spec_lim/length(base_lty))),
                    each = length(base_lty))[1:n_spec_lim],
        lty = c(rep(base_lty, floor(n_spec_lim/length(base_lty))),
                base_lty[seq_len(n_spec_lim %% length(base_lty))])))
  }

  # create a plot for each 'resp_var'
  plot_list <- lapply(setNames(nm = resp_vars), function(rv) {
    # limits for variable `rv`
    lower_limits <- do.call(c, lapply(md_lim_int[[rv]], '[[', "low"))
    if (any(is.infinite(lower_limits))) {
      lower_limits[is.infinite(lower_limits)] <- NA
    }
    upper_limits <- do.call(c, lapply(md_lim_int[[rv]], '[[', "upp"))
    if (any(is.infinite(upper_limits))) {
      upper_limits[is.infinite(upper_limits)] <- NA
    }
    rv_limit_names <- names(md_lim_int[[rv]])

    # range of data values
    max_data <- max(ds1[[rv]], na.rm = TRUE)
    min_data <- min(ds1[[rv]], na.rm = TRUE)
    # range of values to be included in the plot
    max_plot <- max(c(max_data, lower_limits, upper_limits), na.rm = TRUE)
    min_plot <- min(c(min_data, lower_limits, upper_limits), na.rm = TRUE)

    # set up data for plotting, omit NAs to prevent warnings
    rv_data <- data.frame("values" = ds1[[rv]],
                          sd_lim_HL_check[[rv]])
    rv_data <- rv_data[complete.cases(rv_data[, "values"]), ]

    rv_data$viol_n_limits <- apply(rv_data[, rv_limit_names, drop = FALSE], 1,
                                   function(rr) {
                                     length(which(rr != "within"))
                                   })
    rv_data$limit_violations <- paste0("detected (", rv_data$viol_n_limits, ")")
    rv_data$limit_violations[rv_data$viol_n_limits == 0] <- "none"
    rv_data$fill <- vapply(rv_data$viol_n_limits,
                           FUN.VALUE = "character(1)",
                           FUN = function(vv) { base_col[vv + 1] })
    if ("HARD_LIMITS" %in% colnames(rv_data)) {
      rv_data$limit_violations[rv_data$HARD_LIMITS != "within"] <- "severe"
      rv_data$fill[rv_data$HARD_LIMITS != "within"] <- rev(base_col)[1]
    }
    spec_bars <- unique(rv_data[, c(rv_limit_names,
                                    "limit_violations",
                                    "fill")])

    # Should the plot be a histogram? If not, it will be a bar chart.
    plot_histogram <-
      meta_data[[SCALE_LEVEL]][which(meta_data[[label_col]] == rv)] %in%
      c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$RATIO)

    values <- NULL # to make r cmd check happy

    if (plot_histogram) {
      # histogram: preparation -------------------------------------------------
      # The plot will be created in segments defined by limit breaks. For this,
      # we have to define these segments and their boundaries.
      plot_segments <- rv_data %>%
        dplyr::arrange(values) %>%
        dplyr::select(all_of(rv_limit_names)) %>%
        dplyr::group_by_all(.) %>%
        dplyr::count() %>%
        as.data.frame(., drop = FALSE)

      plot_segments_intervals <-
        apply(plot_segments[, rv_limit_names, drop = FALSE], 1, function(rr) {
          rr_int <- lapply(seq_along(rr), function(ii) {
            md_lim_all_segments[[rv]][[rv_limit_names[ii]]][[rr[ii]]]
          })
          out_int <- rr_int[[1]] # initialize interval for output
          # Which interval has the highest lower interval boundary?
          rr_low <- do.call(c, lapply(rr_int, '[[', "low"))
          rr_low_max <- max(rr_low, na.rm = TRUE)
          out_int$low <- rr_low_max
          # There may be several intervals with the same value. We have to check
          # if the lower limit shall be included or not.
          rr_low_which_max <- which(rr_low == rr_low_max)
          if (length(rr_low_which_max) > 1) {
            inc_l <- do.call(c,
                             lapply(rr_int, '[[', "inc_l"))[rr_low_which_max]
            if (any(!inc_l)) {
              out_int$inc_l <- FALSE
            } else {
              out_int$inc_l <- TRUE
            }
          } else {
            out_int$inc_l <- rr_int[[rr_low_which_max]][["inc_l"]]
          }
          # Which interval has the lowest upper interval boundary?
          rr_upp <- do.call(c, lapply(rr_int, '[[', "upp"))
          rr_upp_min <- min(rr_upp, na.rm = TRUE)
          out_int$upp <- rr_upp_min
          # There may be several intervals with the same value. We have to check
          # if the upper limit shall be included or not.
          rr_upp_which_min <- which(rr_upp == rr_upp_min)
          if (length(rr_upp_which_min) > 1) {
            inc_u <- do.call(c,
                             lapply(rr_int, '[[', "inc_u"))[rr_upp_which_min]
            if (any(!inc_u)) {
              out_int$inc_u <- FALSE
            } else {
              out_int$inc_u <- TRUE
            }
          } else {
            out_int$inc_u <- rr_int[[rr_upp_which_min]][["inc_u"]]
          }
          return(out_int)
        })

      # calculate bin breaks so that bins end at segment borders
      # The optimal bandwidth will be computed according to Freedman-Diaconis
      # based on the data in the segment with most of the observations.
      # However, we limit the total number of bins to avoid rendering problems
      # and to obtain a readable figure.
      largest_segment <- plot_segments_intervals[[which.max(plot_segments$n)]]

      bin_breaks <- util_optimize_histogram_bins(
        x = rv_data$values,
        interval_freedman_diaconis = largest_segment,
        cuts = sort(as.numeric(unique(c(
          do.call(c, lapply(plot_segments_intervals, '[[', "low")),
          do.call(c, lapply(plot_segments_intervals, '[[', "upp")))))),
        nbins_max = 100)

      if (length(plot_segments_intervals) != length(bin_breaks)) {
        # discard sections without data
        bin_segments_with_data <-
          vapply(bin_breaks, FUN.VALUE = logical(1), function(bb) {
            # check that this section of bin breaks matches exactly one
            # pre-specified plot segment
            sum(
              vapply(plot_segments_intervals, FUN.VALUE = logical(1),
                     function(p_int) {
                       bb[1] >= p_int$low & bb[1] <= p_int$upp &
                         bb[length(bb)] >= p_int$low & bb[length(bb)] <= p_int$upp
                       # We use this base R check instead of the interval notation,
                       # because the bin breaks might touch the limits of an
                       # open interval and would be excluded then, but we do not have
                       # to consider the boundary cases in detail here.
                     })
            ) == 1
          })
        bin_breaks[!bin_segments_with_data] <- NULL
      }

      # define limits for plotting
      if (!is_datetime_var[[rv]]) {
        myxlim <- c(floor(min_plot), ceiling(max_plot))
      } else {
        myxlim <- c(min_plot, max_plot)
        myxlim <- as.POSIXct(myxlim, origin = min(as.POSIXct(Sys.Date()), 0)) # ensure to have posixct
      }
      # prepare limit lines
      all_limits <- c(lower_limits, upper_limits)
      all_limits <- all_limits[!is.na(all_limits)]
      all_limits_df <- data.frame("limits" = names(all_limits),
                                  "values" = all_limits)
      # add the limit interval to the legend of the plot
      all_limits_df$limits <- paste(
        all_limits_df$limits,
        meta_data[meta_data[[label_col]] == rv,
                  all_limits_df$limits, drop = FALSE])
      spec_lines$limits <- paste(
        spec_lines$limits,
        meta_data[meta_data[[label_col]] == rv,
                  spec_lines$limits, drop = FALSE])

      # histogram: create plot -------------------------------------------------
      p <- ggplot(data = rv_data, aes(x = .data[["values"]],
                                      fill = .data[["limit_violations"]]))
      # add bins per segment
      for (ii in seq_along(plot_segments_intervals)) {
        p <- p +
          geom_histogram(
            data = subset(rv_data,
                          redcap_env$`in`(rv_data$values,
                                          plot_segments_intervals[[ii]])),
            breaks = bin_breaks[[ii]]
          )
      }

      p <- p +
        # add lines for limits
        geom_vline(data = all_limits_df,
                   aes(xintercept = .data[["values"]],
                       linetype = .data[["limits"]],
                       color = .data[["limits"]])) +
        # adjust colors, linetypes, etc.
        util_coord_flip(ref_env = ref_env, xlim = myxlim) +
        # TODO: estimate w and h, if p is not using discrete axes
        scale_fill_manual(values = spec_bars$fill,
                          breaks = spec_bars$limit_violations,
                          guide = "none") +
        ggplot2::scale_linetype_manual(name = "limits",
                              values = spec_lines$lty,
                              breaks = spec_lines$limits) +
        scale_color_manual(name = "limits",
                           values = spec_lines$color,
                           breaks = spec_lines$limits) +
        labs(x = paste0(rv), y = "") +
        theme_minimal() +
        theme(
          title = spec_txt,
          axis.text.x = spec_txt,
          axis.text.y = spec_txt,
          axis.title.x = spec_txt,
          axis.title.y = spec_txt
        )
    } else {
      # bar chart --------------------------------------------------------------
      # prepare limit lines (move by 0.5 to prevent overlap with bars)
      all_limits <- c(lower_limits - 0.5, upper_limits + 0.5)
      all_limits <- all_limits[!is.na(all_limits)]
      all_limits_df <- data.frame("limits" = names(all_limits),
                                  "values" = all_limits)
      # include space for the moved limits (or wide bars) to the plotting range
      max_plot <- max_plot + 0.5
      min_plot <- min_plot - 0.5

      p <- ggplot(data = rv_data, aes(x = .data[["values"]],
                                      fill = .data[["limit_violations"]])) +
        geom_bar() +
        # add lines for limits
        geom_vline(data = all_limits_df,
                   aes(xintercept = .data[["values"]],
                       linetype = .data[["limits"]],
                       color = .data[["limits"]])) +
        # adjust colors, linetypes, etc.
        util_coord_flip(ref_env = ref_env,
                        xlim = c(min_plot, max_plot)) +
        # TODO: estimate w and h, if p is not using discrete axes
        scale_fill_manual(values = spec_bars$fill,
                          breaks = spec_bars$limit_violations,
                          guide = "none") +
        ggplot2::scale_linetype_manual(name = "limits",
                              values = spec_lines$lty,
                              breaks = spec_lines$limits) +
        scale_color_manual(name = "limits",
                           values = spec_lines$color,
                           breaks = spec_lines$limits) +
        labs(x = paste0(rv), y = "") +
        theme_minimal() +
        theme(
          title = spec_txt,
          axis.text.x = spec_txt,
          axis.text.y = spec_txt,
          axis.title.x = spec_txt,
          axis.title.y = spec_txt
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
  if (return_flagged_study_data) {
    fsd <- do.call(cbind.data.frame, sd_lim)
    colnames(fsd) <- paste(
      unlist(lapply(names(sd_lim), function(rv) {
        rep(rv, length(sd_lim[[rv]]))
      })),
      unlist(lapply(sd_lim, names)),
      sep = "_")
    fsd <- cbind(ds1, fsd)
  } else {
    fsd <- NULL
  }

  # summary data ---------------------------------------------------------------
  sumdat <- do.call(rbind.data.frame,
                    lapply(setNames(nm = resp_vars), function(rv) {
                      divisor <- rep(unname(unlist(sd_n_obs[[rv]])), each = 3)
                      data.frame(
                        "Variables" = rv,
                        "Section" = rep(colnames(freq_lim_viol[[rv]]),
                                        nrow(freq_lim_viol[[rv]])),
                        "Limits" = rep(rownames(freq_lim_viol[[rv]]),
                                       each = 3),
                        "Number" = c(t(freq_lim_viol[[rv]])),
                        "Percentage" = ifelse(divisor == 0, 0,
                                              round(c(t(freq_lim_viol[[rv]])) /
                                                      divisor * 100, 2)),
                        check.names = FALSE)
                    }))
  rownames(sumdat) <- NULL

  # report summary table -------------------------------------------------------
  report_all_limits <- unique(sumdat$Limits)
  heatmap_tab <- do.call(
    rbind.data.frame,
    lapply(setNames(nm = resp_vars), function(rv) {
      n_viol <-
        rowSums(freq_lim_viol[[rv]][, c("below", "above"), drop = FALSE])
      # different N for HARD_LIMITS and SOFT_LIMITS (!),
      # so we give a relative number instead and set N to 1
      divisor <- unlist(sd_n_obs[[rv]])[rownames(freq_lim_viol[[rv]])]
      n_viol_rel <- ifelse(divisor == 0, 0, n_viol / divisor)
      out_viol <-
        setNames(rep(NA, length(report_all_limits)), report_all_limits)
      out_viol[names(n_viol_rel)] <- n_viol_rel
      out_viol <- as.data.frame(t(out_viol))
      out_viol$Variables <- rv
      out_viol$N <- 1
      return(out_viol[, c("Variables", "N",
                          setdiff(colnames(out_viol), c("Variables", "N")))])
    }))
  rownames(heatmap_tab) <- NULL
  colnames(heatmap_tab)[3:ncol(heatmap_tab)] <-
    paste(colnames(heatmap_tab)[3:ncol(heatmap_tab)], "violations")

  class(heatmap_tab) <- union("ReportSummaryTable", class(heatmap_tab))

  # summary table --------------------------------------------------------------
  sumtab_inadm_num <- vapply(
    resp_vars, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        sumdat_sub <- sumdat %>%
          dplyr::filter(Variables == rv &
                          Limits == "HARD_LIMITS")
        if (nrow(sumdat_sub) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          res <- sumdat_sub %>%
            dplyr::select(Section, Number) %>%
            dplyr::mutate(n = sum(Number)) %>%
            dplyr::filter(Section != "within") %>%
            dplyr::mutate(n_viol = sum(Number)) %>%
            dplyr::distinct(n_viol, n)
          res <- unlist(res)
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })
  sumtab_inadm_datetime <- vapply(
    resp_vars, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (!is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        sumdat_sub <- sumdat %>%
          dplyr::filter(Variables == rv &
                          Limits == "HARD_LIMITS")
        if (nrow(sumdat_sub) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          res <- sumdat_sub %>%
            dplyr::select(Section, Number) %>%
            dplyr::mutate(n = sum(Number)) %>%
            dplyr::filter(Section != "within") %>%
            dplyr::mutate(n_viol = sum(Number)) %>%
            dplyr::distinct(n_viol, n)
          res <- unlist(res)
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })

  sumtab_unc_num <- vapply(
    resp_vars, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        rv_sd_int <- sd_lim_HL_check[[rv]]
        rv_sd_int <- rv_sd_int[which(names(rv_sd_int) != "HARD_LIMITS")]
        if (length(rv_sd_int) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          rv_sd_int <- lapply(rv_sd_int, function(ll) {
            levels(ll) <- c(1, 0, 1)
            return(as.numeric(as.character(ll)))
          })
          rv_sd_int <- as.data.frame(rv_sd_int)
          rv_sd_int <- rv_sd_int[complete.cases(rv_sd_int), , drop = FALSE]
          rv_sd_int$N_lim <- rowSums(rv_sd_int)
          res <- c(length(which(rv_sd_int$N_lim > 0)),
                   nrow(rv_sd_int))
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })
  sumtab_unc_datetime <- vapply(
    resp_vars, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (!is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        rv_sd_int <- sd_lim_HL_check[[rv]]
        rv_sd_int <- rv_sd_int[which(names(rv_sd_int) != "HARD_LIMITS")]
        if (length(rv_sd_int) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          rv_sd_int <- lapply(rv_sd_int, function(ll) {
            levels(ll) <- c(1, 0, 1)
            return(as.numeric(as.character(ll)))
          })
          rv_sd_int <- as.data.frame(rv_sd_int)
          rv_sd_int <- rv_sd_int[complete.cases(rv_sd_int), , drop = FALSE]
          rv_sd_int$N_lim <- rowSums(rv_sd_int)
          res <- c(length(which(rv_sd_int$N_lim > 0)),
                   nrow(rv_sd_int))
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })

  sumtab <- data.frame(
    "Variables" = resp_vars,
    "NUM_con_rvv_unum" = sumtab_unc_num["n_viol", ],
    "PCT_con_rvv_unum" = round(sumtab_unc_num["n_viol", ] /
                                 sumtab_unc_num["n", ] * 100, 2),
    "FLG_con_rvv_unum" = ifelse(sumtab_unc_num["n_viol", ] > 0, TRUE, FALSE),
    "NUM_con_rvv_utdat" = sumtab_unc_datetime["n_viol", ],
    "PCT_con_rvv_utdat" = round(sumtab_unc_datetime["n_viol", ] /
                                  sumtab_unc_datetime["n", ] * 100, 2),
    "FLG_con_rvv_utdat" = ifelse(sumtab_unc_datetime["n_viol", ] > 0,
                                 TRUE, FALSE),
    "NUM_con_rvv_inum" = sumtab_inadm_num["n_viol", ],
    "PCT_con_rvv_inum" = round(sumtab_inadm_num["n_viol", ] /
                                 sumtab_inadm_num["n", ] * 100, 2),
    "FLG_con_rvv_inum" = ifelse(sumtab_inadm_num["n_viol", ] > 0, TRUE, FALSE),
    "NUM_con_rvv_itdat" = sumtab_inadm_datetime["n_viol", ],
    "PCT_con_rvv_itdat" = round(sumtab_inadm_datetime["n_viol", ] /
                                  sumtab_inadm_datetime["n", ] * 100, 2),
    "FLG_con_rvv_itdat" = ifelse(sumtab_inadm_datetime["n_viol", ] > 0,
                                 TRUE, FALSE))
  # for n = 0, entries in PCT columns will be NaN
  # we can replace them by 0 (but we will not replace NA by 0, because they
  # show that an indicator did not apply)
  sumtab$PCT_con_rvv_unum[which(sumtab_unc_num["n", ] == 0)] <- 0
  sumtab$PCT_con_rvv_utdat[which(sumtab_unc_datetime["n", ] == 0)] <- 0
  sumtab$PCT_con_rvv_inum[which(sumtab_inadm_num["n", ] == 0)] <- 0
  sumtab$PCT_con_rvv_itdat[which(sumtab_inadm_datetime["n", ] == 0)] <- 0

  rownames(sumtab) <- NULL

  # final return statement -----------------------------------------------------
  return(util_attach_attr(list(
    FlaggedStudyData = fsd,
    SummaryTable = sumtab,
    SummaryData = sumdat,
    ReportSummaryTable = heatmap_tab,
    SummaryPlotList = plot_list
  ), as_plotly = "util_as_plotly_con_limit_deviations"))
}



#' @family plotly_shims
#' @concept plotly_shims
#' @keywords internal
util_as_plotly_con_limit_deviations <- function(res, ...) {
  res$SummaryPlot <- util_remove_dataquieR_result_class(res$SummaryPlot)
  util_ensure_suggested("plotly")
  p <- res$SummaryPlotList
  util_stop_if_not(length(p) == 1)
  p <- p[[1]]
  lb <- ggplot_build(p)$plot$scales$scales[[2]]$get_labels(1)
  cl <- ggplot_build(p)$plot$scales$scales[[2]]$palette(1)
  br <- ggplot_build(p)$plot$scales$scales[[2]]$breaks
  py <- plotly::ggplotly(p, ...)
  for (i in seq_along(py$x$data)) {
    nm <- py$x$data[[i]]$name
    if (nm %in% paste0("(", br, ",1)")) {
      py$x$data[[i]]$name <- br[nm == paste0("(", br, ",1)")]
    } else {
      py$x$data[[i]]$showlegend <- FALSE
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
