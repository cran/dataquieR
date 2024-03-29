#' Smoothes and plots adjusted longitudinal measurements
#'
#' @description
#' The following R implementation executes calculations for quality indicator
#' "Unexpected location" (see [here](
#' https://dataquality.qihs.uni-greifswald.de/PDQC_DQ_3_2_1_3.html
#' ). Local regression (LOESS) is a versatile statistical method to explore an
#' averaged course of time series
#' measurements (Cleveland, Devlin, and Grosse 1988). In context of
#' epidemiological data, repeated measurements using the same measurement
#' device or by the same examiner can be considered a time series. LOESS allows
#' to explore changes in these measurements over time.
#'
#' [Descriptor]
#'
#' @export
#'
#' @param resp_vars [variable] the name of the continuous measurement variable
#' @param group_vars [variable] the name of the observer, device or reader
#'                             variable
#' @param time_vars [variable] the name of the variable giving the time
#'                             of measurement
#' @param co_vars [variable list] a vector of covariables for adjustment, for
#'                             example age and sex. Can be NULL (default) for no
#'                             adjustment.
#' @param min_obs_in_subgroup [integer] (optional argument) If `group_vars` is
#'                             specified, this argument can be used to specify
#'                             the minimum number of observations required for
#'                             each of the subgroups. Subgroups with fewer
#'                             observations are excluded. The default number
#'                             is `30`.
#' @param label_col [variable attribute] the name of the column in the metadata
#'                             with labels of variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                             attributes of study data
#' @param resolution [numeric] the maximum number of time points used for
#'                             plotting the trend lines
#' @param comparison_lines [list] type and style of lines with which trend
#'                             lines are to be compared. Can be mean +/- 0.5
#'                             standard deviation (the factor can be specified
#'                             differently in `sd_factor`) or quartiles
#'                             (Q1, Q2, and Q3). Arguments `color` and
#'                             `linetype` are passed to [ggplot2::geom_line()].
#' @param mark_time_points [logical] mark time points with observations
#'                             (caution, there may be many marks)
#' @param plot_observations [logical] show observations as scatter plot in the
#'                             background. If there are `co_vars` specified,
#'                             the values of the observations in the plot will
#'                             also be adjusted for the specified covariables.
#' @param plot_format [enum] AUTO | COMBINED | FACETS | BOTH. Return the plot
#'                             as one combined plot for all groups or as
#'                             facet plots (one figure per group). `BOTH` will
#'                             return both variants, `AUTO` will decide based
#'                             on the number of observers.
#'
#' @return a [list] with:
#'   - `SummaryPlotList`: list with two plots if `plot_format = "BOTH"`,
#'   otherwise one of the two figures described below:
#'     - `Loess_fits_facets`: The plot contains LOESS-smoothed curves
#'       for each level of the `group_vars` in a separate panel. Added trend
#'       lines represent mean and standard deviation or quartiles (specified
#'       in `comparison_lines`) for moving windows over the whole data.
#'     - `Loess_fits_combined`: This plot combines all curves into one
#'       panel. Given a low number of levels in the `group_vars`, this plot
#'       eases comparisons. However, if the number increases this plot may
#'       be too crowded and unclear.
#'
#' @details
#'
#' If `mark_time_points` or `plot_observations` is selected, but would result in
#' plotting more than 400 points, only a sample of the data will be displayed.
#'
#' Limitations
#'
#' The application of LOESS requires model fitting, i.e. the smoothness
#' of a model is subject to a smoothing parameter (span).
#' Particularly in the presence of interval-based missing data, high
#' variability of measurements combined with a low number of
#' observations in one level of the `group_vars` may distort the fit.
#' Since our approach handles data without knowledge
#' of such underlying characteristics, finding the best fit is complicated if
#' computational costs should be minimal. The default of
#' LOESS in R uses a span of 0.75, which provides in most cases reasonable fits.
#' The function `acc_loess` adapts the span for each level of the `group_vars`
#' (with at least as many observations as specified in `min_obs_in_subgroup`
#' and with at least three time points) based on the respective
#' number of observations.
#' LOESS consumes a lot of memory for larger datasets. That is why `acc_loess`
#' switches to a generalized additive model with integrated smoothness
#' estimation (`gam` by `mgcv`) if there are 1000 observations or more for
#' at least one level of the `group_vars` (similar to `geom_smooth`
#' from `ggplot2`).
#'
#' @importFrom ggplot2 ggplot aes scale_color_manual xlab ylab geom_point
#'                     geom_line facet_wrap theme_minimal ggtitle theme
#'                     element_blank
#' @importFrom stats as.formula lm loess predict na.omit glm binomial poisson sd
#'                   cov var runif
#'
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_loess.html
#' )
acc_loess <- function(resp_vars, group_vars = NULL, time_vars, co_vars = NULL,
                      study_data, meta_data, label_col = NULL,
                      min_obs_in_subgroup = 30, resolution = 80,
                      comparison_lines = list(type = c("mean/sd", "quartiles"),
                                              color = "grey30",
                                              linetype = 2,
                                              sd_factor = 0.5),
                      mark_time_points =
                        getOption("dataquieR.acc_loess.mark_time_points",
                                  FALSE),
                      plot_observations =
                        getOption("dataquieR.acc_loess.plot_observations",
                                  TRUE),
                      plot_format = "AUTO") {
  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  # correct variable use?
  util_correct_variable_use("resp_vars",
                            need_type = "!string",
                            need_scale = "interval | ratio",
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

  # check that other arguments are specified correctly
  util_expect_scalar(min_obs_in_subgroup,
                     check_type = util_is_numeric_in(
                       min = 1, whole_num = TRUE, finite = TRUE),
                     convert_if_possible = function(x) {
                       x1 <- suppressWarnings(as.integer(x))
                       if (is.na(x1) ||
                           !util_is_numeric_in(min = 1, whole_num = TRUE,
                                               finite = TRUE)(x1)) {
                         x1 <- 30L
                         util_message(paste(
                              "Argument min_obs_in_subgroup is not specified",
                              "correctly and is set to 30 instead."),
                           applicability_problem = TRUE)
                       }
                       x1
                     },
                     conversion_may_replace_NA = TRUE)

  util_expect_scalar(resolution,
                     check_type = util_is_numeric_in(
                       min = 3, whole_num = TRUE, finite = TRUE),
                     convert_if_possible = function(x) {
                       x1 <- suppressWarnings(as.integer(x))
                       if (is.na(x1) ||
                           !util_is_numeric_in(min = 3, whole_num = TRUE,
                                               finite = TRUE)(x1)) {
                         x1 <- 80L
                         util_message(
                           paste("Argument resolution is not specified",
                                 "correctly and is set to 80 instead."),
                           applicability_problem = TRUE)
                       }
                       x1
                     },
                     conversion_may_replace_NA = TRUE)

  if (!is.list(comparison_lines) ||
      !all(names(comparison_lines) %in%
           c("type", "color", "linetype", "sd_factor")) ||
      ("type" %in% names(comparison_lines) &&
       !all(comparison_lines$type %in% c("mean/sd", "quartiles"))) ||
      ("type" %in% names(comparison_lines) &&
       "mean/sd" %in% comparison_lines$type  &&
        "sd_factor" %in% names(comparison_lines) &&
        (!is.numeric(comparison_lines$sd_factor) ||
          !is.finite(comparison_lines$sd_factor) ||
          length(comparison_lines$sd_factor) != 1
        )
      )
  ) {
    util_error(c(
      "%s needs to be a list of arguments as specified in the documentation."),
      dQuote("comparison_lines"),
      applicability_problem = TRUE)
  }
  if ("type" %in% names(comparison_lines)) {
    if (length(comparison_lines$type) > 1) {
      comparison_lines$type <- comparison_lines$type[1]
    }
    lines_to_add <- comparison_lines$type
  } else {
    lines_to_add <- "mean/sd"
  }
  sd_fac <- NULL
  if (lines_to_add == "mean/sd" & "sd_factor" %in% names(comparison_lines)) {
    sd_fac <- comparison_lines$sd_factor
  } else {
    sd_fac <- 0.5
  }
  lines_arg <- list(color = "grey30", linetype = 2)
  if ("color" %in% names(comparison_lines)) {
    lines_arg$color <- comparison_lines$color
  }
  if ("linetype" %in% names(comparison_lines)) {
    lines_arg$linetype <- comparison_lines$linetype
  }

  util_expect_scalar(mark_time_points,
                     check_type = is.logical)

  util_expect_scalar(plot_observations,
                     check_type = is.logical)

  util_expect_scalar(plot_format,
                     check_type = function(x) {
                       is.character(x) &&
                         any(grepl(x,
                                   c("AUTO", "COMBINED", "FACETS", "BOTH",
                                     "auto", "combined", "facets", "both" ),
                                   fixed = TRUE)) })

  # omit missing values and unnecessary variables
  n_prior <- nrow(ds1)
  ds1 <- ds1[, c(resp_vars, time_vars, group_vars, co_vars)]
  ds1 <- ds1[complete.cases(ds1[, c(time_vars, group_vars, co_vars)]), ]
  n_post <- nrow(ds1)
  msg <- NULL
  if (n_post < n_prior) {
    msg <- paste0(
      "Due to missing values in ",
      paste0(c(group_vars, co_vars), collapse = ", "),
      " or ", time_vars, ", N = ", n_prior - n_post,
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

  # convert group_vars to factor
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

  if (length(levels(ds1[[group_vars]])) < 2) {
    plot_format <- "COMBINED"
  }

  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.",
               applicability_problem = FALSE)
  }

  # order data by time and groups
  # (for plotting and for the moving window calculations)
  ds1 <- ds1[order(ds1[[time_vars]], ds1[[group_vars]]), ]
  tp_seq <- ds1[[time_vars]]
  tp_seq <- unique(tp_seq)

  # reduce time points according to the resolution
  # compute number of seconds between two consecutive time points to match
  # the given resolution
  if (resolution < length(tp_seq)) {
    period <- (max(tp_seq) - min(tp_seq)) / (resolution - 1)
    secs <- suppressWarnings(as.integer(as.double(period, units = "secs")))
    if (secs <= 0) {
      secs <- 1
    }
    # This defines a new sequence of equally spaced time points ranging from
    # the earliest observed time point to the last one.
    # This sequence is stored in `tp_round_seq`. The number of time points
    # equals the number given in `resolution`.
    tp_round_seq <- suppressWarnings(as.POSIXct(
      seq(from = min(tp_seq),
          to = max(tp_seq),
          by = secs)))
    ds1[["ROUND_TIME"]] <- suppressWarnings(as.POSIXct(
      lubridate::round_date(ds1[[time_vars]], unit = tp_round_seq)))
  } else {
    tp_round_seq <- tp_seq
    ds1[["ROUND_TIME"]] <- ds1[[time_vars]]
  }

  # store a numeric version of the original time variable for later calculations
  ds1$time_vars_num <- suppressWarnings(as.numeric(ds1[[time_vars]]))

  # Modelling group-wise trends ------------------------------------------------
  # adjust response for covariables (if any) using a linear model
  if (length(co_vars) > 0) {
    fmla <- as.formula(paste0(paste0(util_bQuote(resp_vars), "~"),
                              paste0(util_bQuote(co_vars),
                                     collapse = " + ")))
    lmfit1 <- lm(fmla, data = ds1)
    # store residuals + intercept (i.e., discard effects from covariables)
    # These values will be used for LOESS fits. In this way, we fit LOESS after
    # adjusting the response for the covariables.
    ds1$Residuals <- lmfit1$residuals + lmfit1$coefficients[1]
    # Memory consumption
    rm(lmfit1)
  } else {
    ds1$Residuals <- ds1[[resp_vars]]
  }

  # fit LOESS/GAM for each group separately
  grouped_ds1 <- split(ds1, ds1[[group_vars]])
  processed_grouped_ds1 <- lapply(grouped_ds1, function(data_i) {
    # To plot the trend line at the time points in `tp_round_seq` (restricted to
    # those values that lie within the observed time period for this group), we
    # need fitted values at these time points.
    data_i_seq <- tp_round_seq[
      (which(tp_round_seq == min(data_i[["ROUND_TIME"]]))):
        (which(tp_round_seq == max(data_i[["ROUND_TIME"]])))]
    data_i_seq_num <- suppressWarnings(as.numeric(data_i_seq))
    data_i_seq_num <- as.data.frame(data_i_seq_num)
    colnames(data_i_seq_num) <- "time_vars_num"

    if (max(tab_groups) > 1000 &&
        util_ensure_suggested("mgcv",
          "use GAM from mgcv instead of loess for lower memory consumption",
          err = FALSE)) {
      # If there are too many observations, switch to GAM instead of LOESS
      # because of memory consumption (if available).
      fit_i <- mgcv::gam(Residuals ~ s(time_vars_num, bs = "cs"),
                         method = "REML",
                         data = data_i)
      fit_vals <- mgcv::predict.gam(fit_i, data_i_seq_num)
    } else { # LOESS
      # calculate smoothing parameter for data_i
      max_smooth <- round(1 / log10(length(unique(data_i[[time_vars]]))), 2)
      max_smooth <- max(0.3, # max_smooth should be greater than or equal to 0.3
        # max_smooth should not be greater than 1
        # (happens if there are few time points)
        min(max_smooth, 1),
        na.rm = TRUE)
      # fit LOESS for data_i
      fit_i <- suppressWarnings(
        loess(Residuals ~ time_vars_num,
              span = max_smooth,
              data = data_i))
      fit_vals <- predict(fit_i, data_i_seq_num)
    }

    pred_df <- data.frame(TIME = data_i_seq,
                          FITTED_VALUE = fit_vals,
                          GROUP = rep(data_i[[group_vars]][1],
                                      length(data_i_seq)))
    if (is.na(pred_df$FITTED_VALUE[1])) {
      # This value might be missing if the minimum of `data_i_seq` (round time!)
      # was not included in the actual observed time points in `data_i` (`fit_i`
      # for `time_vars_num`!), and the prediction is only supported for values
      # in the range of `data_i[[resp_vars]]` (for LOESS, extrapolation is not
      # possible). We include the earliest observed time point instead.
      pred_df[1, "TIME"] <- data_i[[time_vars]][1]
      pred_df[1, "FITTED_VALUE"] <- fit_i$fitted[1]
    }
    if (is.na(pred_df$FITTED_VALUE[nrow(pred_df)])) {
      # same as above, but for the last value in `data_i_seq`
      pred_df[nrow(pred_df), "TIME"] <- data_i[[time_vars]][nrow(data_i)]
      pred_df[nrow(pred_df), "FITTED_VALUE"] <- fit_i$fitted[nrow(data_i)]
    }
    return(res_round_tp = pred_df[which(!is.na(pred_df$FITTED_VALUE)), ])
  })
  # https://stackoverflow.com/a/39838759
  fit_groups <- dplyr::bind_rows(processed_grouped_ds1)
  # Memory consumption
  rm(grouped_ds1)

  # Calculate comparison lines -------------------------------------------------
  # We will compute either mean and SD or quartiles using a moving window
  # approach on the original, complete dataset (if not adjusted for covariates)
  # or on the residuals after adjusting for covariates.
  if (length(tp_seq) >= 7) {
    mov_win_width <- round(0.3 * length(tp_seq))
    # split window into two parts to align it approximately at the middle
    # If the number is not even, we will have a smaller first part and a larger
    # second part (by one), otherwise both parts will be equal.
    part1 <- floor(0.5 * mov_win_width)
    part2 <- mov_win_width - part1
  } else { # We have to ensure that part1 contains at least one observation.
    part1 <- floor(0.5 * length(tp_seq))
    # note: length(tp_seq) >= 3 (ensured by checks during data preparation)
    part2 <- length(tp_seq) - part1
  }
  # To reduce computing time, we will only calculate values that are required
  # for the plot.
  mov_win_res <- lapply(tp_round_seq, function(tp) {
    if (tp %in% tp_seq) {
      i <- which(tp_seq == tp)
      res_i <- util_for_moving_window(tp_seq = tp_seq,
                                      ds1_resp_var = ds1[["Residuals"]],
                                      ds1_time_var = ds1[[time_vars]],
                                      i = i,
                                      part1 = part1,
                                      part2 = part2,
                                      mode = lines_to_add,
                                      sd_fac = sd_fac)
    } else {
      # We calculate the two moving windows below and above `tp` and interpolate
      # the required value.
      i1 <- which.max(tp_seq[which(tp_seq < tp)])
      i1 <- i1 + which(tp_seq < tp)[1] - 1
      i2 <- which.min(tp_seq[which(tp_seq > tp)])
      i2 <- i2 + which(tp_seq > tp)[1] - 1
      res_i <- util_for_moving_window(tp_seq = tp_seq,
                                      ds1_resp_var = ds1[["Residuals"]],
                                      ds1_time_var = ds1[[time_vars]],
                                      i = c(i1, i2),
                                      part1 = part1,
                                      part2 = part2,
                                      mode = lines_to_add,
                                      sd_fac = sd_fac)
      if (length(res_i) == 2) {
        data_i <- as.data.frame(do.call(rbind, res_i))
        if (all(is.na(data_i))) {
          res_i <- res_i[[1]]
        } else {
          data_i$TIME <- c(tp_seq[i1], tp_seq[i2])
          # interpolate value for `tp` using the two neighboring time points
          res_i <- vapply(c("low", "mid", "high"), FUN.VALUE = numeric(1),
                          FUN = function(cc) {
                            local_lm <- lm(as.formula(paste(cc, "~ TIME")),
                                           data = data_i)
                            return(suppressWarnings(
                              predict(local_lm,
                                      data.frame("TIME" = tp))))
                          })
        }
      }
    }
    return(res_i)
  })
  global_trends <- as.data.frame(do.call(rbind, mov_win_res))
  # fill in values at the beginning and at the end (`NA` because
  # there were not enough data for a full window)
  compl <- complete.cases(global_trends)
  if (!compl[1]) {
    global_trends[1:(which(compl)[1] - 1), ] <- global_trends[which(compl)[1], ]
  }
  if (!compl[nrow(global_trends)]) {
    global_trends[(rev(which(compl))[1] + 1):nrow(global_trends), ] <-
      global_trends[rev(which(compl))[1], ]
  }
  global_trends$TIME <- tp_round_seq

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

  if (lines_to_add == "mean/sd") {
    lines_info <- paste("Trend lines shown for comparison indicate mean \u00B1",
                        sd_fac, "SD.")
  } else {
    lines_info <- paste("Trend lines shown for comparison indicate",
                        "quartiles Q1, Q2, Q3.")
  }

  # If observations should be included in the plot, we have to ensure that not
  # too many points are displayed.
  points_shown_max <- 400
  if (mark_time_points | plot_observations) {
    sel_obs <- list(
      # Dashed marks for observed time points on fitted trend line:
      "dp_facets" = 1:nrow(ds1), # for facets
      "dp_comb" = 1:nrow(ds1), # for combined plot
      # Scatter plot of observed values:
      "obs_facets" = 1:nrow(ds1), # for facets
      "obs_comb" = 1:nrow(ds1)) # for combined plot
    # If there are too many points to plot, we will only plot a sample of
    # them.
    if (nrow(ds1) > points_shown_max) {
      # Cut x axis (time) into five segments for (stratified) sampling.
      ds1_time_var_cut <- cut(as.numeric(ds1[[time_vars]]),
                              breaks = 5, labels = FALSE)
      ds1_tab <- table(ds1[[group_vars]], ds1_time_var_cut)
      ds1_tab_adj <- floor(ds1_tab / nrow(ds1) * points_shown_max)
      # store row indices for each group and time segment to ease data wrangling
      gr_ind <- setNames(nm = levels(ds1[[group_vars]]),
                         lapply(levels(ds1[[group_vars]]), function(gr) {
                           which(ds1[[group_vars]] == gr)
                         }))
      tp_ind <- setNames(nm = 1:max(ds1_time_var_cut),
                         lapply(1:max(ds1_time_var_cut), function(tp) {
                           which(ds1_time_var_cut == tp)
                         }))
      set.seed(400)
      sel_obs$obs_facets <- sort(unlist(
        lapply(levels(ds1[[group_vars]]), function(gr) {
          if (sum(ds1_tab[gr, ]) > points_shown_max) {
            ds1_tab_gr_adj <- floor(ds1_tab[gr, ] / sum(ds1_tab[gr, ]) *
                                      points_shown_max)
            unlist(lapply(names(ds1_tab_gr_adj), function(tp) {
              gr_tp_ind <- intersect(gr_ind[[gr]], tp_ind[[tp]])
              if (length(gr_tp_ind) == 0 | ds1_tab_gr_adj[tp] == 0) {
                out <- integer(0)
              } else if (var(ds1[gr_tp_ind, "Residuals"]) == 0) {
                out <- sample(gr_tp_ind, size = ds1_tab_gr_adj[tp])
              } else {
                # keep 'extreme' values to show the true range
                gr_tp_max <- gr_tp_ind[which.max(ds1[gr_tp_ind, "Residuals"])]
                gr_tp_min <- gr_tp_ind[which.min(ds1[gr_tp_ind, "Residuals"])]
                if (ds1_tab_gr_adj[tp] <= 2) {
                  out <- sample(c(gr_tp_max, gr_tp_min),
                                size = ds1_tab_gr_adj[tp])
                } else {
                  out <- c(gr_tp_max, gr_tp_min,
                           sample(setdiff(gr_tp_ind, c(gr_tp_max, gr_tp_min)),
                                  size = ds1_tab_gr_adj[tp] - 2))
                }
              }
              return(out)
            }))
          } else {
            return(gr_ind[[gr]])
          }
        })))
      sel_obs$obs_comb <- sort(unlist(
        lapply(levels(ds1[[group_vars]]), function(gr) {
          unlist(
            lapply(colnames(ds1_tab_adj), function(tp) {
              gr_tp_ind <- intersect(gr_ind[[gr]], tp_ind[[tp]])
              if (length(gr_tp_ind) == 0 | ds1_tab_adj[gr, tp] == 0) {
                out <- integer(0)
              } else if (var(ds1[gr_tp_ind, "Residuals"]) == 0) {
                out <- sample(gr_tp_ind, size = ds1_tab_adj[gr, tp])
              } else {
                # keep 'extreme' values to show the true range
                gr_tp_max <- gr_tp_ind[which.max(ds1[gr_tp_ind, "Residuals"])]
                gr_tp_min <- gr_tp_ind[which.min(ds1[gr_tp_ind, "Residuals"])]
                if (ds1_tab_adj[gr, tp] <= 2) {
                  out <- sample(c(gr_tp_max, gr_tp_min),
                                size = ds1_tab_adj[gr, tp])
                } else {
                  out <- c(gr_tp_max, gr_tp_min,
                           sample(setdiff(gr_tp_ind, c(gr_tp_max, gr_tp_min)),
                                  size = ds1_tab_adj[gr, tp] - 2))
                }
              }
              return(out)
            }))
        })))
      # When we mark observed time points on the trend line, we can
      # omit 'duplicated' marks from observations at the same time point for
      # the same group.
      sel_obs$dp_facets <- which(!duplicated(ds1[sel_obs$obs_facets,
                                                 c(group_vars, time_vars)]))
      sel_obs$dp_comb <- which(!duplicated(ds1[sel_obs$obs_comb,
                                               c(group_vars, time_vars)]))
    }
  }

  geom_dp_facets <- NULL
  geom_dp_comb <- NULL
  if (mark_time_points) {
    sel_obs_dp <- sort(unique(c(sel_obs$dp_facets, sel_obs$dp_comb)))
    mark_obs <- ds1[sel_obs_dp, c(time_vars, group_vars)]
    colnames(mark_obs) <- c("TIME", "GROUP")
    mark_obs$facets <- as.numeric(sel_obs_dp %in% sel_obs$dp_facets)
    mark_obs$comb <- as.numeric(sel_obs_dp %in% sel_obs$dp_comb)
    # If we want to show marks for the actual observed time points
    # (not rounded time points) on top of the trend line, we have to estimate
    # the position of the marks on the trend line. Otherwise, marks and trend
    # line will not match if the resolution is reduced considerably in
    # comparison to the actual resolution of the data.
    pred_pos_mark <-
        vapply(1:nrow(mark_obs), FUN.VALUE = numeric(1),
               function(i) {
                 tp <- mark_obs[["TIME"]][i]
                 gr <- mark_obs[["GROUP"]][i]
                 pred_gr <- processed_grouped_ds1[[gr]]
                 if (tp %in% pred_gr$TIME) {
                   res_i <- pred_gr$FITTED_VALUE[which(pred_gr$TIME == tp)]
                 } else {
                   tp_ind1 <- which.max(pred_gr$TIME[which(pred_gr$TIME < tp)])
                   tp_ind1 <- tp_ind1 + which(pred_gr$TIME < tp)[1] - 1
                   tp_ind2 <- which.min(pred_gr$TIME[which(pred_gr$TIME > tp)])
                   tp_ind2 <- tp_ind2 + which(pred_gr$TIME > tp)[1] - 1
                   if (length(tp_ind1) == 0 & length(tp_ind2) == 0) {
                     res_i <- NA # should not happen
                   } else if (length(tp_ind1) == 0 | length(tp_ind2) == 0) {
                     res_i <- pred_gr$FITTED_VALUE[c(tp_ind1, tp_ind2)]
                   } else {
                     local_lm <- lm(FITTED_VALUE ~ TIME,
                                    data = pred_gr[tp_ind1:tp_ind2, ])
                     res_i <- suppressWarnings(predict(
                       local_lm, data.frame("TIME" = tp)))
                   }
                 }
                 return(res_i)
               })
    mark_obs$FITTED_VALUE <- pred_pos_mark
    geom_dp_facets <- geom_point(
      shape = "|",
      data = mark_obs[mark_obs$facets == 1, c("TIME", "GROUP", "FITTED_VALUE")]
    )
    geom_dp_comb <- geom_point(
      shape = "|",
      data = mark_obs[mark_obs$comb == 1, c("TIME", "GROUP", "FITTED_VALUE")]
    )
  }

  geom_obs_facets <- NULL
  geom_obs_comb <- NULL
  if (plot_observations) {
    scatter_obs_facets <- ds1[sel_obs$obs_facets,
                              c(time_vars, "Residuals", group_vars)]
    colnames(scatter_obs_facets) <- c("TIME", "FITTED_VALUE", "GROUP")
    geom_obs_facets <- geom_point(data = scatter_obs_facets,
                                  alpha = 0.2)
    scatter_obs_comb <- ds1[sel_obs$obs_comb,
                            c(time_vars, "Residuals", group_vars)]
    colnames(scatter_obs_comb) <- c("TIME", "FITTED_VALUE", "GROUP")
    geom_obs_comb <- geom_point(data = scatter_obs_comb,
                                alpha = 0.2)

  }

  if (length(levels(ds1[[group_vars]])) <= 8) {
    hex_code <- c(
      "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
    )
    names(hex_code) <- as.character(levels(ds1[[group_vars]]))
  } else {
    hex_code <- NULL
  }

  # Facet-Grids for categorical variable (observer/device)
  p1 <- ggplot(fit_groups,
               aes(x = .data$TIME,
                   y = .data$FITTED_VALUE,
                   color = .data$GROUP)) + {
      if (!is.null(hex_code)) {
        scale_color_manual(values = hex_code)
      }
    } +
    xlab(lines_info) +
    ylab("") +
    geom_dp_facets +
    geom_obs_facets +
    geom_line() +
    facet_wrap(~ .data$GROUP, ncol = 2) + #TODO: What about this ~?
    geom_line(data = global_trends, aes(y = .data$low, group = NA),
              color = lines_arg$color, linetype = lines_arg$linetype) +
    geom_line(data = global_trends, aes(y = .data$mid, group = NA),
              color = lines_arg$color, linetype = 1) +
    geom_line(data = global_trends, aes(y = .data$high, group = NA),
              color = lines_arg$color, linetype = lines_arg$linetype) +
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
      }
    } +
    xlab(lines_info) +
    ylab("") +
    geom_dp_comb +
    geom_obs_comb +
    geom_line() +
    geom_line(data = global_trends, aes(y = .data$low, group = NA),
              color = lines_arg$color, linetype = lines_arg$linetype) +
    geom_line(data = global_trends, aes(y = .data$mid, group = NA),
              color = lines_arg$color, linetype = 1) +
    geom_line(data = global_trends, aes(y = .data$high, group = NA),
              color = lines_arg$color, linetype = lines_arg$linetype) +
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

  if (plot_format == "BOTH") {
    return(list(SummaryPlotList = pl))
  } else if (plot_format == "COMBINED") {
    return(list(SummaryPlotList = setNames(pl["Loess_fits_combined"],
                    nm = resp_vars)))
  } else if (plot_format == "FACETS") {
    return(list(SummaryPlotList = setNames(pl["Loess_fits_facets"],
                    nm = resp_vars)))
  } else if (plot_format != "AUTO") {
    util_message("Unknown %s: %s -- will switch to default value AUTO.",
               dQuote("plot_format"), dQuote(plot_format),
               applicability_problem = TRUE)
  }
  if (length(levels(ds1[[group_vars]])) < 15) {
    selection <- "Loess_fits_combined"
  } else {
    selection <- "Loess_fits_facets"
  }
  pl <- pl[selection]
  names(pl) <- resp_vars
  return(list(SummaryPlotList = pl))
}


util_for_moving_window <- function(tp_seq, ds1_resp_var, ds1_time_var,
                                   i, part1, part2,
                                   mode, sd_fac) {
  if (is.null(i)) {
    return(c("low" = NA,
             "mid" = NA,
             "high" = NA))
  }
  if (length(i) > 1) {
    res_out <- lapply(i, util_for_moving_window,
                      tp_seq = tp_seq, ds1_resp_var = ds1_resp_var,
                      ds1_time_var = ds1_time_var, part1 = part1, part2 = part2,
                      mode = mode, sd_fac = sd_fac)
    return(res_out)
  }
  if (i - part1 < 0 |
      i + part2 > length(tp_seq)) {
    # The moving window overlaps the beginning or end of the sequence of
    # observed time points.
    return(c("low" = NA,
             "mid" = NA,
             "high" = NA))
  } else {
    tp1 <- tp_seq[i - part1 + 1]
    # i is contained in the first part
    tp2 <- tp_seq[i + part2]
    data_i <- ds1_resp_var[ds1_time_var >= tp1 &
                             ds1_time_var <= tp2]
    if (mode == "mean/sd") {
      center <- mean(data_i)
      sd_emp <- sd(data_i)
      upper <- center + sd_fac * sd_emp
      lower <- center - sd_fac * sd_emp
    } else {
      qq <- quantile(data_i,
                     probs = c(0.25, 0.5, 0.75),
                     names = FALSE)
      lower <- qq[1]
      center <- qq[2]
      upper <- qq[3]
    }
    return(c("low" = lower,
             "mid" = center,
             "high" = upper))
  }
}
