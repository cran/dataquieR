#' Smoothes and plots adjusted longitudinal measurements
#'
#' @description
#' The following R implementation executes calculations for quality indicator
#' Unexpected distribution wrt location (link). Local regression (LOESS) is a
#' versatile statistical method to explore an averaged course of time series
#' measurements (Cleveland, Devlin, and Grosse 1988). In context of
#' epidemiological data, repeated measurements using the same measurement
#' device or by the same examiner can be considered a time series. LOESS allows
#' to explore changes in these measurements over time.
#'
#' @export
#'
#' @param resp_vars [variable] the name of the continuous measurement variable
#' @param group_vars [variable] the name of the observer, device or reader
#'                              variable
#' @param time_vars [variable] a variable identifying the variable with the time
#'                             of measurement
#' @param co_vars [variable list] a vector of covariables, e.g. age and sex for
#'                                adjustment. Can be NULL (default) for no
#'                                adjustment.
#' @param min_obs_in_subgroup [integer] from=0. optional argument if
#'                                  `group_vars` are used. This argument
#'                                  specifies the minimum number of observations
#'                                  that is required to include a subgroup
#'                                   (level) of the group variable named by
#'                                    `group_vars` in the analysis. Subgroups
#'                                  with less observations are excluded. The
#'                                  default is `30`.
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param resolution [numeric] how many timepoints have a standard error
#'                             estimation
#' @param se_line [list] standard error estimator line style, as arguments
#'                       passed to [ggplot2::geom_line()]
#' @param plot_data_time [logical] mark times with data values (caution, there
#'                                 may be many marks)
#' @param plot_format [enum] AUTO | COMBINED | FACETS | BOTH. Return the
#'                           LOESS plot as a combined plot or as facets plots
#'                           one per group. BOTH will return both plot variants,
#'                           AUTO will decide based on the number of observers.
#'
#' @return a [list] with:
#'   - `SummaryPlotList`: list with two plots:
#'     - `Loess_fits_facets`: ggplot2 LOESS plot provides panels for each
#'       subject/object. The plot contains LOESS-smoothed curves
#'       for each level of the group_vars. The red dashed lines represent the
#'       confidence interval of a LOESS curve for the whole data.
#'     - `Loess_fits_combined`: ggplot2 LOESS plot combines all curves into one
#'       panel and is obtained by `myloess$Loess_fits_combined`. Given a low
#'       number of levels in the group_vars
#'       this plot eases comparisons. However, if number increases this plot may
#'       be too crowded and unclear.
#'
#'
#' @details
#'
#' If `plot_data_time` is not set, it will be selected based on the number of
#' data points per group: If more than 4000 points would be plotted for at least
#' one group, the > 4000 marks will not be plotted.
#'
#' Limitations
#'
#' The application of LOESS usually requires model fitting, i.e. the smoothness
#' of a model is subject to a smoothing parameter (span).
#' Particularly in the presence of interval-based missing data (`USR_181`), high
#' variability of measurements combined with a low number of
#' observations in one level of the group_vars the fit to the data may be
#' distorted. Since our approach handles data without knowledge
#' of such underlying characteristics, finding the best fit is complicated if
#' computational costs should be minimal. The default of
#' LOESS in R uses a span 0.75 which provides in most cases reasonable fits. The
#' function above increases the fit to the data automatically
#' if the minimum of observations in one level of the group_vars is higher than
#' `n=30`.
#'
#' @importFrom ggplot2 ggplot aes_ scale_color_manual xlab ylab geom_point
#'                     geom_line facet_wrap theme_minimal ggtitle theme
#'                     element_blank
#' @importFrom stats as.formula lm loess predict na.omit glm binomial poisson sd
#'                   cov
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_acc_impl_loess.html
#' )
acc_loess <- function(resp_vars, group_vars, time_vars, co_vars = NULL,
                      min_obs_in_subgroup, label_col = NULL, study_data,
                      meta_data, resolution = 180, se_line = list(color = "red",
                                                                  linetype = 2),
                      plot_data_time, plot_format = "AUTO") {

  ###########################
  # STOPS, PREPS AND CHECKS #
  ###########################

  util_prepare_dataframes()

  if (missing(min_obs_in_subgroup)) {
    min_obs_in_subgroup <- 30
    util_warning(
      "No min_obs_in_subgroup was set. Default n=30 per level is used.",
      applicability_problem = TRUE
      )
  } else {
    if (length(min_obs_in_subgroup) != 1) {
      util_error("%s should be a scalar integer value, not %d values.",
                 dQuote("min_obs_in_subgroup"), length(min_obs_in_subgroup),
                 applicability_problem = TRUE)
    }
    .min_obs_in_subgroup <- suppressWarnings(as.integer(min_obs_in_subgroup))
    if (is.na(.min_obs_in_subgroup) != is.na(min_obs_in_subgroup)) {
      util_warning(c(
        "Coulud not convert min_obs_in_subgroup %s to a number.",
        "Set to standard value n=30."),
        dQuote(as.character(min_obs_in_subgroup)),
        applicability_problem = TRUE
      )
      min_obs_in_subgroup <- 30
    } else {
      min_obs_in_subgroup <- .min_obs_in_subgroup
    }
    if (is.na(min_obs_in_subgroup)) {
      min_obs_in_subgroup <- 30
      util_warning("No min_obs_in_subgroup. Default n=30 per level is used.",
                   applicability_problem = TRUE)
    }
  }

  # correct variable use?
  util_correct_variable_use("resp_vars")
  util_correct_variable_use("group_vars")
  util_correct_variable_use("time_vars")

  if (length(resolution) != 1 ||
      !is.numeric(resolution) ||
      !is.finite(resolution)) {
    util_error("%s needs to be a single finite numeric value.",
               dQuote("resolution"),
               applicability_problem = TRUE)
  }

  if (!is.list(se_line)) {
    util_error(c(
      "%s needs to be a list of arguments for ggplot2::geom_line for",
      "the standard error lines."),
      dQuote("se_line"),
      applicability_problem = TRUE)
  }

  if (is.null(co_vars)) {
    co_vars <- character(0)
  }

  # util_ensure_variable_exactly_once_avail("co_vars", allow_na = TRUE,
  # allow_more_than_one = TRUE)
  util_correct_variable_use("co_vars", allow_more_than_one = TRUE,
                            allow_null = TRUE)

  co_vars <- na.omit(co_vars)

  # missing in resp_vars?
  if (any(is.na(ds1[[resp_vars]]))) {
    util_warning(paste0(sum(is.na(ds1[[resp_vars]])),
                        " observations were omitted due to missing values in '",
                        resp_vars, "'",
      collapse = " "),
      applicability_problem = FALSE)
    ds1 <- ds1[!(is.na(ds1[[resp_vars]])), ]
  }

  # missings in time_vars?
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[!(is.na(ds1[[time_vars]])), ]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_warning(paste0("Due to missing values in ", time_vars, " ",
                        n_prior - n_post, " observations were deleted."),
                 applicability_problem = FALSE)
  }

  # missings in group_vars?
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[!(is.na(ds1[[group_vars]])), ]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_warning(paste0("Due to missing values in ", group_vars, " ",
                        n_prior - n_post, " observations were deleted."),
                 applicability_problem = FALSE)
  }

  # unparseable time values in time_vars?
  if (is.character(ds1[[time_vars]]) || is.factor(ds1[[time_vars]])) {
    before <- sum(is.na(ds1[[time_vars]]))
    if (util_anytime_installed()) {
      ds1[[time_vars]] <- anytime::anytime(ds1[[time_vars]])
    } else {
      message(sprintf(
        paste("Package %s is not installed. Trying conversion with base",
              "as.POSIXct function.", collapse = " "),
        dQuote("anytime")))
      ds1[[time_vars]] <- suppressWarnings(as.POSIXct(ds1[[time_vars]],
                                                      optional = TRUE))
    }
    after <- sum(is.na(ds1[[time_vars]]))
    if (before < after) {
      util_warning(
        "Converting %s to DATETIME, %d values could not be converted",
        dQuote(time_vars), after - before,
        applicability_problem = FALSE)
    }
    if (all(is.na(ds1[[time_vars]]))) {
      util_error("No not-missing value in DATETIME variable %s",
                 dQuote(time_vars),
                 applicability_problem = FALSE)
    }
    n_prior <- dim(ds1)[1]
    ds1 <- ds1[!(is.na(ds1[[time_vars]])), ]
    n_post <- dim(ds1)[1]
    if (n_post < n_prior) {
      util_warning(paste0("Due to invalid time formats in ", time_vars, " ",
                          n_prior - n_post, " observations were deleted."),
                   applicability_problem = FALSE)
    }
  }

  # missings in co_vars?
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[rowSums(is.na(ds1[, co_vars, drop = FALSE])) == 0, , drop = FALSE]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_warning(paste0("Due to missing values in any of ",
                        paste0(co_vars, collapse = ", "), " ",
                        n_prior - n_post, " observations were deleted."),
                 applicability_problem = FALSE)
  }

  # Type factor
  ds1[[group_vars]] <- factor(ds1[[group_vars]])

  # too few observations per level?
  check_df <- table(ds1[[group_vars]][!(is.na(ds1[[resp_vars]]))])
  obs_gt_minobs <- names(check_df)[check_df >= min_obs_in_subgroup]
  obs_lt_minobs <- names(check_df)[check_df < min_obs_in_subgroup]

  # min_obs_in_subgroup is always defined, if not specified set to 30
  if (length(obs_lt_minobs) > 0) {
    util_warning(paste0(c("The following levels:", obs_lt_minobs,
                          "have < 30 observations and were discarded."),
                        collapse = " "),
                 applicability_problem = FALSE)
    # exclude levels with too few observations
    ds2 <- subset(ds1, ds1[[group_vars]] %in% obs_gt_minobs)
    # dropping unused levels
    ds2[[group_vars]] <- factor(ds2[[group_vars]])
  } else {
    ds2 <- ds1
  }

  if (nrow(ds2) == 0) {
    util_error("No data left, cannot produce a plot, sorry.",
               applicability_problem = FALSE)
  }

  rm(ds1) # memory consumption

  if (is.factor(ds2[[resp_vars]])) {
    # reference_category <- NA
    # if (("refcat" %in% colnames(meta_data) &&
    #    !is.na(meta_data[meta_data[[VAR_NAMES]] == resp_vars, "refcat"])) &&
    #   ("valuelist" %in% colnames(meta_data) &&
    #    !is.na(meta_data[meta_data[[VAR_NAMES]] == resp_vars, "valuelist"]))) {
    #   valuelist <-
    #           meta_data[meta_data[[VAR_NAMES]] == resp_vars, VALUE_LABELS]
    #   valuelist <- util_parse_assignments(valuelist)
    #   reference_category <-
    #           meta_data[meta_data[[VAR_NAMES]] == resp_vars, "refcat"]
    #   reference_category <- valuelist[[reference_category]]
    # }
    # if (TRUE || is.null(reference_category) || is.na(reference_category)) {
      if (!is.ordered(ds2[[resp_vars]])) {
        util_warning(c("%s is a categorial but not an ordinal variable.",
                       "I'll use the levels as ordinals, but this may lead",
                        "to wrong conclusions."), paste0(dQuote(resp_vars),
                                                         collapse = ", "),
                     applicability_problem = TRUE)
      }
      util_warning(
        c("%s is not a metric variable. Ordinal variables may in some cases",
          "still be interpretable with the LOESS plots, but be aware that",
          "distances are meaningless."), paste0(dQuote(resp_vars), collapse =
                                                  ", "),
        applicability_problem = TRUE)
      ds2[[resp_vars]] <- suppressWarnings(as.integer(ds2[[resp_vars]]))
    # } else {
    #   # too complicated, unclear concept. cancelled so far.
    #   # dichotomize per week
    #   new_data <-
    #     apply(unique(ds2[, co_vars, drop = FALSE]), 1, function(covarval) {
    #       ds2 <- ds2[apply((ds2[, co_vars, drop = TRUE]), 1, function(x)
    #          all(x == covarval)), , drop = FALSE]
    #       res_aux <- lapply(unique(ds2[[group_vars]]), function(aux) {
    #         times <- seq(trunc(min(ds2[[time_vars]]), units = "days"),
    #                   trunc(max(ds2[[time_vars]]), units = "days") + 1, "1 w")
    #         fractions <- sapply(seq_along(times), function(.i) {
    #           if (.i == length(times))
    #             return(NA)
    #           .time <- times[[.i]]
    #           .time2 <- times[[.i + 1]]
    #           cntr <- sum(ds2[[group_vars]] == aux & ds2[[resp_vars]] ==
    #                                                       reference_category &
    #                         trunc(ds2[[time_vars]], "days") >= .time &
    #                                  trunc(ds2[[time_vars]], "days") < .time2)
    #           dnom <- cntr + sum(ds2[[group_vars]] == aux & ds2[[resp_vars]]
    #                                                    != reference_category &
    #                         trunc(ds2[[time_vars]], "days") >= .time &
    #                                  trunc(ds2[[time_vars]], "days") < .time2)
    #           if (dnom == 0)
    #             return(NA)
    #           else
    #             return(cntr/dnom)
    #         })
    #         times <- times[!is.na(fractions)]
    #         fractions <- fractions[!is.na(fractions)]
    #         covarval <- as.data.frame(t(covarval))
    #         rownames(covarval) <- NULL
    #         if (length(times) == 0)
    #           return(squareControl:::util.empty.df(c("times", "fractions",
    #                                                                   "aux")))
    #         res_times <- as.data.frame(cbind(data.frame(times, fractions,
    #                                                           aux), covarval))
    #         return(res_times)
    #       })
    #       return(res_aux)
    #     })
    # new_data <- do.call(dplyr::bind_rows, new_data)
    # colnames(new_data) <- c(time_vars, resp_vars, group_vars, co_vars)
    # ds2 <- new_data
    # }
  }

  # order data
  ds2 <- ds2[order(ds2[[group_vars]], ds2[[time_vars]]), ]

  # determine max of smoothing parameter
  max_smooth <- round(max(1 / log10(suppressWarnings(
    as.numeric(table(ds2[[group_vars]]))))), 2)
  max_smooth <- max(max_smooth, 0.3, na.rm = TRUE)

  # Modelling

  # build model formula
  if (length(co_vars) > 0) {
    fmla <- as.formula(paste0(paste0(resp_vars, "~"), paste0(co_vars,
                                                             collapse = " + ")))
  } else {
    fmla <- as.formula(paste0(paste0(resp_vars, "~"), 1))
  }

  # Linear model
  # Adjustment for covariables
  lmfit1 <- lm(fmla, data = ds2)

  # use residuals + intercept
  ds2$Residuals[!is.na(ds2[[resp_vars]])] <- lmfit1$residuals +
    lmfit1$coefficients[1]

  # Memory consumption
  rm(lmfit1)

  # global loess
  loess_fit <- loess(Residuals ~
                       suppressWarnings(as.numeric(ds2[[time_vars]])), span =
                       max_smooth, ds2)
  fit_df <- data.frame(TIME = ds2[[time_vars]], FITTED_VALUE = loess_fit$fitted,
                       GROUP = "GLOBAL")

  GROUP <- ds2[[group_vars]]
  grouped_ds2 <- split(ds2, GROUP)

  processed_grouped_ds2 <- lapply(grouped_ds2, function(data_i) {
    # i-th smoothing parameter
    max_smooth <- round(1 / log10(length(data_i$Residuals)), 2)
    max_smooth <- max(max_smooth, 0.3, na.rm = TRUE)
    fit_i <- loess(Residuals ~
                     suppressWarnings(as.numeric(data_i[[time_vars]])), span =
                     max_smooth, data = data_i)
    data.frame(TIME = data_i[[time_vars]], FITTED_VALUE = fit_i$fitted,
               GROUP = data_i[[group_vars]],
               row.names =
                 rownames(data_i)) # https://stackoverflow.com/a/39838759
  })

  fit_groupdf <- unsplit(processed_grouped_ds2, GROUP)

  fit_df <- dplyr::bind_rows(fit_df, fit_groupdf)

  #  browser()

  # Prediction
  # Try this approach:
  # (1) predict the smoothed values one the complete data (A)

  # pred <- predict(loess_fit, ds2)

  # (2) reduce the data frame to unique time points (B)
  #     - the subsequent loess plots uses one value for each coordinate at x
  #       anyways

  ds3 <- ds2[, c(time_vars, "Residuals")]

  tp <- ds2[[time_vars]]

  period <- (max(tp) - min(tp)) / resolution
  secs <- suppressWarnings(as.integer(as.double(period, units = "secs")))
  if (secs <= 0) {
    secs <- 1
  }

  if (secs / 3600 / 24 / 365 > 1) {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f years",
                                                               round(secs /
                                                                       3600 /
                                                                       24 /
                                                                       365))))
  } else if (secs / 3600 / 24 / 31 > 1) {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f months",
                                                               round(secs /
                                                                       3600 /
                                                                       24 /
                                                                       31))))
  } else if (secs / 3600 / 24 / 7 > 1) {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f weeks",
                                                               round(secs /
                                                                       3600 /
                                                                       24 /
                                                                       7))))
  } else if (secs / 3600 / 24 > 1) {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f days",
                                                               round(secs /
                                                                       3600 /
                                                                       24))))
  } else if (secs / 3600 > 1) {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f hours",
                                                               round(secs /
                                                                       3600))))
  } else if (secs / 60 > 1) {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f minutes",
                                                               round(secs /
                                                                       60))))
  } else {
    ds3[[time_vars]] <- as.character(lubridate::round_date(ds2[[time_vars]],
                                                           unit =
                                                             sprintf(
                                                               "%f seconds",
                                                               round(secs))))
  }
  #  ds3[[time_vars]] <- as.character(round.POSIXt(ds2[[time_vars]],
  #                                                              units = "day"))

  ds3_reduced <- aggregate(
    as.formula(sprintf(". ~ %s", time_vars)),
    ds3,
    dplyr::first,
    simplify = FALSE,
    drop = FALSE
  )

  ds3_reduced[[time_vars]] <-
    suppressWarnings(as.POSIXct(ds3_reduced[[time_vars]]))
  ds3_reduced[["Residuals"]] <- unlist(ds3_reduced[["Residuals"]])

  loess_fit_reduced <- loess(Residuals ~
                               suppressWarnings(
                                 as.numeric(ds3_reduced[[time_vars]])),
                             span = max_smooth, ds3_reduced)

  pred_reduced <- predict(loess_fit_reduced, ds3_reduced, se = TRUE)

  # timepoints <- seq(min(ds2[[time_vars]]), max(ds2[[time_vars]]),
  #                                                             length.out = 80)
  # pred_reduced <- predict(loess_fit_reduced, timepoints, se = TRUE)
  #
  # ds_new <- data.frame(
  #   t = timepoints,
  #   Residuals = ,
  #   ResPerTime = ,
  #   stringsAsFactors = FALSE
  # )
  # colnames(ds_new) <- c(time_vars, Residuals, ResPerTime)
  #
  #


  # (3) merge B to A to multiply rows according A

  pred_reduced_df <- data.frame(
    fit = pred_reduced$fit,
    se.fit = pred_reduced$se.fit,
    residual.scale = pred_reduced$residual.scale,
    df = pred_reduced$df,
    ROUND_TIME = ds3_reduced[[time_vars]]
  )

  pred_reduced_map_df <- data.frame(
    TIME = ds2[[time_vars]],
    ROUND_TIME = suppressWarnings(as.POSIXct(ds3[[time_vars]])),
    stringsAsFactors = FALSE
  )

  pred_df <- merge.data.frame(pred_reduced_map_df, pred_reduced_df,
                              by = "ROUND_TIME")

  #
  # If this is not working do the predict also on a dataframe with unique
  #   timepoints
  # If this is not working, we will use a ggplot-approach:
  # (1) gl1 <- ggplot(df, aes(x = x, y = y)) + geom_smooth(aes(color = z),
  #                   method = "loess")
  # (2) gl1data <- ggplot2::ggplot_build(gl1)
  # (3) gl1data$data is reduced to 80 points at x-axis
  #
  # If this is also not working, we will use another smoothing technique such as
  #   GAM

  # 99% CI + residuals
  pred_df$lwl <- pred_df$fit - 2.575 * pred_df$se.fit - pred_df$residual.scale
  pred_df$upl <- pred_df$fit + 2.575 * pred_df$se.fit + pred_df$residual.scale

  fit_df <- merge(fit_df, pred_df, by = "TIME", all.x = TRUE)

  # statistical threshold (tbd)

  if (length(co_vars) > 0) {
    if (length(co_vars) < 10) {
      subtitle <- sprintf("adjusted for %s", paste0(co_vars, collapse = ", "))
    } else {
      subtitle <- sprintf("adjusted for %d variables", length(co_vars))
    }
  } else {
    subtitle <- ""
  }

  # Facet-Grids for categorical variable (observer/device)
  fit_df <- fit_df[fit_df$GROUP != "GLOBAL", ]

  # fit_df <- fit_df[unique(round(seq.int(1, nrow(fit_df), length.out =
  #                      10000))), , TRUE]
  fit_df <- fit_df[!duplicated(fit_df), , FALSE]

  if (missing(plot_data_time)) {
    dots_per_group <- table(fit_df$GROUP)
    if (max(dots_per_group) > 4000) {
      util_warning(
        c("Argument %s was not set. Based on the maximum of observations of %d",
          "for group %s > 4000, marks for timepoints featuring data will be",
          "turned off."),
        sQuote("plot_data_time"),
        max(dots_per_group),
        dQuote(names(dots_per_group))[which.max(dots_per_group)],
        applicability_problem = TRUE
      )
      plot_data_time <- FALSE
    } else {
      plot_data_time <- TRUE
    }
  } else {
    if (length(plot_data_time) != 1 ||
        is.na(plot_data_time) ||
        !is.logical(plot_data_time)) {
          util_error("Argument %s must be a sclar logical value.",
                     dQuote("plot_data_time"),
                     applicability_problem = TRUE)
        }
  }

  if (plot_data_time) {
    geom_dp <- geom_point(shape = "|")
  } else {
    geom_dp <- NULL
  }


  # browser()
  if (length(unique(GROUP)) <= 10) {
    hex_code <- c(
      "#000000", "#B0B0B0", "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
    )

    names(hex_code) <- as.character(unique(GROUP))
  } else {
    hex_code <- NULL
  }

  p1 <- ggplot(fit_df, aes_(x = ~TIME, y = ~FITTED_VALUE, color = ~GROUP)) + {
      if (!is.null(hex_code)) {
        scale_color_manual(values = hex_code)
      }
    } +
    xlab("") +
    ylab("") +
    geom_dp +
    geom_line() +
    facet_wrap(~GROUP, ncol = 2) +
    do.call(geom_line, c(list(aes_(y = ~lwl, group = NA)), se_line)) +
    do.call(geom_line, c(list(aes_(y = ~upl, group = NA)), se_line)) +
    theme_minimal() +
    ggtitle(paste("Effects of ", group_vars, " in ", resp_vars), subtitle) +
    theme(legend.title = element_blank())



  # combined plot
  p2 <- ggplot(fit_df, aes_(x = ~TIME, y = ~FITTED_VALUE, group = ~GROUP,
                            color = ~GROUP)) + {
      if (!is.null(hex_code)) {
        scale_color_manual(values = hex_code)
      }
    } +
    xlab("") +
    ylab("") +
    geom_dp +
    geom_line() +
    do.call(geom_line, c(list(aes_(y = ~lwl, group = NA)), se_line)) +
    do.call(geom_line, c(list(aes_(y = ~upl, group = NA)), se_line)) +
    theme_minimal() +
    ggtitle(paste("Effects of ", group_vars, " in ", resp_vars), subtitle) +
    theme(legend.title = element_blank())

  p1 <- util_set_size(p1,
                      width_em = 45,
                      height_em = (length(unique(fit_df$GROUP))) * 15 / 2)
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
    util_warning("Unknown %s: %s -- will switch to default value AUTO.",
               dQuote("plot_format"), dQuote(plot_format),
               applicability_problem = TRUE)
  }
  if (length(unique(GROUP)) < 15) {
    selection <- "Loess_fits_combined"
  } else {
    selection <- "Loess_fits_facets"
  }
  pl <- pl[selection]
  names(pl) <- resp_vars
  return(list(SummaryPlotList = pl))

}
