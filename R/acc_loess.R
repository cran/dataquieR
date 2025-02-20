#' Smoothes and plots adjusted longitudinal measurements and longitudinal trends
#' from logistic regression models
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
#' @inheritParams .template_function_indicator
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
#' @importFrom stats as.formula lm loess lowess predict na.omit glm
#'                   binomial poisson sd cov var runif
#'
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_loess.html
#' )
acc_loess <- function(resp_vars,
                      group_vars = NULL,
                      time_vars,
                      co_vars = NULL,
                      study_data,
                      label_col = VAR_NAMES,
                      item_level = "item_level",
                      min_obs_in_subgroup = 30,
                      resolution = 80,
                      comparison_lines = list(type = c("mean/sd", "quartiles"),
                                              color = "grey30",
                                              linetype = 2,
                                              sd_factor = 0.5),
                      mark_time_points =
                        getOption("dataquieR.acc_loess.mark_time_points",
                                  dataquieR.acc_loess.mark_time_points_default),
                      plot_observations =
                        getOption("dataquieR.acc_loess.plot_observations",
                                  dataquieR.acc_loess.plot_observations_default),
                      plot_format =
                        getOption("dataquieR.acc_loess.plot_format",
                                  dataquieR.acc_loess.plot_format_default),
                      meta_data = item_level,
                      meta_data_v2,
                      n_group_max = getOption("dataquieR.max_group_var_levels_in_plot", dataquieR.max_group_var_levels_in_plot_default),
                      enable_GAM = getOption("dataquieR.GAM_for_LOESS",
                                             dataquieR.GAM_for_LOESS.default),
                      exclude_constant_subgroups = getOption("dataquieR.acc_loess.exclude_constant_subgroups",
                                                             dataquieR.acc_loess.exclude_constant_subgroups.default)) {
  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()

  label_col <- attr(prep_get_labels("",
                                    item_level = meta_data,
                                    label_class = "LONG",
                                    label_col = label_col),
                    "label_col")

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE)

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

  # check that other arguments are specified correctly
  util_expect_scalar(
    min_obs_in_subgroup,
    check_type = util_is_numeric_in(min = 1, whole_num = TRUE, finite = TRUE),
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
  util_expect_scalar(
    resolution,
    check_type = util_is_numeric_in(min = 3, whole_num = TRUE, finite = TRUE),
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
  util_expect_scalar(n_group_max,
                     check_type = util_is_numeric_in(min = 1, whole_num = TRUE))
  util_expect_scalar(enable_GAM, check_type = is.logical)
  util_expect_scalar(exclude_constant_subgroups, check_type = is.logical)
  util_expect_scalar(plot_format,
                     check_type = function(x) {
                       is.character(x) &&
                         any(grepl(x,
                                   c("AUTO", "COMBINED", "FACETS", "BOTH",
                                     "auto", "combined", "facets", "both" ),
                                   fixed = TRUE)) })

  # check data properties to choose a suitable method
  ds1 <- ds1[, c(resp_vars, time_vars, group_vars, co_vars)]
  ds1 <- ds1[complete.cases(ds1), ]
  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.",
               applicability_problem = TRUE)
  }
  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  scl <- meta_data[[SCALE_LEVEL]][meta_data[[label_col]] == resp_vars]

  # generate a LOESS plot using a suitable method ------------------------------
  if (var_prop$NDistinct > 9 &&
      scl %in% c(SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL)) {
    SummaryPlotList_from_util <-
      util_acc_loess_continuous(resp_vars = resp_vars,
                                group_vars = group_vars,
                                time_vars = time_vars,
                                co_vars = co_vars,
                                study_data = study_data,
                                meta_data = meta_data,
                                label_col = label_col,
                                min_obs_in_subgroup = min_obs_in_subgroup,
                                resolution = resolution,
                                comparison_lines = comparison_lines,
                                mark_time_points = mark_time_points,
                                plot_observations = plot_observations,
                                plot_format = plot_format,
                                n_group_max = n_group_max,
                                enable_GAM = enable_GAM,
                                exclude_constant_subgroups =
                                  exclude_constant_subgroups)
  } else if ((var_prop$NDistinct < 10 &&
              scl %in% c(SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL)) ||
             scl %in% c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$ORDINAL)) {
    SummaryPlotList_from_util <-
      util_acc_loess_bin(resp_vars = resp_vars,
                         group_vars = group_vars,
                         time_vars = time_vars,
                         co_vars = co_vars,
                         study_data = study_data,
                         meta_data = meta_data,
                         label_col = label_col,
                         min_obs_in_subgroup = min_obs_in_subgroup,
                         resolution = resolution,
                         plot_format = plot_format,
                         n_group_max = n_group_max,
                         enable_GAM = enable_GAM,
                         exclude_constant_subgroups =
                           exclude_constant_subgroups)
  } else {
    util_error("Variable '%s' has a disallowed scale level (%s)",
               dQuote(resp_vars),
               dQuote(tolower(trimws(scl))),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  return(SummaryPlotList = SummaryPlotList_from_util)
}

