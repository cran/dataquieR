#' Calculate and plot Mahalanobis distances
#'
#' @description
#' A standard tool to detect multivariate outliers is the Mahalanobis distance.
#' This approach is very helpful for the interpretation of the plausibility of a
#' measurement given the value of another.
#' In this approach the Mahalanobis distance is used as a univariate measure
#' itself. We apply the same rules for the identification of outliers as in
#' univariate outliers:
#' - the classical approach from Tukey: \eqn{1.5 * IQR} from the
#'   1st (\eqn{Q_{25}}) or 3rd (\eqn{Q_{75}}) quartile.
#' - the \eqn{6* \sigma} approach, i.e. any measurement of the Mahalanobis
#'   distance not in the interval of \eqn{\bar{x} \pm 3*\sigma} is considered an
#'   outlier.
#' - the approach from Hubert for skewed distributions which is embedded in the
#'   R package \pkg{robustbase}
#' - a completely heuristic approach named \eqn{\sigma}-gap.
#'
#' For further details, please see the vignette for univariate outlier.
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to variables of type float
#' - Remove missing codes from the study data (if defined in the metadata)
#' - The covariance matrix is estimated for all variables from `variable_group`
#' - The Mahalanobis distance of each observation is calculated
#'   \eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
#' - The four rules mentioned above are applied on this distance for
#'   each observation in the study data
#' - An output data frame is generated that flags each outlier
#' - A parallel coordinate plot indicates respective outliers
#'
#' List function.
#'
#' @param variable_group [variable list] the names of the continuous
#'                                       measurement variables building
#'                                       a group, for that multivariate outliers
#'                                       make sense.
#' @param id_vars [variable] optional, an ID variable of
#'                        the study data. If not specified row numbers are used.
#' @param n_rules [numeric] from=1 to=4. the no. of rules that must be violated
#'                                       to classify as outlier
#' @param max_non_outliers_plot [integer] from=0. Maximum number of non-outlier
#'                                                points to be plot. If more
#'                                                points exist, a subsample will
#'                                                be plotted only. Note, that
#'                                                sampling is not deterministic.
#' @param criteria [set] tukey | sixsigma | hubert | sigmagap. a vector with
#'                       methods to be used for detecting outliers.
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `SummaryPlot`: [ggplot2] outlier plot
#'   - `FlaggedStudyData` [data.frame] contains the original data frame with
#'                                     the additional columns `tukey`,
#'                                     `sixsigma`,
#'                                     `hubert`, and `sigmagap`. Every
#'                                     observation
#'                                     is coded 0 if no outlier was detected in
#'                                     the respective column and 1 if an
#'                                     outlier was detected. This can be used
#'                                     to exclude observations with outliers.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_path  scale_color_manual geom_point
#'                     discrete_scale theme_minimal scale_alpha_manual
#'
#' @importFrom stats mahalanobis
#' @importFrom rlang .data
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html
#' )
acc_multivariate_outlier <- function(variable_group = NULL, id_vars = NULL,
                                     label_col,
                                     n_rules = 4,
                                     max_non_outliers_plot = 10000,
                                     criteria = c("tukey", "sixsigma",
                                                  "hubert", "sigmagap"),
                                     study_data, meta_data) { # TODO: see univ.out to select criteria to use

  # preps ----------------------------------------------------------------------
  if (length(unique(criteria)) < 1 ||
      length(unique(criteria)) >
      length(eval(formals(acc_multivariate_outlier)$criteria)) ||
      !all(criteria %in% eval(formals(acc_multivariate_outlier)$criteria))) {
    util_warning(c("The formal criteria must have > 0 and < %d entries.",
                   "Allowed values are %s.",
                   "I was called with %s, falling back to default %s."),
                 length(eval(formals(acc_multivariate_outlier)$criteria)),
                 paste(dQuote(eval(formals(acc_multivariate_outlier)$criteria)),
                       collapse = ", "),
                 paste(dQuote(unique(criteria)), collapse = ", "),
                 paste(dQuote(eval(formals(acc_multivariate_outlier)$criteria)),
                       collapse = ", "))
    criteria <- eval(formals(acc_multivariate_outlier)$criteria)
  }

  if (length(n_rules) != 1 || !is.numeric(n_rules) ||
      !all(util_is_integer(n_rules)) ||
      !(n_rules %in% seq_len(length(unique(criteria))))) {
    util_warning(
      "The formal n_rules is not an integer between 1 and %d, default (%d) is used.",
      length(unique(criteria)),
      min(eval(formals(acc_multivariate_outlier)$n_rules),
          length(unique(criteria))),
      applicability_problem = TRUE)
    n_rules <- min(eval(formals(acc_multivariate_outlier)$n_rules),
                   length(unique(criteria)))

  }
    # rules in 1:4?
  if (n_rules != 4) {
    if (!(n_rules %in% 1:4)) {
      util_warning(
        "The formal n_rules is not an integer of 1 to 4, default (4) is used. ",
        applicability_problem = TRUE)
      n_rules <- 4
    }
  }

  if (length(max_non_outliers_plot) != 1 ||
      !is.numeric(max_non_outliers_plot) ||
      !all(util_is_integer(max_non_outliers_plot)) ||
      (max_non_outliers_plot < 0)) {
    util_warning(
      c("The formal max_non_outliers_plot is not an integer >= 0,",
        "default (%d) is used."),
      formals(acc_multivariate_outlier)$max_non_outliers_plot,
      applicability_problem = TRUE)
    max_non_outliers_plot <-
      formals(acc_multivariate_outlier)$max_non_outliers_plot
  }

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  util_correct_variable_use("variable_group",
    allow_more_than_one = TRUE,
    allow_any_obs_na = TRUE,
    need_type = "integer | float"
  )

  if (length(variable_group) == 1) {
    util_error("Need at least two variables for multivariate outliers.",
               applicability_problem = TRUE)
  }

  # check correct use of ID-variable if specified
  util_correct_variable_use("id_vars",
    allow_any_obs_na = TRUE,
    allow_null = TRUE,
    need_type = "string | integer"
  )

  # no use of id_vars?
  if (is.null(id_vars)) {
    ds1$dq_id <- rownames(ds1)
    id_vars <- "dq_id"
    util_warning("As no ID-var has been specified the rownumbers will be used.",
                 applicability_problem = TRUE)
  }

  if (length(id_vars) > 1) {
    id_vars <- id_vars[1]
  }

  # missing data in any of the variables?
  vars <- c(variable_group, id_vars)
  vars <- vars[!is.na(vars)]

  n_prior <- dim(ds1)[1]
  ds1plot <- ds1[rowSums(is.na(ds1[, vars, drop = FALSE])) == 0, ,
    drop = FALSE
  ]
  n_post <- dim(ds1plot)[1]

  if (n_post == 0) {
    util_error("No samples with without missing values in one of %s. Aborting.",
               paste0(dQuote(vars), collapse = ", "),
               applicability_problem = FALSE)
  }

  if (n_post < n_prior) {
    util_warning(paste0(
      "Due to missing values in ", paste0(variable_group, collapse = ", "),
      " or ", id_vars, " N=", n_prior - n_post,
      " observations were excluded."
    ), applicability_problem = FALSE)
  }

  variable_group <-
    util_no_value_labels(
      resp_vars = variable_group,
      meta_data = meta_data,
      label_col = label_col,
      warn = TRUE,
      stop = TRUE
    )

  if (length(variable_group) < 2) {
    util_error(
      "Fewer than two variables left, no multivariate analysis possible.")
  }

  # Mahalanobis ----------------------------------------------------------------

  # Estimate covariance of response variables
  Sx <- cov(ds1plot[, variable_group, drop = FALSE])

  # Calculation of the Mahalanobis distance
  ds1plot$MD <- mahalanobis(ds1plot[, variable_group], colMeans(ds1plot[, variable_group,
                                                             drop = FALSE]), Sx)

  # browser()

  # outlier identification
  # Initialize with NA
  ds1plot$tukey <- NA
  ds1plot$sixsigma <- NA
  ds1plot$hubert <- NA
  ds1plot$sigmagap <- NA
  # View(ds1)
  # apply outlier functions to plot-df
  # after export/final built  correct the call of the utility functions
  ds1plot$tukey <- util_tukey(ds1plot$MD)
  ds1plot$sixsigma <- util_sixsigma(ds1plot$MD)
  ds1plot$hubert <- util_hubert(ds1plot$MD)
  ds1plot$sigmagap <- util_sigmagap(ds1plot$MD)

  # calculate summary of all outlier definitions
  ds1plot$Rules <-
    apply(ds1plot[, criteria, drop = FALSE], 1, sum)

  n_non_ol <- sum(ds1plot$Rules == 0)

  ### remove non-outliers, if too many
  if (max_non_outliers_plot < n_non_ol) {

    dsi_non_ol <- ds1plot[ds1plot$Rules == 0, , FALSE]
    dsi_ol <- ds1plot[ds1plot$Rules > 0, , FALSE]

    subsel_non_ol <- sample(seq_len(nrow(dsi_non_ol)),
                            size =
                              min(max_non_outliers_plot, nrow(dsi_non_ol)))

    ds1plot <- rbind.data.frame(dsi_non_ol[subsel_non_ol, , FALSE], dsi_ol)

    util_warning(
      c("For %s, %d from %d non-outlier data values were",
        "sampled to avoid large plots."),
      sQuote(sprintf("acc_multivariate_outlier(%s)",
                     paste0(dQuote(variable_group), collapse = ", "))),
      max_non_outliers_plot,
      n_non_ol,
      applicability_problem = FALSE
    )
  }
  ###

  # create summary table
  # outlier deviating according to all rules?
  n_devs <- ifelse(ds1plot$Rules >= n_rules, 1, 0)
  grading <- max(n_devs, na.rm = TRUE)

  st1 <- data.frame(
    Variables = paste0(variable_group, collapse = " | "),
    Tukey = sum(ds1plot$tukey),
    SixSigma = sum(ds1plot$sixsigma),
    Hubert = sum(ds1plot$hubert),
    SigmaGap = sum(ds1plot$sigmagap),
    GRADING = grading
  )


  # reshape wide to long
  ds2plot <-
    melt(ds1plot[, c(variable_group, id_vars, "Rules")], measure.vars = variable_group, id.vars =
           c(id_vars, "Rules"))

  # use neamed color vector
  disc_cols <- c("#2166AC", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000")
  names(disc_cols) <- c(0:4)

  # use ID/Rules as factor
  ds2plot[[id_vars]] <- factor(ds2plot[[id_vars]])
  ds2plot$Rules <- factor(ds2plot$Rules, ordered = TRUE)


  # transparency
  .a <- c(0.2, 0.7, 0.8, 0.9, 1)
  .a <- .a[seq_along(levels(ds2plot$Rules))]
  names(.a) <- levels(ds2plot$Rules)

  # size
  .s <- c(0.05, 0.2, 0.3, 0.4, 0.5)
  .s <- .s[seq_along(levels(ds2plot$Rules))]
  names(.s) <- levels(ds2plot$Rules)

  # browser()

  # PLOT
  p <- ggplot(ds2plot, aes(x = variable, y = value, colour = Rules,
                           group = .data[[id_vars]])) +
    geom_path(aes(alpha = Rules, linewidth = Rules), position = "identity") +
    scale_color_manual(values = disc_cols) +
    geom_point(aes(alpha = Rules)) +
    scale_alpha_manual(values = .a) +
    discrete_scale("linewidth", "outlier_rules_scale",
                            function(n) {
                              c(0.05, 0.2, 0.3, 0.4, 0.5)
                            }) +
    theme_minimal()

  return(list(FlaggedStudyData = ds1plot,
              SummaryTable = st1,  # TODO: VariableGroupTable, maybe other functions, too?
              SummaryPlot =
                util_set_size(p,
                              width_em = 25 +
                                1.2 * length(variable_group),
                              height_em = 25
                              )))
}
