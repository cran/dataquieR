#' Calculate and plot Mahalanobis distances for social science indices
#'
#' @description
#' A standard tool to calculate Mahalanobis distance.
#' In this approach the Mahalanobis distance is calculated for ordinal
#' variables (treated as continuous) to identify inattentive responses.
#' It calculates the distance for each observational unit from the sample mean.
#' The greater the distance, the atypical the responses.
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to variables of type integer
#' - Remove missing codes from the study data (if defined in the metadata)
#' - The covariance matrix is estimated for all variables from `variable_group`
#' - The Mahalanobis distance of each observation is calculated
#'   \eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
#' - The default to consider a value an outlier is "use the 0.975 quantile
#'    of a chi-square distribution with p degrees of freedom" (Mayrhofer and Filzmoser, 2023)
#' List function.
#'
#' @inheritParams .template_function_indicator
#'
#' @param variable_group [variable list] the names of the continuous
#'                                       measurement variables building
#'                                       a group, for that multivariate outliers
#'                                       make sense.
#' @param mahalanobis_threshold [numeric] TODO: ES
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `SummaryPlot`: [ggplot2::ggplot2] outlier plot
#'   - `FlaggedStudyData` [data.frame] contains the original data frame with
#'                                     the additional columns `tukey`,
#'                                     `3SD`,
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
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html
#' )
acc_mahalanobis <- function(variable_group = NULL,
                                     label_col = VAR_NAMES,
                                     study_data,
                                     item_level = "item_level",
                                     meta_data = item_level,
                                     meta_data_v2,
                                     mahalanobis_threshold =
                              suppressWarnings(as.numeric(getOption("dataquieR.MAHALANOBIS_THRESHOLD",
                                dataquieR.MAHALANOBIS_THRESHOLD_default)))) {
  # preps ----------------------------------------------------------------------
  mahalanobis_distance_check = !util_empty(mahalanobis_threshold)
  if (.called_in_pipeline && !mahalanobis_distance_check) {
    util_error("No Mahalanobis distance requested",
               intrinsic_applicability_problem = TRUE)
  }
  if (length(mahalanobis_threshold) != 1 ||
      !is.numeric(mahalanobis_threshold) ||
      !is.finite(mahalanobis_threshold)) {
    util_message(
      c("The mahalanobis_threshold argument is not correct",
        "default (%g) is used."),
      dataquieR.MAHALANOBIS_THRESHOLD_default,
      applicability_problem = TRUE)
    mahalanobis_threshold <- dataquieR.MAHALANOBIS_THRESHOLD_default
  }

  util_maybe_load_meta_data_v2()

  if (missing(label_col)) {
    orig_label_col <- rlang::missing_arg()
  } else {
    orig_label_col <- force(label_col)
  }

  label_col <- attr(prep_get_labels("",
                                    item_level = meta_data,
                                    label_class = "SHORT",
                                    label_col = label_col),
                    "label_col")

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  util_correct_variable_use("variable_group",
                            allow_more_than_one = TRUE,
                            allow_any_obs_na = TRUE,
                            need_type = "integer | float",
                            need_scale = "interval | ratio | ordinal" #ES: should I leave only ordinal?
  )

  if (length(variable_group) == 1) {
    util_error("Need at least two variables for Mahanobis distance.",
               applicability_problem = TRUE)
  }

  # missing data in any of the variables?
  vars <- variable_group[!is.na(variable_group)]
  #Filter to only keep the columns in the variable_group
  ds1_group <- ds1[, vars, drop = FALSE]

  n_prior <- dim(ds1_group)[1]
  ds1plot <- ds1_group[rowSums(is.na(ds1_group[, vars, drop = FALSE])) == 0, ,
                 drop = FALSE]
  rm(ds1_group)
  n_post <- dim(ds1plot)[1]

  if (n_post == 0) {
    util_error("No samples with without missing values in one of %s. Aborting.",
               paste0(dQuote(vars), collapse = ", "),
               applicability_problem = FALSE)
  }

  if (n_post < n_prior) {
    util_message(paste0(
      "Due to missing values",
      " N=", n_prior - n_post,
      " observational units were excluded."
    ), applicability_problem = FALSE)
  }

  if (length(variable_group) < 2) {
    util_error(
      "Fewer than two variables left, no Mahalanobis distance calculation possible.",
      applicability_problem = TRUE)
  }

  # Mahalanobis ----------------------------------------------------------------
  # no. variables used to calculate the MD
  degree_freedom <- ncol(ds1plot[, variable_group]) #ES: check is this is actually the no. vars

  # Estimate covariance of response variables
  Sx <- cov(ds1plot[, variable_group, drop = FALSE])

  # Calculation of the Mahalanobis distance
  ds1plot$MD <-
    mahalanobis(ds1plot[, variable_group],
                colMeans(ds1plot[, variable_group,drop = FALSE]),
                Sx)

  # Threshold to identify multivariate outliers (argument mahalanobis_threshold)
  # is 0.975 quantile of a chi-square distribution
  # with p degrees of freedom, where p = variables (Mayrhofer and Filzmoser, 2023)
  MD_outliers_threshold <- stats::qchisq(mahalanobis_threshold, df = degree_freedom)

  #ES: This differs from careless function mahad that calculates MD even if there are missing values.

  #create a column with outliers
  ds1plot$MD_outliers <- NA
  ds1plot$MD_outliers <- ifelse(ds1plot$MD > MD_outliers_threshold, 1, 0)

  n_non_ol <- sum(ds1plot$MD_outliers == 0)
  n_devs <- sum(ds1plot$MD_outliers == 1)

  # create summary table
#  grading <- max(n_devs, na.rm = TRUE)

  st1 <- data.frame(Variables = paste0(variable_group, collapse = " | "))
  st1$"MD_outliers (N)" <- n_devs
  st1$"MD_outliers (%)" <- round(n_devs/length(ds1plot$MD)*100, digits = 2)
  st1$"N" <- length(ds1plot$MD)
  st1$"observational_units_removed" <- n_prior - n_post
  st1$"mahalanobis_threshold" <- mahalanobis_threshold

  SummaryData <- st1

  SummaryTable <- st1
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (N)"] <- 'NUM_ssc_MD' # add indicator ssc_MD to DQ_OBS to make grading rules work, here.
  names(SummaryTable)[names(SummaryTable) == 'MD_outliers (%)'] <- 'PCT_ssc_MD'


  # new complete data with the MD (included exclude rows with missing values)
  # in that case MD is NA
  FlaggedStudyData <- merge(ds1, ds1plot,
                            by = intersect(colnames(ds1),
                                           colnames(ds1plot)),
                            all = TRUE)


  # Q-Q plot ----
  p_df <- length(ds1plot$MD)
  probabilities <- stats::ppoints(p_df)
  theoretical_quantiles <- stats::qchisq(probabilities, df = p_df)
  ds1plot <- ds1plot[order(ds1plot$MD), ]
  point_colors <- ifelse(ds1plot$MD_outliers == 1, "#7f0000", "#2166AC") #ES: maybe a better way?

  plot_data <- data.frame(
    Theoretical = theoretical_quantiles,
    Sample = ds1plot$MD
  )
  plot_title <- "Q-Q Plot: Sample Data vs. Chi-squared Distribution"
  x_label <- paste0("Theoretical Quantiles (Chi-squared, df =", p_df, ")")
  y_label <- "Sample Data (MD)"

  p1 <- util_create_lean_ggplot({
    ggplot(plot_data, aes(x = .data[["Theoretical"]],
                                y = .data[["Sample"]])) +
      geom_point(alpha = 0.6,
        color = point_colors) +
      labs(
        title = plot_title,
        x = x_label,
        y = y_label
      ) +
      theme_minimal()
  },
  plot_data = plot_data,
  point_colors = point_colors,
  plot_title = plot_title,
  x_label = x_label,
  y_label = y_label)


  p <- stats::qqplot(
    x = theoretical_quantiles,
    y = ds1plot$MD,
    main = "Q-Q Plot: Sample Data vs. Chi-squared Distribution",
    xlab = paste0("Theoretical Quantiles (Chi-squared, df =", p_df, ")"),
    ylab = "Sample Data (MD)",
    pch = 19,
    col = grDevices::adjustcolor(point_colors, alpha.f = 0.6)
  )

  # Information for sizing
  number_of_vars <- length(ds1plot$MD)
  min_value <- min(ds1plot$MD, na.rm = TRUE)
  max_value <- max(ds1plot$MD, na.rm = TRUE)
  range <- max_value - min_value
  no_char_y <- nchar(max_value)

  #Sizing information - a default
  attr(p, "sizing_hints") <- list(
    figure_type_id = "multivar_plot",
    number_of_vars = number_of_vars,
    range = range,
    no_char_y = no_char_y
  )
  rm(min_value, max_value, range, number_of_vars, no_char_y)


  #############################################
#ES: not sure what this does ??
  if (!missing(orig_label_col)) { # map back SummaryTable to requested label_col and tables to suitable table labels
    for (slot in c("SummaryTable", "SummaryData")) {
      tb <- get(slot)
      vs <- lapply(util_parse_assignments(tb$Variables,
                                          multi_variate_text = TRUE),
                   function(x) {
                     prep_map_labels(
                       x = x,
                       item_level = meta_data,
                       from = label_col,
                       to  = orig_label_col,
                       ifnotfound = x,
                       warn_ambiguous = FALSE
                     )
                   }
      )
      tb$Variables <-
        lapply(vs, paste0, collapse = sprintf(" %s ", SPLIT_CHAR))

      tb[["Variables"]] <-
        prep_map_labels(x = tb[["Variables"]],
                        item_level = meta_data,
                        to  = orig_label_col,
                        from = label_col,
                        ifnotfound = tb[["Variables"]],
                        warn_ambiguous = FALSE)
      assign(slot, tb)
    }
  }

  ##########################################
  return(list(FlaggedStudyData = FlaggedStudyData,
              SummaryTable = SummaryTable,
              SummaryData = SummaryData,
              SummaryPlot = util_set_size(p1,
                                          width_em = 25 +
                                            1.2 * length(variable_group),
                                          height_em = 25
              )))
}
