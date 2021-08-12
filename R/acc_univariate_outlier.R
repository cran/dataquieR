#' Function to identify univariate outliers by four different approaches
#'
#' @description
#' A classical but still popular approach to detect univariate outlier is the
#' boxplot method introduced by Tukey 1977. The boxplot is a simple graphical
#' tool to display information about continuous univariate data (e.g., median,
#' lower and upper quartile). Outliers are defined as values deviating more
#' than \eqn{1.5 \times IQR} from the 1st (Q25) or 3rd (Q75) quartile. The
#' strength of Tukey’s method is that it makes no distributional assumptions
#' and thus is also applicable to skewed or non mound-shaped data
#' Marsh and Seo, 2006. Nevertheless, this method tends to identify frequent
#' measurements which are falsely interpreted as true outliers.
#'
#' A somewhat more conservative approach in terms of symmetric and/or normal
#' distributions is the \eqn{6 * \sigma} approach, i.e. any measurement not in
#' the interval of \eqn{mean(x) +/- 3 * \sigma} is considered an outlier.
#'
#' Both methods mentioned above are not ideally suited to skewed distributions.
#' As many biomarkers such as laboratory measurements represent in skewed
#' distributions the methods above may be insufficient. The approach of Hubert
#' and Vandervieren 2008 adjusts the boxplot for the skewness of the
#' distribution. This approach is implemented in several R packages such as
#' [`robustbase::mc`] which is used in this implementation of [`dataquieR`].
#'
#' Another completely heuristic approach is also included to identify outliers.
#' The approach is based on the assumption that the distances between
#' measurements of the same underlying distribution should homogeneous. For
#' comprehension of this approach:
#'  - consider an ordered sequence of all measurements.
#'  - between these measurements all distances are calculated.
#'  - the occurrence of larger distances between two neighboring measurements
#'  may
#' than indicate a distortion of the data. For the heuristic definition of a
#' large distance \eqn{1 * \sigma} has been been chosen.
#'
#' Note, that the plots are not deterministic, because they use
#' [ggplot2::geom_jitter].
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Select all variables of type float in the study data
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Remove measurements deviating from limits defined in the metadata
#'  - Identify outlier according to the approaches of Tukey (Tukey 1977),
#'    SixSigma (-Bakar et al. 2006), Hubert (Hubert and Vandervieren 2008),
#'    and SigmaGap (heuristic)
#'  - A output data frame is generated which indicates the no. of possible
#'    outlier, the direction of deviations (to low, to high) for all methods
#'    and a summary score which sums up the deviations of the different rules
#'  - A scatter plot is generated for all examined variables, flagging
#'    observations according to the no. of violated rules (step 5).
#'
#' @param resp_vars [variable list] the name of the continuous measurement
#'                                  variable
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param exclude_roles [variable roles] a character (vector) of variable roles
#'                                       not included
#' @param n_rules [integer] from=1 to=4. the no. of rules that must be violated
#'        to flag a variable as containing outliers. The default is 4, i.e. all.
#' @param max_non_outliers_plot [integer] from=0. Maximum number of non-outlier
#'                                                points to be plot. If more
#'                                                points exist, a subsample will
#'                                                be plotted only. Note, that
#'                                                sampling is not deterministic.
#'
#' @seealso
#' - [acc_robust_univariate_outlier]
#' - [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_acc_impl_robust_univariate_outlier.html
#' )
#'
#' @importFrom reshape melt
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot geom_jitter position_jitter aes
#'                     scale_size_continuous scale_color_manual
#'                     facet_wrap vars scale_alpha theme_minimal
#' @importFrom stats median aggregate sd aggregate
#' @export
#'
#' @return a list with:
#'   - `SummaryTable`: [`data.frame`] with the columns
#'        `Variables`, `Mean`, `SD`, `Median`, `Skewness`, `Tukey (N)`,
#'        `6-Sigma (N)`, `Hubert (N)`, `Sigma-gap (N)`, `Most likely (N)`,
#'        `To low (N)`, `To high (N)` `Grading`
#'   - `SummaryPlotList`: [`ggplot`] univariate outlier plots
#'
#'
acc_univariate_outlier <- function(resp_vars = NULL, label_col, study_data,
                                   meta_data, exclude_roles, n_rules = 4,
                                   max_non_outliers_plot = 10000) {
  rvs <- resp_vars

  if (length(n_rules) != 1 || !is.numeric(n_rules) ||
      !all(util_is_integer(n_rules)) ||
      !(n_rules %in% 1:4)) {
    util_warning(
      "The formal n_rules is not an integer of 1 to 4, default (%d) is used.",
      formals(acc_univariate_outlier)$n_rules,
      applicability_problem = TRUE)
    n_rules <- formals(acc_univariate_outlier)$n_rules
  }

  if (length(max_non_outliers_plot) != 1 ||
      !is.numeric(max_non_outliers_plot) ||
      !all(util_is_integer(max_non_outliers_plot)) ||
      (max_non_outliers_plot < 0)) {
    util_warning(
      c("The formal max_non_outliers_plot is not an integer >= 0,",
        "default (%d) is used."),
      formals(acc_univariate_outlier)$max_non_outliers_plot,
      applicability_problem = TRUE)
    max_non_outliers_plot <-
      formals(acc_univariate_outlier)$max_non_outliers_plot
  }

  # Preps ----------------------------------------------------------------------
  # map meta to study
  util_prepare_dataframes()

  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    allow_any_obs_na = TRUE,
    need_type = "integer | float"
  )

  if (is.null(meta_data[[DATA_TYPE]]) ||
      any(is.na(meta_data[[DATA_TYPE]]))) {
    if (is.null(meta_data[[DATA_TYPE]])) {
      which_na <- rep(TRUE, nrow(meta_data))
    } else {
      which_na <- is.na(meta_data[[DATA_TYPE]])
    }
    meta_data[[DATA_TYPE]][which_na] <-
      prep_datatype_from_data(resp_vars = meta_data[[label_col]][which_na],
                              study_data = ds1)

    list_of_types <-
      paste(sQuote(meta_data[[label_col]][which_na]),
            '->',
            sQuote(meta_data[[DATA_TYPE]][which_na]))

    if (length(list_of_types) > 5) {
      dts <- "..."
    } else {
      dts <- NULL
    }
    list_of_types <- c(head(list_of_types, 5), dts)
    list_of_types <- paste0(list_of_types, collapse = ", ")
    util_warning(c(
      "No %s for all or some variables defined in the metadata.",
      "I guessed them based on data: %s"),
      dQuote(DATA_TYPE),
      list_of_types,
      applicability_problem = TRUE
    )
  }

  # no variables defined?
  if (length(rvs) == 0) {

    # which are float or integer?
    rvs <- meta_data[[label_col]][meta_data[[DATA_TYPE]] %in%
                                    c(DATA_TYPES$FLOAT, DATA_TYPES$INTEGER)]
    util_warning(paste0("The following variables: ",
                        paste0(rvs, collapse = ", "), " were selected."),
                 applicability_problem = TRUE)
    if (length(rvs) == 0) {
      util_error(paste0("No variables suitable data type defined."),
                 applicability_problem = TRUE)
    }

    rvs <- intersect(rvs, colnames(ds1))
  } else {
    # defined variables are not of type float/integer?
    isfloat <- meta_data[[DATA_TYPE]] %in%
      c(DATA_TYPES$FLOAT, DATA_TYPES$INTEGER)
    isrvs <-
      meta_data[[label_col]] %in% rvs

    if (!all(isfloat | !isrvs)) {
      rvs <- meta_data[[label_col]][isfloat & isrvs]
      if (!all(!isrvs | isfloat)) util_warning(paste0("Only: ",
                                      paste0(rvs, collapse = ", "),
                          " are defined to be of type float or integer."),
                          applicability_problem = TRUE)
    }
  }

  # should some variables not be considered?
  if (!missing(exclude_roles)) {
    if (!(all(exclude_roles %in% meta_data[[VARIABLE_ROLE]]))) {
      util_warning(
        "Specified VARIABLE_ROLE not in meta_data. No exclusion applied.",
        applicability_problem = TRUE)
    } else {
      which_vars_not <- meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in%
                                                 exclude_roles]
      if (length(intersect(rvs, which_vars_not)) > 0) {
        util_warning(paste0("Study variables: ",
                            paste(dQuote(intersect(rvs, which_vars_not)),
                                  collapse = ", "), " have been excluded."),
                     applicability_problem = TRUE)
      }
      rvs <- setdiff(rvs, which_vars_not)
    }
  }

  # remove rvs with non-matching data type
  whicharenum <- vapply(FUN.VALUE = logical(1), ds1[, rvs, drop = FALSE],
                        function(x) is.numeric(x))

  if (!all(whicharenum)) {
    util_warning(paste0(
      "Variables ", paste0(dQuote(rvs[!whicharenum]), collapse = ", "),
      " are not of type float or integer and will be removed",
      " from univariate outlier analysis."
    ), applicability_problem = TRUE)
    rvs <- rvs[whicharenum]
  }

  intcheck <- vapply(FUN.VALUE = logical(1), ds1[, rvs, drop = FALSE],
                     function(x) all(util_is_integer(x), na.rm = TRUE))

  if (any(intcheck)) {
    util_warning(paste0(
      "Variables: ", paste0(dQuote(rvs[intcheck]), collapse = ", "),
      " show integer values only, but will be nonetheless considered."
    ), applicability_problem = FALSE)
  }

  if (length(rvs) > 0) {
    rvs <-
      util_no_value_labels(
        resp_vars = rvs,
        meta_data = meta_data,
        label_col = label_col,
        warn = TRUE,
        stop = TRUE
      )
  }

  # Label assignment -----------------------------------------------------------
  # temporary study data
  ds1_ll <- ds1

  #############
  # Results   #
  #############
  # Boxplot
  # ds2 <- melt(ds1_ll[, c(rvs, group_vars)], measure.vars = rvs)

  if (length(rvs) == 0) {
    util_error("No suitable response variables left.",
               applicability_problem = TRUE)
  } else if (length(rvs) == 1) {
    ds2 <- ds1_ll[, rvs, drop = FALSE]
    ds2$variable <- rvs
    ds2 <- ds2[, c(2, 1)]
    names(ds2) <- c("variable", "value")
  } else {
    ds2 <- melt(ds1_ll[, c(rvs)], measure.vars = rvs)
  }

  ds2$value <- as.numeric(ds2$value)

  # remove NAs from analysis df
  ds2plot <- ds2[!is.na(ds2$value), ]
  if (nrow(ds2plot) * ncol(ds2plot) == 0) {
    util_error("No data left, aborting.",
               applicability_problem = FALSE)
  }

  # Initialize with NA
  ds2plot$tukey <- NA
  ds2plot$sixsigma <- NA
  ds2plot$hubert <- NA
  ds2plot$sigmagap <- NA
  # browser()
  # View(ds2plot)
  # apply outlier functions to plot-df
  # after export/final built  correct the call of the utility functions
  ds2plotOL <- ds2plot %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(tukey = util_tukey(value)) %>%
    dplyr::mutate(sixsigma = util_sixsigma(value)) %>%
    dplyr::mutate(hubert = util_hubert(value)) %>%
    dplyr::mutate(sigmagap = util_sigmagap(value))

  # table(util_tukey(ds2plotOL$value))

  # tibble to df
  ds2plot <- as.data.frame(ds2plotOL)

  # calculate summary of all outlier definitions
  ds2plot$Rules <- apply(ds2plot[, 3:6], 1, sum)

  # outlier to low or to high?
  ds2plot$tlta <- ifelse(ds2plot$Rules >= n_rules, 1, 0)

  # Indicate direction of deviation (low/high)
  for (i in unique(ds2plot$variable)) {
    if (length(ds2plot$tlta[ds2plot$tlta == 1 & ds2plot$variable == i]) > 0) {
      ds2plot$tlta[ds2plot$tlta == 1 & ds2plot$variable == i] <- ifelse(
        ds2plot$value[ds2plot$tlta == 1 & ds2plot$variable == i] <
          median(ds2plot$value[ds2plot$variable == i], na.rm = TRUE),
        -1, 1
      )
    }
  }

  # create summary table here --------------------------------------------------
  st1 <- aggregate(ds2plot$value, list(ds2plot$variable), mean)
  colnames(st1) <- c("Variables", "Mean")
  st1$"SD" <- aggregate(ds2plot$value, list(ds2plot$variable), sd)$x
  st1$"Median" <- aggregate(ds2plot$value, list(ds2plot$variable), median)$x
  st1$"Skewness" <-
    aggregate(ds2plot$value, list(ds2plot$variable), robustbase::mc)$x
  st1$"Tukey (N)" <- aggregate(ds2plot$tukey, list(ds2plot$variable), sum)$x
  st1$"6-Sigma (N)" <-
    aggregate(ds2plot$sixsigma, list(ds2plot$variable), sum)$x
  st1$"Hubert (N)" <- aggregate(ds2plot$hubert, list(ds2plot$variable), sum)$x
  st1$"Sigma-gap (N)" <-
    aggregate(ds2plot$sigmagap, list(ds2plot$variable), sum)$x
  st1$"Most likely (N)" <- aggregate(ds2plot$Rules, list(ds2plot$variable),
                                     function(x) {
    sum(x >= n_rules)
  })$x
  st1$"To low (N)" <- aggregate(ds2plot$tlta, list(ds2plot$variable),
                                function(x) {
    sum(x == -1)
  })$x
  st1$"To high (N)" <- aggregate(ds2plot$tlta, list(ds2plot$variable),
                                 function(x) {
    sum(x == 1)
  })$x
  st1$Grading <- ifelse(st1$"Most likely (N)" > 0, 1, 0)

  # format output
  st1$Mean <- round(st1$Mean, digits = 2)
  st1$Median <- round(st1$Median, digits = 2)
  st1$SD <- round(st1$SD, digits = 2)
  st1$Skewness <- round(st1$Skewness, digits = 2)

  # create plot list here ------------------------------------------------------
  # format to factor for plot
  plot_list <- list()

  disc_cols <- c("#2166AC", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000")
  names(disc_cols) <- c(0:4)

  for (i in unique(ds2plot$variable)) {

    ds_i <- subset(ds2plot, variable == i)

    n_non_ol <- sum(ds_i$Rules == 0)

    if (max_non_outliers_plot < n_non_ol) {

      dsi_non_ol <- ds_i[ds_i$Rules == 0, , FALSE]
      dsi_ol <- ds_i[ds_i$Rules > 0, , FALSE]

      subsel_non_ol <- sample(seq_len(nrow(dsi_non_ol)),
                              size =
                                min(max_non_outliers_plot, nrow(dsi_non_ol)))

      ds_i <- rbind.data.frame(dsi_non_ol[subsel_non_ol, , FALSE], dsi_ol)

      util_warning(
        c("For %s, %d from %d non-outlier data values were",
          "sampled to avoid large plots."),
        dQuote(i),
        max_non_outliers_plot,
        n_non_ol,
        applicability_problem = FALSE
        )
    }

    ds_i$Rules <- factor(ds_i$Rules)

    if (nrow(ds_i) > 0) {
      p_i <- ggplot(ds_i, aes(x = variable, y = value)) +
        geom_jitter(data = ds_i, position = position_jitter(0.1),
                    aes(color = Rules, alpha = 0.5, size =
                          as.numeric(Rules) / 10)) +
        scale_size_continuous(range = c(0.01, 3), guide = "none") +
        scale_color_manual(values = disc_cols) +
        facet_wrap(vars(variable), scales = "free") +
        scale_alpha(guide = "none") +
        theme_minimal()
    } else {
      p_i <- ggplot() +
        annotate("text", x = 0, y = 0, label =
                   sprintf("No outliers detected for %s", dQuote(i))) +
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
        )
    }

    plot_list[[i]] <- util_set_size(p_i, width_em = 10, height_em = 25)
  }

  return(list(SummaryTable = st1, SummaryPlotList = plot_list))
}

#' @inherit acc_univariate_outlier
#' @seealso [acc_univariate_outlier]
#' @export
acc_robust_univariate_outlier <- acc_univariate_outlier
