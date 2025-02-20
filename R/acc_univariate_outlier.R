#' Identify univariate outliers by four different approaches
#'
#' @description
#' A classical but still popular approach to detect univariate outlier is the
#' boxplot method introduced by Tukey 1977. The boxplot is a simple graphical
#' tool to display information about continuous univariate data (e.g., median,
#' lower and upper quartile). Outliers are defined as values deviating more
#' than \eqn{1.5 \times IQR} from the 1st (Q25) or 3rd (Q75) quartile. The
#' strength of Tukey's method is that it makes no distributional assumptions
#' and thus is also applicable to skewed or non mound-shaped data
#' Marsh and Seo, 2006. Nevertheless, this method tends to identify frequent
#' measurements which are falsely interpreted as true outliers.
#'
#' A somewhat more conservative approach in terms of symmetric and/or normal
#' distributions is the 3SD approach, i.e. any measurement not in
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
#' [Indicator]
#'
#' @details
#'
#' ***Hint***: *The function is designed for unimodal data only.*
#'
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Select all variables of type float in the study data
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Remove measurements deviating from limits defined in the metadata
#'  - Identify outliers according to the approaches of Tukey (Tukey 1977),
#'    3SD (Saleem et al. 2021), Hubert (Hubert and Vandervieren 2008),
#'    and SigmaGap (heuristic)
#'  - An output data frame is generated which indicates the no. possible
#'    outliers, the direction of deviations (Outliers, low; Outliers, high) for all methods
#'    and a summary score which sums up the deviations of the different rules
#'  - A scatter plot is generated for all examined variables, flagging
#'    observations according to the no. violated rules (step 5).
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable list] the name of the continuous measurement
#'                                  variable
#' @param exclude_roles [variable roles] a character (vector) of variable roles
#'                                       not included
#' @param n_rules [integer] from=1 to=4. the no. rules that must be violated
#'        to flag a variable as containing outliers. The default is 4, i.e. all.
#' @param max_non_outliers_plot [integer] from=0. Maximum number of non-outlier
#'                                                points to be plot. If more
#'                                                points exist, a subsample will
#'                                                be plotted only. Note, that
#'                                                sampling is not deterministic.
#' @param criteria [set] tukey | 3SD | hubert | sigmagap. a vector with
#'                       methods to be used for detecting outliers.
#'
#' @seealso
#' - [acc_robust_univariate_outlier]
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_robust_univariate_outlier.html
#' )
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot geom_jitter position_jitter aes
#'                     scale_size_continuous scale_color_manual
#'                     facet_wrap vars scale_alpha theme_minimal
#' @importFrom stats median aggregate sd aggregate
#' @export
#'
#' @return a list with:
#' - `SummaryTable`: [`data.frame`] with the columns
#'        `Variables`, `Mean`, `SD`, `Median`, `Skewness`, `Tukey (N)`,
#'        `3SD (N)`, `Hubert (N)`, `Sigma-gap (N)`, `NUM_acc_ud_outlu`,
#'        `Outliers, low (N)`, `Outliers, high (N)` `Grading`
#'   - `SummaryData`: [`data.frame`] with the columns
#'        `Variables`, `Mean`, `SD`, `Median`, `Skewness`, `Tukey (N)`,
#'        `3SD (N)`, `Hubert (N)`, `Sigma-gap (N)`, `Outliers (N)`,
#'        `Outliers, low (N)`, `Outliers, high (N)`
#'   - `SummaryPlotList`: [`ggplot2::ggplot`] univariate outlier plots
#'
#'
acc_univariate_outlier <- function(resp_vars = NULL,
                                   study_data,
                                   label_col,
                                   item_level = "item_level",
                                   exclude_roles,
                                   n_rules = length(unique(criteria)),
                                   max_non_outliers_plot = 10000,
                                   criteria = c("tukey", "3sd",
                                                "hubert", "sigmagap"),
                                   meta_data = item_level,
                                   meta_data_v2) {

  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()

  #compatibility with previous name (sixsigma)
  #replace "sixsigma" (if any attributed in criteria) with 3SD
  old_name<- "sixsigma"
  if(any(criteria %in% old_name)== TRUE){
    criteria[criteria=="sixsigma"] <- "3sd"
  }

  #all lowercase
  criteria <- trimws(tolower(criteria))

  # TODO: Remove all obsoleted checks on resp_vars, the function uses util_correct_variable_use
  util_expect_scalar(criteria,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)



  if (length(unique(criteria)) < 1 ||
      length(unique(criteria)) >
      length(eval(formals(acc_univariate_outlier)$criteria)) ||
      !all(criteria %in% eval(formals(acc_univariate_outlier)$criteria))) {
    if (!.called_in_pipeline)
      util_message(c("The formal criteria must have > 0 and < %d entries.",
                     "Allowed values are %s.",
                     "I was called with %s, falling back to default %s."),
                   length(eval(formals(acc_univariate_outlier)$criteria)),
                   paste(dQuote(eval(formals(acc_univariate_outlier)$criteria)),
                         collapse = ", "),
                   paste(dQuote(unique(criteria)), collapse = ", "),
                   paste(dQuote(eval(formals(acc_univariate_outlier)$criteria)),
                         collapse = ", "), applicability_problem = TRUE)
    criteria <- eval(formals(acc_univariate_outlier)$criteria)
  }

  if (length(n_rules) != 1 || !is.numeric(n_rules) ||
      !all(util_is_integer(n_rules)) ||
      !(n_rules %in% seq_len(length(unique(criteria))))) {
    if (!.called_in_pipeline)
      util_message(
        "The formal n_rules is not an integer between 1 and %d, default (%d) is used.",
        length(unique(criteria)),
        min(eval(formals(acc_univariate_outlier)$n_rules),
            length(unique(criteria))),
        applicability_problem = TRUE)
    n_rules <- min(eval(formals(acc_univariate_outlier)$n_rules),
                   length(unique(criteria)))
  }

  if (length(max_non_outliers_plot) != 1 ||
      !is.numeric(max_non_outliers_plot) ||
      !all(util_is_integer(max_non_outliers_plot)) ||
      (max_non_outliers_plot < 0)) {
    util_message(
      c("The formal max_non_outliers_plot is not an integer >= 0,",
        "default (%d) is used."),
      formals(acc_univariate_outlier)$max_non_outliers_plot,
      applicability_problem = TRUE)
    max_non_outliers_plot <-
      formals(acc_univariate_outlier)$max_non_outliers_plot
  }

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_null = TRUE,
                            allow_any_obs_na = TRUE,
                            need_type = "integer | float",
                            need_scale = "interval | ratio",
                            do_not_stop = TRUE,
                            remove_not_found = TRUE
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
  if (length(resp_vars) == 0) {

    # which are float or integer?
    resp_vars <- meta_data[[label_col]][meta_data[[DATA_TYPE]] %in%
                                          c(DATA_TYPES$FLOAT,
                                            DATA_TYPES$INTEGER)]
    util_message(paste0("The following variables: ",
                        paste0(resp_vars, collapse = ", "), " were selected."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    if (length(resp_vars) == 0) {
      util_error(paste0("No variables with suitable data type defined."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }

    resp_vars <- intersect(resp_vars, colnames(ds1))
  } else {
    # defined variables are not of type float/integer?
    isfloat <- meta_data[[DATA_TYPE]] %in%
      c(DATA_TYPES$FLOAT, DATA_TYPES$INTEGER)
    isrvs <-
      meta_data[[label_col]] %in% resp_vars

    if (!all(isfloat | !isrvs)) {
      resp_vars <- meta_data[[label_col]][isfloat & isrvs]
      if (!all(!isrvs | isfloat)) util_warning(paste0("Only: ",
                                                      paste0(resp_vars,
                                                             collapse = ", "),
                                                      " are defined to be of ",
                                                      "type float or integer."),
                                               applicability_problem = TRUE,
                                               intrinsic_applicability_problem = TRUE)
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
      if (length(intersect(resp_vars, which_vars_not)) > 0) {
        util_message(paste0("Study variables: ",
                            paste(dQuote(intersect(resp_vars, which_vars_not)),
                                  collapse = ", "), " have been excluded."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
      }
      resp_vars <- setdiff(resp_vars, which_vars_not)
    }
  }

  # remove resp_vars with non-matching data type
  whicharenum <- vapply(FUN.VALUE = logical(1), ds1[, resp_vars, drop = FALSE],
                        function(x) is.numeric(x))

  if (!all(whicharenum)) {
    util_message(paste0(
      "Variables ", paste0(dQuote(resp_vars[!whicharenum]), collapse = ", "),
      " are not of type float or integer and will be removed",
      " from univariate outlier analysis."
    ), applicability_problem = TRUE,
    intrinsic_applicability_problem = TRUE)
    resp_vars <- resp_vars[whicharenum]
  }

  intcheck <- vapply(FUN.VALUE = logical(1), ds1[, resp_vars, drop = FALSE],
                     function(x) all(util_is_integer(x), na.rm = TRUE))

  if (any(intcheck)) {
    util_message(paste0(
      "Variables: ", paste0(dQuote(resp_vars[intcheck]), collapse = ", "),
      " show integer values only, but will be nonetheless considered."
    ), applicability_problem = TRUE, intrinsic_applicability_problem = TRUE)
  }

  if (length(resp_vars) > 0) {
    resp_vars <-
      util_no_value_labels(
        resp_vars = resp_vars,
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
  # ds2 <- melt(ds1_ll[, c(resp_vars, group_vars)], measure.vars = resp_vars)

  if (length(resp_vars) == 0) {
    util_error("No suitable response variables left.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  } else if (length(resp_vars) == 1) {
    ds2 <- ds1_ll[, resp_vars, drop = FALSE]
    ds2$variable <- resp_vars
    ds2 <- ds2[, c(2, 1)]
    names(ds2) <- c("variable", "value")
  } else {
    # ds2 <- melt(ds1_ll[, c(resp_vars)], measure.vars = resp_vars)
    ds2 <- stats::reshape(data = ds1_ll[, resp_vars],
                          varying = colnames(ds1_ll[, resp_vars]),
                          v.names = "value",
                          times = colnames(ds1_ll[, resp_vars]),
                          direction = "long")
    ds2 <- ds2[, -which(names(ds2) == "id")]
    #ds2$time <- as.factor(ds2$time)
    ds2$time <- factor(ds2$time, levels = resp_vars, ordered = FALSE)
    names(ds2)[names(ds2) == "time"] <- "variable"
    rownames(ds2) <- NULL

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
  ds2plot$threeSD <- NA
  ds2plot$hubert <- NA
  ds2plot$sigmagap <- NA

  # apply outlier functions to plot-df
  # after export/final built  correct the call of the utility functions
  ds2plotOL <- ds2plot %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(tukey = util_tukey(value)) %>%
    dplyr::mutate(threeSD = util_3SD(value)) %>%
    dplyr::mutate(hubert = util_hubert(value)) %>%
    dplyr::mutate(sigmagap = util_sigmagap(value))

  # table(util_tukey(ds2plotOL$value))

  # tibble to df
  ds2plot <- as.data.frame(ds2plotOL)


  #Fix the problem with name 3SD starting with a number replacing it with threeSD---
  orig_name<- "3sd"
  if(any(criteria %in% orig_name)== TRUE){
    criteria[criteria=="3sd"] <- "threeSD"
  }

  # calculate summary of all outlier definitions
  ds2plot$Rules <- apply(ds2plot[, criteria, drop = FALSE], 1, sum)

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
  st1 <- aggregate(ds2plot$value, list(ds2plot$variable), mean) # TODO: here and in multi, add standard columns for indicator metrics
  colnames(st1) <- c("Variables", "Mean")
  st1$"No.records"<- aggregate(ds2plot$value, list(ds2plot$variable), length)$x
  st1$"SD" <- aggregate(ds2plot$value, list(ds2plot$variable), sd)$x
  st1$"Median" <- aggregate(ds2plot$value, list(ds2plot$variable), median)$x
  st1$"Skewness" <-
    aggregate(ds2plot$value, list(ds2plot$variable), robustbase::mc,
              doScale = FALSE)$x
  st1$"Tukey (N)" <- aggregate(ds2plot$tukey, list(ds2plot$variable), sum)$x
  st1$"3SD (N)" <-
    aggregate(ds2plot$threeSD, list(ds2plot$variable), sum)$x
  st1$"Hubert (N)" <- aggregate(ds2plot$hubert, list(ds2plot$variable), sum)$x
  st1$"Sigma-gap (N)" <-
    aggregate(ds2plot$sigmagap, list(ds2plot$variable), sum)$x
  st1$"Outliers (N)" <- aggregate(ds2plot$Rules, list(ds2plot$variable),
                                  function(x) {
                                    sum(x >= n_rules)
                                  })$x
  st1$"Outliers, low (N)" <- aggregate(ds2plot$tlta, list(ds2plot$variable),
                                       function(x) {
                                         sum(x == -1)
                                       })$x
  st1$"Outliers, high (N)" <- aggregate(ds2plot$tlta, list(ds2plot$variable),
                                        function(x) {
                                          sum(x == 1)
                                        })$x
  st1$GRADING <- ifelse(st1$"Outliers (N)" > 0, 1, 0)

  # format output
  st1$Mean <- round(st1$Mean, digits = 2)
  st1$Median <- round(st1$Median, digits = 2)
  st1$SD <- round(st1$SD, digits = 2)
  st1$Skewness <- round(st1$Skewness, digits = 2)

  SummaryTable<- st1
  names(SummaryTable)[names(SummaryTable) == "Outliers (N)"] <- "NUM_acc_ud_outlu"
  SummaryTable$PCT_acc_ud_outlu <- round(SummaryTable$NUM_acc_ud_outlu/
                                           SummaryTable$No.records*100,
                                         digits = 2)

  # create plot list here ------------------------------------------------------
  # format to factor for plot
  plot_list <- list()

  disc_cols <- c("#2166AC", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000")
  names(disc_cols) <- c(0:4)

  # select as many colors as needed
  disc_cols <- disc_cols[c("0", rev(5-seq_len(length(unique(criteria)))))]
  names(disc_cols) <- c("0", seq_len(length(unique(criteria))))

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

      util_message(
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
      ds_i$variable <-
        prep_get_labels(
          ds_i$variable,
          item_level = meta_data,
          label_col = label_col,
          resp_vars_match_label_col_only = TRUE,
          label_class = "SHORT")
      if (length(unique(ds_i$variable)) == 1 && .called_in_pipeline) {
        p_i <- ggplot(ds_i, aes(x = variable, y = value)) +
          geom_jitter(data = ds_i,
                      position = position_jitter(width = 0.3, height = 0.03),
                      aes(color = Rules, alpha = 0.5, size =
                            as.numeric(Rules) / 10)) +
          scale_size_continuous(range = c(0.5, 3), guide = "none") +
          scale_color_manual(values = disc_cols) +
          scale_alpha(guide = "none") +
          xlab("") + ylab("") +
          theme_minimal() +
          theme(axis.text.x = element_blank())
      } else {
        p_i <- ggplot(ds_i, aes(x = variable, y = value)) +
          geom_jitter(data = ds_i,
                      position = position_jitter(width = 0.3, height = 0.03),
                      aes(color = Rules, alpha = 0.5, size =
                            as.numeric(Rules) / 10)) +
          scale_size_continuous(range = c(0.5, 3), guide = "none") +
          scale_color_manual(values = disc_cols) +
          facet_wrap(vars(variable), scales = "free") +
          scale_alpha(guide = "none") +
          xlab("") + ylab("") +
          theme_minimal()
      }
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

    # Define min and max values in plot for size hint
    if (nrow(ds_i)==0){
      #In case of an empty result in ds_i
      min_val <- 0
      max_val <- 0
    } else{
      min_val <-  min(ds_i$value)
      max_val <- max(ds_i$value)
    }

    range <- max_val - min_val
    y_variable <-  unique(ds_i$variable)
    no_char_y <- max(nchar(c(min_val, max_val)))


    # Figure size hint for plot
    attr(p_i, "sizing_hints") <- list(
      figure_type_id = "scatt_plot",
      range = range,
      number_of_vars = length(unique(ds_i$variable)),
      no_char_y = no_char_y
    )

    plot_list[[i]] <- util_set_size(p_i, width_em = 10, height_em = 25)

  }

  st1 <- st1[, !names(st1) %in% c("GRADING")]

  return(list(SummaryTable = SummaryTable,
              SummaryData = st1,
              SummaryPlotList = plot_list))
}

#' @inherit acc_univariate_outlier
#' @seealso [acc_univariate_outlier]
#' @export
acc_robust_univariate_outlier <- acc_univariate_outlier
