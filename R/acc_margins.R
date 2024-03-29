#' Estimate marginal means, see [emmeans::emmeans]
#'
#' @description
#' margins does calculations for quality indicator
#' Unexpected distribution wrt location (link). Therefore we pursue a combined
#' approach of descriptive and model-based statistics to investigate differences
#' across the levels of an auxiliary variable.
#'
#' CAT: Unexpected distribution w.r.t. location
#'
#' Marginal means
#'
#' Marginal means rests on model based results, i.e. a significantly different
#' marginal mean depends on sample size. Particularly in large studies, small
#' and irrelevant differences may become significant. The contrary holds if
#' sample size is low.
#'
#' [Indicator]
#'
#' @details
#' Limitations
#'
#' Selecting the appropriate distribution is complex. Dozens of continuous,
#' discrete or mixed distributions are conceivable in the context of
#' epidemiological data. Their exact exploration is beyond the scope of this
#' data quality approach. The function above uses the help function
#' \link{util_dist_selection}
#' which discriminates four cases:
#' \itemize{
#'   \item continuous data
#'   \item binary data
#'   \item count data with <= 20 categories
#'   \item count data with > 20 categories
#'  }
#'  Nonetheless, only three different plot types are generated. The fourth case
#'  is treated as continuous data. This is in fact a coarsening of the original
#'  data but for the purpose of clarity this approach is chosen.
#'
#' @keywords accuracy
#'
#' @param resp_vars [variable] the name of the continuous measurement variable
#' @param group_vars [variable list] len=1-1. the name of the observer, device or
#'                                   reader variable
#' @param co_vars [variable list] a vector of covariables, e.g. age and sex for
#'                                adjustment
#' @param threshold_type [enum] empirical | user | none. In case empirical is
#'                       chosen a multiplier of the scale measure is used,
#'                       in case of user a value of the mean or probability
#'                       (binary data) has to be defined see Implementation
#'                       and use of thresholds. In case of none, no thresholds
#'                       are displayed and no flagging of
#'                       unusual group levels is applied.
#' @param threshold_value [numeric] a multiplier or absolute value see
#'                                  Implementation and use of thresholds
#' @param min_obs_in_subgroup [integer] from=0. optional argument if a
#'                                      "group_var" is used.
#'                                      This argument specifies the
#'                                      minimum no. of observations that is
#'                                      required to include a subgroup (level)
#'                                      of the "group_var" in
#'                                      the analysis. Subgroups with less
#'                                      observations are excluded. The
#'                                      default is 5.
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list with:
#'   - SummaryTable: data frame underlying the plot
#'   - SummaryData: data frame
#'   - SummaryPlot: ggplot2 margins plot
#' @export
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot geom_pointrange
#'                     element_blank element_text element_text
#'                     geom_pointrange geom_density coord_flip annotate
#'                     ggplot_build theme_minimal labs theme scale_colour_manual
#'                     geom_count aes geom_vline theme_set
#' @import patchwork
#' @importFrom utils tail head
#'
#' @examples
#' \dontrun{
#' # runs spuriously slow on rhub
#' load(system.file("extdata/study_data.RData", package = "dataquieR"))
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"))
#' acc_margins(resp_vars = "DBP_0",
#'             study_data = study_data,
#'             meta_data = meta_data,
#'             group_vars = "USR_BP_0",
#'             label_col = LABEL,
#'             co_vars = "AGE_0")
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_margins.html
#' )
acc_margins <- function(resp_vars = NULL, group_vars = NULL, co_vars = NULL,
                        threshold_type = NULL, threshold_value,
                        min_obs_in_subgroup = 5,
                        study_data, meta_data, label_col
                        # TODO: flip_mode =
) {

  # to avoid "no visible binding for global variable ‘sample_size’"
  sample_size <- NULL

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  util_correct_variable_use("resp_vars",
                            need_type = "integer|float",
                            need_scale = "!na | !nominal | !ordinal",
                            min_distinct_values = 2)

  util_correct_variable_use("group_vars",
                            allow_any_obs_na = TRUE,
                            need_type = "!float",
                            need_scale = "nominal | ordinal")

  util_correct_variable_use("co_vars",
                            allow_more_than_one = TRUE,
                            allow_all_obs_na = FALSE,
                            allow_null = TRUE,
                            allow_na = TRUE)

  co_vars <- na.omit(co_vars)
  if (is.null(co_vars)) {
    co_vars <- character(0)
  }

  # replace dollar signs for emmeans
  dollar <- "\uFE69"
  colnames(ds1) <- gsub("$", dollar, fixed = TRUE, colnames(ds1))
  if (any(grepl('$', fixed = TRUE, c(resp_vars, group_vars, co_vars)))) {
    util_message(c("emmeans used by acc_margins does not support variable",
                   "names containing %s, replacing this symbol by an",
                   "equivalent unicode character %s"),
                 dQuote("$"),
                 dQuote(dollar), applicability_problem = TRUE,
                 intrinsic_applicability_problem = FALSE)
  }
  resp_vars <- gsub("$", dollar, fixed = TRUE, resp_vars)
  group_vars <- gsub("$", dollar, fixed = TRUE, group_vars)
  co_vars <- gsub("$", dollar, fixed = TRUE, co_vars)

  util_expect_scalar(min_obs_in_subgroup,
                     check_type = util_is_numeric_in(min = 5,
                                                     whole_num = TRUE,
                                                     finite = TRUE),
                     convert_if_possible = function(x) {
                       x1 <- suppressWarnings(as.integer(x))
                       if (is.na(x1) ||
                           !util_is_numeric_in(min = 5, whole_num = TRUE,
                                               finite = TRUE)(x1)) {
                         x1 <- as.integer(5)
                         util_message(
                           paste("min_obs_in_subgroup is not specified",
                                 "correctly and is set to 5 instead."),
                           applicability_problem = TRUE)
                       }
                       x1
                     })

  # Label assignment
  if (!is.null(group_vars)) {
    # all labelled variables
    levlabs <- meta_data$VALUE_LABELS[meta_data[[label_col]] %in% group_vars]

    # only variables with labels

    if (!all(is.na(levlabs))) {
      gvs_ll <- group_vars
      # assign labels only if assignment operator is found and the variable has
      # labels
      if (grepl("=", levlabs)) {
        ds1[[gvs_ll]] <- util_assign_levlabs(
          variable = ds1[[gvs_ll]],
          string_of_levlabs = levlabs,
          splitchar = SPLIT_CHAR,
          assignchar = " = "
        )
      }
    }
  }

  # omit missing values and unnecessary variables
  n_prior <- nrow(ds1)
  ds1 <- ds1[, c(resp_vars, group_vars, co_vars), drop = FALSE]
  ds1 <- ds1[complete.cases(ds1[, c(group_vars, co_vars)]), ]
  n_post <- nrow(ds1)
  msg <- NULL
  if (n_post < n_prior) {
    msg <- paste0(
      "Due to missing values in ",
      ifelse(length(co_vars) > 0,
             paste(paste0(co_vars, collapse = ", "), "or "),
             ""),
      group_vars, ", N = ", n_prior - n_post,
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

  # resp_vars should not be coded as factor
  if (is.factor(ds1[[resp_vars]])) {
    ds1[[resp_vars]] <- as.numeric(ds1[[resp_vars]])
  }

  if (!missing(threshold_value)) {
    if (is.vector(threshold_value)) {
      .threshold_value <- as.numeric(threshold_value)
    } else {
      .threshold_value <- NA
    }
    if (length(threshold_value) != 1 || is.na(.threshold_value)) {
      util_message(
        "threshold_value is not numeric(1): %s, setting it to default value 1.",
        dQuote(head(try(as.character(threshold_value)), 1)),
        applicability_problem = TRUE)
      threshold_value <- 1
    } else {
      threshold_value <-
        .threshold_value
    }
  }

  if (is.null(threshold_type) || (!is.list(threshold_type)
                                  && length(threshold_type) != 1)) {
    if (
      !is.null(threshold_type)
      ||
      !.called_in_pipeline
    ) util_message("No or many threshold type specified and set to empirical.",
                   applicability_problem = TRUE)
    threshold_type <- "empirical"
  }

  threshold_type <- match.arg(threshold_type, c("empirical", "user", "none"))

  # no relative distance (based on SD) to mean defined?
  if (threshold_type %in% c("empirical", "none") & missing(threshold_value)) {
    threshold_value <- 1
  }

  # threshold is user but no value defined -> switch to empirical
  if (threshold_type == "user" & missing(threshold_value)) {
    util_message(
      c(
        "Threshold was set to user but no value for the unit of measurements",
        "was defined.\n",
        "The function switches to one SD as default."),
      applicability_problem = TRUE)
    threshold_type == "empirical"
    threshold_value <- 1
  }

  # summary data frame of observations by level of random effect and non-missing
  # values in resp_vars
  check_df <- util_table_of_vct(ds1[[group_vars]])

  # Consider remaining obs/level after na.rm() ---------------------------------
  # too few observations in >1 level of group_vars
  if (min(check_df[, 2]) < min_obs_in_subgroup) {
    critical_levels <- levels(check_df$Var1)[check_df$Freq <
                                               min_obs_in_subgroup]
    util_message(paste0(c(
      "The following levels:", head(critical_levels, 100),
      if (length(critical_levels) > 100)  {", ..." }, "have <",
      min_obs_in_subgroup, " observations and will be removed."
    ),
    collapse = " "
    ), applicability_problem = FALSE)

    ds1 <- ds1[!(ds1[[group_vars]] %in% critical_levels), ]
  }

  if (!(prod(dim(ds1)))) {
    util_error("No data left after data preparation.")
  }

  # Modelling ------------------------------------------------------------------
  # if no co_vars are defined for adjustment only the intercept is modelled
  if (length(co_vars) == 0) {
    co_vars <- "1"
  } else {
    co_vars <- util_bQuote(co_vars)
  }

  # build model formula
  fmla <- as.formula(paste0(
    paste0(util_bQuote(resp_vars), "~"),
    paste0(
      paste0(co_vars, collapse = " + "),
      " + ",
      util_bQuote(group_vars)
    )
  ))

  # choose a suitable modeling approach
  var_prop <- util_dist_selection(ds1[[resp_vars]])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  var_scale <- meta_data[[SCALE_LEVEL]][meta_data[[label_col]] == resp_vars]
  var_dtype <- meta_data[[DATA_TYPE]][meta_data[[label_col]] == resp_vars]
  if (var_dtype == DATA_TYPES$INTEGER &
      var_prop$NCategory > 2 & var_prop$NCategory <= 20 &
      # TODO: Count data can exceed 20, of course! How do we identify count
      # data here? Maybe include a pre-test to choose between poisson and
      # linear regression? Or also consider negative binomial regression here?
      !var_prop$AnyNegative) {
    method <- "poisson"
  } else if (!is.na(var_prop$NCategory) && var_prop$NCategory == 2) {
    method <- "logistic"
  } else if (var_scale %in% c(SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL)) {
    method <- "linear"
  } else {
    util_error("No suitable method implemented yet, sorry.")
  }

  if (method == "linear") {
    # call linear model
    model <- lm(fmla, data = ds1)
  } else if (method == "poisson") {
    # call poisson model
    model <- glm(fmla, data = ds1, family = poisson(link = "log"))
  } else {
    # recode binary variable to 0/1, if needed
    if (!all(unique(ds1[[resp_vars]]) %in% c(0, 1))) {
      bin_codes <- names(sort(table(ds1[[resp_vars]])))
      mf <- as.numeric(tail(bin_codes, 1))
      # https://stackoverflow.com/questions/12187187/
      #     how-to-retrieve-the-most-repeated-value-in-a-
      #                                         column-present-in-a-data-frame
      ds1[[resp_vars]][ds1[[resp_vars]] == mf] <- 0
      # could be other than 1
      lf <- as.numeric(head(bin_codes, 1))
      ds1[[resp_vars]][ds1[[resp_vars]] == lf] <- 1
    }
    # call logistic regression
    model <- glm(fmla, data = ds1, family = binomial(link = "logit"))
  }

  # TODO: check whether the following problem still occurs:
  # emmeans::emmeans appears not working correctly for the overall mean. Somehow
  # the package assumes an interaction term although there isn't.
  # browser()
  # call emmeans::emmeans
  res_df <- data.frame(emmeans::emmeans(model, group_vars, type = "response"),
                       check.names = FALSE)
  summary_ds <- as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
                                                                 unname(group_vars)), sample_size = dplyr::n()))

  res_df<- merge(res_df, summary_ds, by = group_vars, all.x = TRUE)



  if (method == "linear") {
    res_df <- dplyr::rename(res_df, c("margins" = "emmean", "LCL" = "lower.CL",
                                      "UCL" = "upper.CL"))
  } else if (method == "poisson") {
    res_df <- dplyr::rename(res_df, c("margins" = "rate", "LCL" = "asymp.LCL",
                                      "UCL" = "asymp.UCL"))
  } else {
    res_df <- dplyr::rename(res_df, c("margins" = "prob", "LCL" = "asymp.LCL",
                                      "UCL" = "asymp.UCL"))
  }

  # adjusted overall mean
  omv <- data.frame(emmeans::emmeans(model, "1", type = "response"))
  if (method == "linear") {
    omv <- dplyr::rename(omv, c("margins" = "emmean", "LCL" = "lower.CL",
                                "UCL" = "upper.CL"))
  } else if (method == "poisson") {
    omv <- dplyr::rename(omv, c("margins" = "rate", "LCL" = "asymp.LCL",
                                "UCL" = "asymp.UCL"))
  } else {
    omv <- dplyr::rename(omv, c("margins" = "prob", "LCL" = "asymp.LCL",
                                "UCL" = "asymp.UCL"))
  }

  res_df$overall <- omv$margins

  # Thresholds -----------------------------------------------------------------
  if (threshold_type %in% c("empirical", "none")) {
    if (method == "linear") {
      th <- sd(ds1[[resp_vars]])
      parn <- c(
        paste("-", threshold_value, "TH", sep = ""), "Mean",
        paste("+", threshold_value, "TH", sep = "")
      )
    } else if (method == "poisson") {
      # th <- exp(as.numeric(coefficients(glm(v101 ~ 1,
      #    family = poisson(link="log"), data = expl_df))))
      th <- 1
      parn <- c(
        paste("-", threshold_value, "TH", sep = ""), "Mean",
        paste("+", threshold_value, "TH", sep = "")
      )
    } else {
      th <- mean(ds1[[resp_vars]])
      th <- th * (1 - th)
      parn <- c(
        paste("-", threshold_value, "TH", sep = ""), "Prob.",
        paste("+", threshold_value, "TH", sep = "")
      )
    }

    pars <- as.vector(c(
      omv$margins - threshold_value * th, omv$margins,
      omv$margins + threshold_value * th
    ))

    # store threshold
    res_df$threshold <- threshold_value

    # select abnormalities
    res_df$GRADING <- ifelse(res_df$margins < pars[1] |
                               res_df$margins > pars[3], 1, 0)

  } else if (threshold_type == "user") {
    th <- threshold_value
    # store threshold
    res_df$threshold <- th

    pars <- as.vector(c(th, th, th))
    if (method == "logistic") {
      parn <- c("", paste0("Prob.=", threshold_value, sep = ""), "")
    } else {
      parn <- c("", paste0("Mean=", threshold_value, sep = ""), "")
    }

    # select abnormalities
    res_df$GRADING <- mapply(
      function(th, l, u) {
        ifelse(th >= l & th <= u, 0, 1)
      },
      res_df$threshold, res_df$LCL, res_df$UCL
    )
  }

  if (length(setdiff(co_vars, "1")) > 0) {
    if (length(co_vars) < 10) {
      subtitle <- sprintf("adjusted for %s", paste0(co_vars, collapse = ", "))
    } else {
      subtitle <- sprintf("adjusted for %d variables", length(co_vars))
    }
  } else {
    subtitle <- ""
  }

  # Graphics -------------------------------------------------------------------
  # use offset for annotation depending on variable scale
  offs <- ifelse(th <= 50, th / 10, th / 20)

  if (method == "linear") {
    #calculate sample size per group
    #    summary_ds<- as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
    #                                                                   group_vars), samplesize = dplyr::n()))

    # Plot 1: hybrid density/boxplot graph
    warn_code <- c("1" = "#B2182B", "0" = "#2166AC")

    p1 <- ggplot(data = ds1[, c(resp_vars, group_vars), drop = FALSE],
                 aes(x = .data[[group_vars]], y = .data[[resp_vars]])) +
      geom_violin(alpha = 0.9, draw_quantiles = TRUE, fill = "gray99") +
      geom_boxplot(width = 0.1, fill = "white", color = "gray", alpha = 0.5) +
      geom_pointrange(
        data = res_df, aes(
          x = factor(.data[[group_vars]]),
          y = margins,
          ymin = LCL,
          ymax = UCL,
          color = as.factor(GRADING)
          # n = sample_size
        ),
        shape = 18, linewidth = 1,
        inherit.aes = FALSE,
        fatten = 5
      ) +
      theme_minimal() +
      labs(x = "", y = "") +
      theme(
        legend.position = "None", legend.title = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      scale_colour_manual(values = warn_code)+
      geom_text(data = summary_ds, aes(x = summary_ds[, group_vars],
                                       y = max(ds1[, c(resp_vars), drop = FALSE])+1.2,
                                       label =  sample_size), hjust = 0.5, angle = 90)+
      annotate("text", x=0.50, y=max(ds1[, c(resp_vars), drop = FALSE])+1.2, label= "N")

    if (threshold_type != "none") {
      p1 <- p1 +
        geom_hline(yintercept = pars[2], color = "red") +
        geom_hline(yintercept = pars[-2], color = "red", linetype = 2)
    } else {
      p1 <- p1 +
        geom_hline(yintercept = pars[2], color = "red")
    }
  } else {
    warn_code <- c("1" = "#B2182B", "0" = "#2166AC")

    #calculate sample size per group
    #    summary_ds<- as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
    #                                                                   group_vars), samplesize = dplyr::n()))

    p1 <- ggplot(data = ds1[, c(resp_vars, group_vars), drop = FALSE],
                 aes(x =  .data[[group_vars]], y =  .data[[resp_vars]])) +
      geom_count(aes(alpha = 0.9), color = "gray") +
      geom_pointrange(
        data = res_df, aes(
          x = .data[[group_vars]],
          y = margins,
          ymin = LCL,
          ymax = UCL,
          color = as.factor(GRADING),
          # n = sample_size
        ),
        shape = 18, linewidth = 1,
        inherit.aes = FALSE,
        fatten = 5
      ) +
      theme_minimal() +
      labs(x = "", y = "") +
      theme(
        legend.position = "None", legend.title = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      scale_colour_manual(values = warn_code)+
      geom_text(data = summary_ds, aes(x = summary_ds[, group_vars],
                                       y = max(ds1[, c(resp_vars), drop = FALSE])+1.2,
                                       label =  sample_size), hjust = 0.5, angle = 90)+
      annotate("text", x=0.50, y=max(ds1[, c(resp_vars), drop = FALSE])+1.2, label= "N")

    if (threshold_type != "none") {
      p1 <-
        p1 +
        geom_hline(yintercept = pars[2], color = "red") +
        geom_hline(yintercept = pars[-2], color = "red", linetype = 2)
    } else {
      p1 <-
        p1 +
        geom_hline(yintercept = pars[2], color = "red")
    }
  }

  # Plot 2: overall distributional plot flipped on y-axis of plot 1
  get_y_scale <- ggplot(ds1[, resp_vars, drop = FALSE], aes(x = .data[[resp_vars]])) +
    geom_density(alpha = 0.35)
  aty <- mean(range(ggplot_build(get_y_scale)$data[[1]]$y))

  p2 <- ggplot(ds1[, resp_vars, drop = FALSE], aes(.data[[resp_vars]])) +
    geom_density(alpha = 0.35) +
    coord_flip() +
    theme_minimal() +
    #    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          text = element_text(size = 16))
  if (threshold_type != "none") {
    p2 <-
      p2 +
      annotate(geom = "text", x = pars + offs, y = aty, label = parn) +
      geom_vline(xintercept = pars[2], color = "red") +
      geom_vline(xintercept = pars[-2], color = "red", linetype = 2)
  } else {
    p2 <-
      p2 +
      annotate(geom = "text", x = pars + offs, y = aty,
               label = c("", parn[2], "")) +
      geom_vline(xintercept = pars[2], color = "red")
  }


  # combine plots
  # and add the title
  res_plot <- # TODO: For all patchwork-calls, add information to reproduce the layout in plot.ly
    p1 +
    p2 +
    plot_layout(nrow = 1,
                widths = c(5, 1)
    ) +
    plot_annotation(title = paste(group_vars, "margins in", resp_vars),
                    subtitle = subtitle)

  SummaryTable <- data.frame(Variables = resp_vars, FLG_acc_ud_loc =
                               as.numeric(any(res_df$GRADING > 0)),
                             PCT_acc_ud_loc = round(sum(res_df$GRADING == 1)/nrow(res_df)*100, digits = 2))

  SummaryData <- res_df
  SummaryData$df <- NULL
  SummaryData$threshold <- NULL
  SummaryData$overall <- NULL
  SummaryData$GRADING <- NULL
  SummaryData$CL <- paste0("[", format(SummaryData$LCL), "; ",
                           format(SummaryData$UCL), "]")
  SummaryData$LCL <- NULL
  SummaryData$UCL <- NULL

  colnames(SummaryData)[[4]] <- "n"

  attr(SummaryData, "description") <- character(0)
  attr(SummaryData, "description")[[group_vars]] <- "Group: Observer/Device/..."
  attr(SummaryData, "description")[["margins"]] <- "Mean for the Group"
  attr(SummaryData, "description")[["SE"]] <- "Standard error for the Group"
  attr(SummaryData, "description")[["n"]] <-
    "Number of Observations of the Group"
  attr(SummaryData, "description")[["CL"]] <-
    "Confidence Interval for the Group"



  # length(unique(fit_df$GROUP)))
  # output
  return(util_attach_attr(list(
    SummaryData = SummaryData,
    SummaryTable = SummaryTable,
    SummaryPlot = util_set_size(res_plot, width_em = 25 +
                                  1.2 * length(unique(ds1[[group_vars]])),
                                height_em = 25)
  ), as_plotly = "util_as_plotly_acc_margins"))
}


#' @family plotly_shims
#' @concept plotly_shims
#' @keywords internal
util_as_plotly_acc_margins <- function(res, ...) {
  #remove classes for plotly to work properly
  res$SummaryPlot <- util_remove_dataquieR_result_class(res$SummaryPlot)
  # use res$SummaryPlot, not res_plot to avoid depending on the enclosure
  # of the result, that may contain study data.
  util_ensure_suggested("plotly")
  util_stop_if_not(inherits(res$SummaryPlot, "patchwork"))
  # extract estimates on size of the plot from the patchwork object
  # obtain relative width of the single elements of the plot
  rel_w <- res$SummaryPlot$patches$layout$widths /
    sum(res$SummaryPlot$patches$layout$widths, na.rm = TRUE)
  # extract the violin plots
  py1 <- try(plotly::ggplotly(res$SummaryPlot[[1]],
                              ...), silent = TRUE)

  py1$x$data[[2]]$mode <- NULL # suppress a warning on print (https://github.com/plotly/plotly.R/issues/2242)
  # extract the overall distribution plot
  py2 <- try(plotly::ggplotly(res$SummaryPlot[[2]],
                              ...), silent = TRUE)
  # check if both are plotly objects
  util_stop_if_not(!inherits(py1, "try-error"))
  util_stop_if_not(!inherits(py2, "try-error"))
  # https://plotly.com/r/subplots/#subplots-with-shared-yaxes


  #  summary_ds<-as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
  #                                                               group_vars), samplesize = dplyr::n()))


  # py1<- plotly::ggplotly(py1, tooltip = paste("Sample size:",
  #                                            as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
  #                                                                                                             group_vars), samplesize = dplyr::n()))[,2]))
  #py2<- plotly::layout(py2)

  target_layers <-
    which(lapply(lapply(py1$x$data, `[[`, "marker"), `[[`, "symbol") == "diamond")

  hovertexts <- lapply(py1$x$data, `[[`, "hovertext")
  hovertexts_matching_sample_size <- lapply(hovertexts, grepl, pattern = "sample_size: ", fixed = TRUE)
  hovertexts_matching_sample_size_with_length_nr_groups <-
    vapply(hovertexts_matching_sample_size, function(x) length(x) == length(unique(res$SummaryPlot[[1]]$data[[2]])) && all(x, na.rm = TRUE), FUN.VALUE = logical(1))
  layer_with_sample_size <- which(hovertexts_matching_sample_size_with_length_nr_groups)
  if (length(layer_with_sample_size) != 1) {
    util_warning(c("Internal error: unexpected number of",
                   "layer_with_sample_size. Sorry, please report to us"))
  } else {
    hovertexts_with_hovertexts <- py1$x$data[[layer_with_sample_size]]$hovertext

    xpositions_with_hovertexts <- py1$x$data[[layer_with_sample_size]]$x

    for (tl in target_layers) {
      # amend texts for hover-texts, matching by x position.

      py1$x$data[[tl]]$text <- paste0(
        py1$x$data[[tl]]$text,
        "<br />",
        hovertexts_with_hovertexts[match(py1$x$data[[tl]]$x,
                                         xpositions_with_hovertexts)]
      )
    }
  }


  # recombine plots
  suppressMessages(force(plotly::layout(plotly::subplot(py1,
                                 py2,
                                 nrows =
                                   res$SummaryPlot$patches$layout$nrow,
                                 shareY = TRUE, #all plots use the same y axes
                                 widths = #define relative width
                                   rel_w),
                 title = list(text = #get the overall title from patch
                                res$SummaryPlot$patches$annotation$title),
                 font = list(size = 12),
                 margin = 0.01)))
}
