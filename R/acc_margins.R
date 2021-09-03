#' Function to estimate marginal means, see [emmeans::emmeans]
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
#' @importFrom ggplot2 ggplot aes_ geom_violin geom_boxplot geom_pointrange
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
#' co_vars <- c("AGE_0")
#' label_col <- LABEL
#' rvs <- c("DBP_0")
#' group_vars <- prep_map_labels(rvs, meta_data = meta_data, from = label_col,
#'   to = VAR_NAMES)
#' group_vars <- prep_map_labels(group_vars, meta_data = meta_data,
#'   to = KEY_OBSERVER)
#' group_vars <- prep_map_labels(group_vars, meta_data = meta_data)
#' acc_margins(resp_vars = rvs,
#'             study_data = study_data,
#'             meta_data = meta_data,
#'             group_vars = group_vars,
#'             label_col = label_col,
#'             co_vars = co_vars)
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_acc_impl_margins.html
#' )
acc_margins <- function(resp_vars = NULL, group_vars = NULL, co_vars = NULL,
                        threshold_type = NULL, threshold_value,
                        min_obs_in_subgroup,
                        study_data, meta_data, label_col) {

  # map meta to study
  util_prepare_dataframes()


  util_correct_variable_use("resp_vars", need_type = "integer|float")
  util_correct_variable_use("group_vars",
    allow_any_obs_na = TRUE,
    need_type = "!float"
  )
  util_correct_variable_use("co_vars",
    allow_more_than_one = TRUE,
    allow_null = TRUE
  )

  rvs <- resp_vars

  # no minimum observations specified
  if (missing(min_obs_in_subgroup) || length(min_obs_in_subgroup) != 1) {
    min_obs_in_subgroup <- 5
    util_warning(
      "No or many minimum observation count was specified and is set to n=5.",
      applicability_problem = TRUE)
  } else {
    .min_obs_in_subgroup <- as.integer(min_obs_in_subgroup)
    if (is.na(.min_obs_in_subgroup)) {
      util_warning(
       "min_obs_in_subgroup is not integer: %s, setting it to default value 5.",
        dQuote(try(as.character(min_obs_in_subgroup))),
       applicability_problem = TRUE)
      min_obs_in_subgroup <- 5
    } else {
      min_obs_in_subgroup <-
        .min_obs_in_subgroup
    }
  }

  if (min_obs_in_subgroup < 5) {
    min_obs_in_subgroup <- 5
    util_warning("min_obs_in_subgroup cannot be set below 5.",
                 applicability_problem = TRUE)
  }

  # co_vars optional but warn if missing
  if (is.null(co_vars)) {
    co_vars <- NA
#    util_warning("No co_vars specified",
#                 applicability_problem = FALSE)
  }

  # Label assignment -----------------------------------------------------------
  # temporary study data

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

  # missings -------------------------------------------------------------------
  co_vars <- na.omit(co_vars)

  # co_vars and group_vars
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[rowSums(is.na(ds1[, c(co_vars, group_vars), drop = FALSE])) == 0, ,
    drop = FALSE
  ]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_warning(paste0(
      "Due to missing values in ", paste0(co_vars, collapse = ", "),
      " or ", group_vars, " N=", n_prior - n_post,
      " observations were excluded."
    ),
    applicability_problem = FALSE)
  }

  # rvs
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[!is.na(ds1[[rvs]]), ]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_warning(paste0(
      "Due to missing values in ", paste0(rvs), " N=",
      n_prior - n_post, " observations were excluded."
    ), applicability_problem = FALSE)
  }

  # type factor
  ds1[[group_vars]] <- factor(ds1[[group_vars]])

  if (!missing(threshold_value)) {
    if (is.vector(threshold_value)) {
      .threshold_value <- as.numeric(threshold_value)
    } else {
      .threshold_value <- NA
    }
    if (length(threshold_value) != 1 || is.na(.threshold_value)) {
      util_warning(
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
    util_warning("No or many threshold type specified and set to empirical.",
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
    util_warning(
     c(
      "Threshold was set to user but no value for the unit of measurements",
      "was defined.\n",
      "The function switches to one SD as default."),
     applicability_problem = TRUE)
    threshold_type == "empirical"
    threshold_value <- 1
  }

  # summary data frame of observations by level of random effect and non-missing
  # values in rvs
  check_df <- data.frame(table(ds1[[group_vars]]))

  # Consider remaining obs/level after na.rm() ---------------------------------
  # too few observations in >1 level of group_vars
  if (min(check_df[, 2]) < min_obs_in_subgroup) {
    critical_levels <- levels(check_df$Var1)[check_df$Freq <
                                               min_obs_in_subgroup]
    util_warning(paste0(c(
      "The following levels:", head(critical_levels, 100),
      if (length(critical_levels) > 100)  {", ..." }, "have <",
      min_obs_in_subgroup, " observations and will be removed."
    ),
    collapse = " "
    ), applicability_problem = FALSE)

    ds1 <- ds1[!(ds1[[group_vars]] %in% critical_levels), ]
  }

  # if no co_vars are defined for adjustment only the intercept is modelled
  if (length(co_vars) == 1 & is.na(co_vars)[1]) {
    co_vars <- "1"
  }

  #############
  # Modelling #
  #############

  # determine distributional approximation
  seldist <- util_dist_selection(ds1[[rvs]], meta_data = meta_data)

  # build model formula
  fmla <- as.formula(paste0(
    paste0(rvs, "~"),
    paste0(
      paste0(co_vars, collapse = " + "),
      " + ",
      group_vars
    )
  ))

  # variable of type float or integer with > 20 categories
  if (seldist$IsInteger == FALSE | seldist$IsInteger == TRUE &
      seldist$NCategory > 20) {
    # call linear model
    model <- lm(fmla, data = ds1)
  }

  # variable of type binary
  if (seldist$IsInteger == TRUE & seldist$IsMultCat == 0) {
    if (is.factor(ds1[[rvs]])) {
      ds1[[rvs]] <- as.integer(ds1[[rvs]])
    }

    # other levels than 0/1
    if (!all(unique(ds1[[rvs]]) %in% c(0, 1))) {
      mf <- as.numeric(tail(names(sort(table(ds1[[rvs]]))), 1))
        # https://stackoverflow.com/questions/12187187/
        #     how-to-retrieve-the-most-repeated-value-in-a-
        #                                         column-present-in-a-data-frame
      ds1[[rvs]][ds1[[rvs]] == mf] <- 0
      # could be other than 1
      lf <- as.numeric(head(names(sort(table(ds1[[rvs]]))), 1))
      ds1[[rvs]][ds1[[rvs]] == lf] <- 1
      util_warning(paste0("The levels of ", rvs, " (", mf, " and ", lf,
                          ") have been recoded to 0 and 1)"),
                   applicability_problem = FALSE)
    }

    # call logreg
    model <- glm(fmla, data = ds1, family = binomial(link = "logit"))
  }

  # variable of type integer with > 2 categories
  if (seldist$IsMultCat == 1 & seldist$NCategory <= 20) {
    if (is.factor(ds1[[rvs]])) {
      ds1[[rvs]] <- as.integer(ds1[[rvs]])
    }
    # call poisson model
    model <- glm(fmla, data = ds1, family = poisson(link = "log"))
  }

  # emmeans::emmeans appears not working correctly for the overall mean. Somehow
  # the package assumes an interaction term although there isn't.
  # browser()
  # call emmeans::emmeans ------------------------------------------------------
  res_df <- data.frame(emmeans::emmeans(model, group_vars, type = "response"))
  if (seldist$IsInteger == FALSE | seldist$IsInteger == TRUE &
      seldist$NCategory > 20) {
    res_df <- dplyr::rename(res_df, c("margins" = "emmean", "LCL" = "lower.CL",
                                      "UCL" = "upper.CL"))
  }
  if (seldist$IsInteger == 1 & seldist$IsMultCat == 0) {
    res_df <- dplyr::rename(res_df, c("margins" = "prob", "LCL" = "asymp.LCL",
                                      "UCL" = "asymp.UCL"))
  }
  if (seldist$IsMultCat == 1 & seldist$NCategory <= 20) {
    res_df <- dplyr::rename(res_df, c("margins" = "rate", "LCL" = "asymp.LCL",
                                      "UCL" = "asymp.UCL"))
  }

  # adjusted overall mean
  omv <- data.frame(emmeans::emmeans(model, "1", type = "response"))
  if (seldist$IsInteger == FALSE | seldist$IsInteger == TRUE &
      seldist$NCategory > 20) {
    omv <- dplyr::rename(omv, c("margins" = "emmean", "LCL" = "lower.CL",
                                "UCL" = "upper.CL"))
  }
  if (seldist$IsInteger == 1 & seldist$IsMultCat == 0) {
    omv <- dplyr::rename(omv, c("margins" = "prob", "LCL" = "asymp.LCL",
                                "UCL" = "asymp.UCL"))
  }
  if (seldist$IsMultCat == 1 & seldist$NCategory <= 20) {
    omv <- dplyr::rename(omv, c("margins" = "rate", "LCL" = "asymp.LCL",
                                "UCL" = "asymp.UCL"))
  }

  res_df$overall <- omv$margins

  ##############
  # THRESHOLDS #
  ##############
  # continuous data or count data with mor than 20 categories
  if (threshold_type %in% c("empirical", "none")) {
    if (seldist$IsInteger == FALSE | seldist$IsInteger == TRUE &
        seldist$NCategory > 20) {
      th <- sd(ds1[[rvs]])
      parn <- c(
        paste("-", threshold_value, "TH", sep = ""), "Mean",
        paste("+", threshold_value, "TH", sep = "")
      )
    }

    # binary data
    if (seldist$IsInteger == 1 & seldist$IsMultCat == 0) {
      th <- mean(ds1[[rvs]])
      th <- th * (1 - th)
      parn <- c(
        paste("-", threshold_value, "TH", sep = ""), "Prob.",
        paste("+", threshold_value, "TH", sep = "")
      )
    }

    # poisson
    if (seldist$IsMultCat == 1 & seldist$NCategory <= 20) {
      # th <- exp(as.numeric(coefficients(glm(v101 ~ 1,
      #    family = poisson(link="log"), data = expl_df))))
      th <- 1
      parn <- c(
        paste("-", threshold_value, "TH", sep = ""), "Mean",
        paste("+", threshold_value, "TH", sep = "")
      )
    }

    pars <- as.vector(c(
      omv$margins - threshold_value * th, omv$margins,
      omv$margins + threshold_value * th
    ))

    # store threshold
    res_df$threshold <- threshold_value

    # select abnormalties
    res_df$GRADING <- ifelse(res_df$margins < pars[1] |
                               res_df$margins > pars[3], 1, 0)
  } else if (threshold_type == "user") {
    th <- threshold_value
    # store threshold
    res_df$threshold <- th

    pars <- as.vector(c(th, th, th))
    if (seldist$IsInteger == 1 & seldist$IsMultCat == 0) {
      parn <- c("", paste0("Prob.=", threshold_value, sep = ""), "")
    } else {
      parn <- c("", paste0("Mean=", threshold_value, sep = ""), "")
    }

    # select abnormalties
    res_df$GRADING <- mapply(
      function(th, l, u) {
        ifelse(th >= l & th <= u, 0, 1)
      },
      res_df$threshold, res_df$LCL, res_df$UCL
    )
  }

  if (length(co_vars) > 0) {
    if (length(co_vars) < 10) {
      subtitle <- sprintf("adjusted for %s", paste0(co_vars, collapse = ", "))
    } else {
      subtitle <- sprintf("adjusted for %d variables", length(co_vars))
    }
  } else {
    subtitle <- ""
  }

  ############
  # Graphics #
  ############
  # use offset for annotation depending on variable scale
  offs <- ifelse(th <= 50, th / 10, th / 20)

  if (seldist$IsInteger == FALSE | seldist$IsInteger == TRUE &
      seldist$NCategory > 20) {
    # Plot 1: hybrid density/boxplot graph
    warn_code <- c("1" = "#B2182B", "0" = "#2166AC")


    p1 <- ggplot(data = ds1, aes_(x = ~ .data[[group_vars]],
                                  y = ~ .data[[rvs]])) +
      geom_violin(alpha = 0.9, draw_quantiles = TRUE, fill = "gray99") +
      geom_boxplot(width = 0.1, fill = "white", color = "gray", alpha = 0.5) +
      geom_pointrange(
        data = res_df, aes_(
          x = ~ factor(.data[[group_vars]]),
          y = ~margins,
          ymin = ~LCL,
          ymax = ~UCL,
          color = ~ as.factor(GRADING)
        ),
        shape = 18, size = 1,
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
      scale_colour_manual(values = warn_code)

    if (threshold_type != "none") {
      p1 <- p1 +
        geom_hline(yintercept = pars[2], color = "red") +
        geom_hline(yintercept = pars[-2], color = "red", linetype = 2)
    } else {
      p1 <- p1 +
        geom_hline(yintercept = pars[2], color = "red")
    }
  }

  if (seldist$IsInteger == 1 & seldist$NCategory <= 20) {
    warn_code <- c("1" = "#B2182B", "0" = "#2166AC")

    p1 <- ggplot(data = ds1, aes_(x = ~ .data[[group_vars]],
                                  y = ~ .data[[rvs]])) +
      geom_count(aes(alpha = 0.9), color = "gray") +
      geom_pointrange(
        data = res_df, aes_(
          x = ~ .data[[group_vars]],
          y = ~margins,
          ymin = ~LCL,
          ymax = ~UCL,
          color = ~ as.factor(GRADING)
        ),
        shape = 18, size = 1,
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
      scale_colour_manual(values = warn_code)
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
  get_y_scale <- ggplot(ds1, aes_(x = ~ .data[[rvs]])) +
    geom_density(alpha = 0.35)
  aty <- mean(range(ggplot_build(get_y_scale)$data[[1]]$y))


  p2 <- ggplot(ds1, aes_(y = ~ .data[[rvs]])) +
    geom_density(alpha = 0.35, orientation = "y") +
    theme_minimal() +
#    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          text = element_text(size = 16))
  if (threshold_type != "none") {
    p2 <-
      p2 +
      annotate(geom = "text", y = pars + offs, x = aty, label = parn) +
      geom_hline(yintercept = pars[2], color = "red") +
      geom_hline(yintercept = pars[-2], color = "red", linetype = 2)
  } else {
    p2 <-
      p2 +
      annotate(geom = "text", y = pars + offs, x = aty,
               label = c("", parn[2], "")) +
      geom_hline(yintercept = pars[2], color = "red")
  }


  # combine plots
  # and add the title
  res_plot <-
    p1 +
    p2 +
    plot_layout(nrow = 1,
                widths = c(5, 1)
                ) +
    plot_annotation(title = paste(group_vars, "margins in", rvs),
                    subtitle = subtitle)

  SummaryTable <- data.frame(Variables = rvs, GRADING =
                               as.numeric(any(res_df$GRADING > 0)))

  # length(unique(fit_df$GROUP)))
  # output
  return(list(
    SummaryData = res_df,
    SummaryTable = SummaryTable,
    SummaryPlot = util_set_size(res_plot, width_em = 25 +
                                  1.2 * length(unique(ds1[[group_vars]])),
                                height_em = 25)
  ))
}
