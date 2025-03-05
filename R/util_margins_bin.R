#' Utility function to create a margins plot for binary variables
#'
#' @param resp_vars  [variable] the name of the binary measurement variable
#' @param group_vars [variable] the name of the observer, device or
#'                              reader variable
#' @param co_vars [variable list] a vector of covariables, e.g. age and sex for
#'                              adjustment
#' @param min_obs_in_subgroup [integer] from=0. This optional argument specifies
#'                       the minimum number of observations that is required to
#'                       include a subgroup (level) of the `group_var` in the
#'                       analysis.
#' @param min_obs_in_cat [integer] This optional argument specifies the minimum
#'                       number of observations that is required to include
#'                       a category (level) of the outcome (`resp_vars`) in
#'                       the analysis.
#' @param ds1 [data.frame] the data frame that contains the measurements, after
#'                       replacing missing value codes by `NA`, excluding
#'                       inadmissible values and transforming categorical
#'                       variables to factors.
#' @param label_col [variable attribute] the name of the column in the metadata
#'                       with labels of variables
#' @param threshold_type [enum] empirical | user | none. See `acc_margins`.
#' @param threshold_value [numeric] see `acc_margins`
#' @param caption [string] a caption for the plot (optional, typically used to
#'                       report the coding of cases and control group)
#' @param adjusted_hint [character] hint, if adjusted for `co_vars`
#' @param title [character] title for the plot
#' @param sort_group_var_levels [logical] Should the levels of the grouping
#'                        variable be sorted descending by the number of
#'                        observations (in the figure)?
#' @param include_numbers_in_figures [logical] Should the figure report the
#'                        number of observations for each level of the grouping
#'                        variable?
#'
#' @return A table and a matching plot.
#'
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot geom_pointrange geom_text
#'                     element_blank element_text
#'                     geom_density coord_flip annotate
#'                     ggplot_build theme_minimal labs theme scale_colour_manual
#'                     geom_count aes geom_vline geom_hline
#' @import patchwork
#'
util_margins_bin <- function(resp_vars = NULL, group_vars = NULL, co_vars = NULL,
                             threshold_type = NULL, threshold_value,
                             min_obs_in_subgroup = 5, min_obs_in_cat = 5,
                             caption = NULL, ds1, label_col,
                             adjusted_hint = "",
                             title = "",
                             sort_group_var_levels =
                               getOption("dataquieR.acc_margins_sort",
                                         dataquieR.acc_margins_sort_default),
                             include_numbers_in_figures =
                               getOption("dataquieR.acc_margins_num",
                                         dataquieR.acc_margins_num_default)) {
  # preps and checks -----------------------------------------------------------
  # to avoid "no visible binding for global variable ‘sample_size’"
  sample_size <- NULL
  # ensure that the response variable is not constant
  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant (after data preparation).",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  # ensure that the response variable is binary
  if (var_prop$NDistinct != 2) {
    util_error("The response variable is not binary.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  # ensure that there are enough observations for the model
  count_bin <- util_table_of_vct(ds1[[resp_vars]])
  if (any(count_bin[, 2] < min_obs_in_cat)) {
    util_error("Not enough observations per category (after data preparation).")
  }
  count_bin <- util_table_of_vct(ds1[[group_vars]])
  if (any(count_bin[, 2] < min_obs_in_subgroup)) {
    util_error("Not enough observations per group (after data preparation).")
  }

  # modelling ------------------------------------------------------------------
  # if no co_vars are defined for adjustment only the intercept is modelled
  if (length(co_vars) == 0) {
    co_vars <- "1"
    co_vars_bQ <- co_vars
  } else {
    co_vars_bQ <- util_bQuote(co_vars)
  }

  # build model formula
  fmla <- as.formula(paste0(
    paste0(util_bQuote(resp_vars), "~"),
    paste0(
      paste0(co_vars_bQ, collapse = " + "),
      " + ",
      util_bQuote(group_vars)
    )
  ))

  suppressWarnings({
    model <- glm(fmla, data = ds1, family = binomial(link = "logit"))
  })
  res_df <- data.frame(emmeans::emmeans(model, group_vars, type = "response"),
                       check.names = FALSE)

  # adjust for covariates, if needed
  ds1$resp_var_adj <-
    # estimated mean for each level of the grouping variable
    res_df$prob[match(ds1[[group_vars]], res_df[, group_vars])] +
    # residuals: original value of the response variable - fitted value
    ds1[[resp_vars]] - model$fitted.values

  summary_ds <- as.data.frame(
    dplyr::summarize(
      dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
                         unname(group_vars)),
      sample_size = dplyr::n()))

  res_df <- merge(res_df, summary_ds, by = group_vars, all.x = TRUE)
  res_df <- dplyr::rename(res_df, c("margins" = "prob", "LCL" = "asymp.LCL",
                                    "UCL" = "asymp.UCL"))

  # adjusted overall mean
  omv <- data.frame(emmeans::emmeans(model, "1", type = "response"))
  omv <- dplyr::rename(omv, c("margins" = "prob", "LCL" = "asymp.LCL",
                              "UCL" = "asymp.UCL"))
  res_df$overall <- omv$margins # TODO: never used?

  # thresholds -----------------------------------------------------------------
  if (threshold_type %in% c("empirical", "none")) {
    th <- mean(ds1[["resp_var_adj"]]) # TODO: use estimate in 'omv'?
    th <- th * (1 - th) # TODO: Is variance here a good estimator? Maybe align
    # with measurements of deviation for nominal variables?
    parn <- c(
      paste("-", threshold_value, "TH", sep = ""), "Prob.",
      paste("+", threshold_value, "TH", sep = "")
    )
    pars <- as.vector(c(
      omv$margins - threshold_value * th, omv$margins,
      omv$margins + threshold_value * th
    ))

    res_df$threshold <- threshold_value
    res_df$GRADING <- ifelse(res_df$margins < pars[1] |
                               res_df$margins > pars[3], 1, 0)
  } else if (threshold_type == "user") {
    th <- threshold_value
    parn <- c("", paste0("Prob.=", threshold_value, sep = ""), "")
    pars <- as.vector(c(th, th, th))

    res_df$threshold <- th
    res_df$GRADING <- mapply(
      function(th, l, u) {
        ifelse(th >= l & th <= u, 0, 1)
      },
      res_df$threshold, res_df$LCL, res_df$UCL
    )
  }

  # figure ---------------------------------------------------------------------
  # drop confidence intervals for group_var levels that have zero variance to
  # ensure the readability of the figures
  res_df_plot <- res_df[, c(group_vars, "margins", "LCL", "UCL",
                            "GRADING", "sample_size")]
  gr_var <- tapply(ds1[["resp_var_adj"]], ds1[[group_vars]], var)
  gr_zero <- gr_var < sqrt(.Machine$double.eps)
  if (any(gr_zero)) {
    gr_ci_excl <- names(gr_var)[gr_zero]
    res_df_plot$LCL[as.character(res_df_plot[, group_vars]) %in% gr_ci_excl] <- NA
    res_df_plot$UCL[as.character(res_df_plot[, group_vars]) %in% gr_ci_excl] <- NA
  }

  # use offset for annotation depending on variable scale
  offs <- ifelse(th <= 50, th / 10, th / 20)

  warn_code <- c("1" = "#B2182B", "0" = "#2166AC")

  if (sort_group_var_levels) {
    # order levels of the grouping variable by number of observations
    tbl_gr <- util_table_of_vct(ds1[[group_vars]]) %>%
      dplyr::arrange(dplyr::desc(.data$Freq))
    ds1[[group_vars]] <- factor(ds1[[group_vars]],
                                levels = as.character(tbl_gr$Var1))
    res_df <- res_df[match(levels(ds1[[group_vars]]),
                           as.character(res_df[, group_vars])), ]
  }

  # Plot 1: hybrid density/boxplot graph
  p1 <- ggplot(data = ds1[, c("resp_var_adj", group_vars), drop = FALSE],
               aes(x = .data[[group_vars]],
                   y = round(.data[["resp_var_adj"]]))) +
    geom_count(aes(alpha = 0.9), color = "gray") +
    geom_pointrange(
      data = res_df_plot, aes(
        x = .data[[group_vars]],
        y = margins,
        ymin = LCL,
        ymax = UCL,
        color = as.factor(GRADING)#, n = sample_size
      ),
      shape = 18, linewidth = 1,
      inherit.aes = FALSE,
      fatten = 5
    ) +
    theme_minimal() +
    labs(x = "", y = "") +
    theme(
      legend.position = "none", legend.title = element_blank(),
      text = element_text(size = 16),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.margin = ggplot2::unit(c(2, 0, 2, 0), "mm")
    ) +
    scale_colour_manual(values = warn_code) +
    ggplot2::expand_limits(y = c(0, 1))

    if (include_numbers_in_figures) {
      p1 <- p1 +
        geom_text(data = summary_ds,
                  aes(x = summary_ds[, group_vars],
                      y = max(round(ds1[, "resp_var_adj", drop = FALSE])) + 0.3,
                      label = sample_size),
                  hjust = 0.5, angle = 90) +
        annotate("text",
                 x = 0.5,
                 y = max(round(ds1[, "resp_var_adj", drop = FALSE])) + 0.3,
                 label = "N")
    }

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

  # Plot 2: overall distributional plot flipped on y-axis of plot 1
  get_y_scale <- ggplot(ds1[, "resp_var_adj", drop = FALSE],
                        aes(x = round(.data[["resp_var_adj"]]))) +
    geom_density(alpha = 0.35)
  aty <- mean(range(ggplot_build(get_y_scale)$data[[1]]$y))

  p2 <- ggplot(ds1[, "resp_var_adj", drop = FALSE],
               aes(round(.data[["resp_var_adj"]]))) +
    geom_density(alpha = 0.35) +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    ggplot2::xlim(c(min(min(round(ds1[, "resp_var_adj", drop = FALSE])), pars),
                    max(max(round(ds1[, "resp_var_adj", drop = FALSE])) + 0.3,
                        pars + offs))) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          text = element_text(size = 16),
          plot.margin = ggplot2::unit(c(0, 2, 0, 2), "mm")) +
    ggplot2::expand_limits(x = c(0, 1))

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

  if (is.null(caption)) {
    caption <- ""
  }

  # combine plots and add the title
  res_plot <- # TODO: For all patchwork-calls, add information to reproduce the layout in plot.ly
    p1 +
    p2 +
    plot_layout(nrow = 1,
                widths = c(5, 1)
    ) +
    plot_annotation(title = title,
                    subtitle = adjusted_hint,
                    caption = caption)

  # output ---------------------------------------------------------------------
  return(list("plot_data" = res_df, "plot" = res_plot))
}
