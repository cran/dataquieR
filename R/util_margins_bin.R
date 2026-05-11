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
#' @param no_overall_in_bin [logical] Suppress overall distribution in 'margins'
#'                                    figures for binary outcomes
#' @param no_geom_count_in_bin [logical] Suppress counts 'margins'
#'                                       figures for binary outcomes, so they
#'.                                      are not always including 0 and 1.
#'
#' @return A table and a matching plot.
#'
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot geom_pointrange geom_text element_blank element_text geom_density coord_flip annotate ggplot_build theme_minimal labs theme scale_colour_manual geom_count aes geom_vline geom_hline
#' @import patchwork
#'
#' @noRd
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
                                         dataquieR.acc_margins_num_default),
                             no_overall_in_bin =
                               getOption("dataquieR.no_overall_in_bin",
                                         dataquieR.no_overall_in_bin_default),
                             no_geom_count_in_bin =
                               getOption("dataquieR.no_geom_count_in_bin",
                                      dataquieR.no_geom_count_in_bin_default)) {
  # preps and checks -----------------------------------------------------------
  # to avoid "no visible binding for global variable ‘sample_size’"

  if (no_geom_count_in_bin &&  # so it does not scale to 0/1
      !no_overall_in_bin) { # but we want to have the overall distribution
    util_message("Cannot have %s = %s and %s = %s, setting %s to %s",
                 sQuote("no_geom_count_in_bin"),
                 dQuote(TRUE),
                 sQuote("no_overall_in_bin"),
                 dQuote(FALSE),
                 sQuote("no_overall_in_bin"),
                 dQuote(TRUE)
    )
    no_overall_in_bin <- TRUE
  }

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
  .ds00 <- ds1[, c("resp_var_adj", group_vars), drop = FALSE]
  if (all(!is.finite(.ds00$resp_var_adj))) {
    util_error("No data left.")
  }

  if (no_geom_count_in_bin) { # FIXME: Elisa had some corner cases, where this did not match the overall distribution any more.
    y_lims <- c(
      max(0, min(res_df_plot$LCL, na.rm = TRUE)),
      min(1, max(res_df_plot$UCL, na.rm = TRUE))
    )
    ### extract 0/1-values and group
    # tmp_count_df <- data.frame(
    #   .grp = .ds00[[group_vars]],
    #   .y = round(.ds00[["resp_var_adj"]])
    # )
    #
    # ## only 0/1
    # tmp_count_df <- tmp_count_df[!is.na(tmp_count_df$.grp) &
    #                                !is.na(tmp_count_df$.y) &
    #                                tmp_count_df$.y %in% c(0, 1), , drop = FALSE]
    #
    # ## frequencies
    # tab <- table(tmp_count_df$.grp, tmp_count_df$.y)
    #
    # ## ensure coluns 0 and 1
    # tab_df <- data.frame(
    #   .grp = rownames(tab),
    #   n0 = if ("0" %in% colnames(tab)) tab[, "0"] else 0,
    #   n1 = if ("1" %in% colnames(tab)) tab[, "1"] else 0,
    #   row.names = NULL,
    #   check.names = FALSE
    # )
    #
    # ## add to res_df_plot
    # label_df <- merge(
    #   data.frame(.grp = res_df_plot[[group_vars]], res_df_plot, check.names = FALSE),
    #   tab_df,
    #   by = ".grp",
    #   all.x = TRUE,
    #   sort = FALSE
    # )
    #
    # ## NAs -> 0
    # label_df$n0[is.na(label_df$n0)] <- 0
    # label_df$n1[is.na(label_df$n1)] <- 0
    #
    # ## create labels
    # label_df$label <- paste0(label_df$n0, "+", label_df$n1)
    gcnt <- NULL
    # summary_ds$sample_size <- paste0(summary_ds$sample_size,
    #                                  "=\n",
    #                                 label_df$label)
  } else {
    y_lims <- c(0, 1)
    gcnt <- geom_count(aes(alpha = 0.9), color = "gray")
  }

  p1 <- util_create_lean_ggplot(ggplot(data = .ds00,
                                       aes(x = .data[[group_vars]],
                                           y = round(.data[["resp_var_adj"]]))) +
                                  gcnt +
                                  util_geom_pointrange_robust(data = res_df_plot, aes(
                                    x = .data[[group_vars]],
                                    y = margins,
                                    ymin = LCL,
                                    ymax = UCL,
                                    color = as.factor(GRADING)),#, n = sample_size
                                    shape = 18,
                                    linewidth = 1,
                                    inherit.aes = FALSE,
                                    size = .5) +
                                  theme_minimal() +
                                  labs(x = "", y = "") +
                                  theme(
                                    legend.position = "none",
                                    legend.title = element_blank(),
                                    text = element_text(size = 16),
                                    axis.text.x = element_text(angle = 90,
                                                               vjust = 0.5,
                                                               hjust = 1),
                                    plot.margin = ggplot2::unit(c(2, 0, 2, 0),
                                                                "mm")) +
                                  scale_colour_manual(values = warn_code) +
                                  ggplot2::expand_limits(y = y_lims),
                                .ds00 = .ds00,
                                group_vars = group_vars,
                                res_df_plot = res_df_plot,
                                warn_code = warn_code,
                                y_lims = y_lims,
                                gcnt = gcnt)

  p1 <- util_create_lean_ggplot(p1 +
                                  ggplot2::coord_cartesian(ylim = y_lims,
                                                            clip = "off"),
                                p1 = p1,
                                y_lims = y_lims)

    if (include_numbers_in_figures) {
      p1 <- util_create_lean_ggplot(p1 +
                                      geom_text(data = summary_ds,
                                                aes(x = .data[[group_vars]],
                                                    y = Inf,
                                                    label = sample_size),
                                                inherit.aes = FALSE,
                                                hjust = 0.5,
                                                vjust = -0.2,
                                                angle = 90) +
                                      annotate("text",
                                               x = 0.5,
                                               y = Inf,
                                               label = "N",
                                               vjust = -0.2) +
                                      theme(plot.margin = ggplot2::unit(c(8, 0, 2, 0),
                                                                        "mm")),
                                    p1 = p1,
                                    summary_ds = summary_ds,
                                    group_vars = group_vars,
                                    y_lims = y_lims,
                                    sample_size = sample_size)
    }

  if (threshold_type != "none") {
    p1 <- util_create_lean_ggplot(p1 +
                                    geom_hline(yintercept = pars[2],
                                               color = "red") +
                                    geom_hline(yintercept = pars[-2],
                                               color = "red",
                                               linetype = 2),
                                  p1 = p1,
                                  pars = pars)

  } else {
    p1 <- util_create_lean_ggplot(p1 +
                                    geom_hline(yintercept = pars[2],
                                               color = "red"),
                                  p1 = p1,
                                  pars = pars)
  }

  # Plot 2: overall distributional plot flipped on y-axis of plot 1
  .ds01 <- ds1[, "resp_var_adj", drop = FALSE]
  get_y_scale <-
    util_create_lean_ggplot(ggplot(.ds01,
                                   aes(x = round(.data[["resp_var_adj"]]))) +
                              geom_density(alpha = 0.35),
                            .ds01 = .ds01)

  build <- ggplot2::ggplot_build(get_y_scale)
  data1 <- util_gg_get(build, "data")[[1]]
  yvals <- util_gg_get(data1, "y")

  aty <- mean(range(yvals))


  p2 <- util_create_lean_ggplot(ggplot(.ds01,
                                       aes(round(.data[["resp_var_adj"]]))) +
                                  geom_density(alpha = 0.35) +
                                  coord_flip() + # util_lazy_add_coord(p, fli)
                                  theme_minimal() +
                                  labs(x = NULL, y = NULL) +
                                  ggplot2::xlim(c(min(min(round(.ds01)), pars),
                                                  max(max(round(.ds01)) + 0.3,
                                                      pars + offs))) +
                                  theme(axis.text.y = element_blank(),
                                        axis.text.x = element_blank(),
                                        text = element_text(size = 16),
                                        plot.margin =
                                          ggplot2::unit(c(0, 2, 0, 2), "mm")) +
                                  ggplot2::expand_limits(x = y_lims),
                                .ds01 = .ds01,
                                pars = pars,
                                offs = offs,
                                y_lims = y_lims)

  if (threshold_type != "none") {
    p2 <- util_create_lean_ggplot(p2 +
                                    annotate(geom = "text",
                                             x = pars + offs,
                                             y = aty,
                                             label = parn) +
                                    geom_vline(xintercept = pars[2],
                                               color = "red") +
                                    geom_vline(xintercept = pars[-2],
                                               color = "red",
                                               linetype = 2),
                                  p2 = p2,
                                  pars = pars,
                                  offs = offs,
                                  aty = aty,
                                  parn = parn)

  } else {
    p2 <- util_create_lean_ggplot(
      p2 +
        annotate(geom = "text",
                 x = pars + offs,
                 y = aty,
                 label = c("", parn[2], "")) +
        geom_vline(xintercept = pars[2], color = "red"),
      p2 = p2,
      pars = pars,
      offs = offs,
      aty = aty,
      parn = parn)
  }

  if (is.null(caption)) {
    caption <- ""
  }

  # combine plots and add the title
  if (no_overall_in_bin) {
    p2 <- NULL
    my_plot_layout <- plot_layout(nrow = 1)
  } else {
    my_plot_layout <- plot_layout(nrow = 1,
                                  widths = c(5, 1))
  }

  res_plot <- # TODO: For all patchwork-calls, add information to reproduce the layout in plot.ly
    util_create_lean_ggplot(
      p1 +
        p2 +
        my_plot_layout +
        plot_annotation(title = title,
                        subtitle = adjusted_hint,
                        caption = caption),
      p1 = p1,
      p2 = p2,
      title = title,
      adjusted_hint = adjusted_hint,
      caption = caption,
      my_plot_layout = my_plot_layout)

  # output ---------------------------------------------------------------------
  return(list("plot_data" = res_df, "plot" = res_plot))
}
