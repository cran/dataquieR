#' Utility function to create a plot similar to the margins plots for nominal variables
#'
#' This function is still under development. It uses the `nnet` package to
#' compute multinomial logistic regression models.
#'
#' @param resp_vars  [variable] the name of the nominal measurement variable
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
#' @param adjusted_hint [character] hint, if adjusted for `co_vars`
#' @param title [character] title for the plot
#' @param sort_group_var_levels [logical] Should the levels of the grouping
#'                        variable be sorted descending by the number of
#'                        observations (in the figure)?
#'
#' @return A table and a matching plot.
#'
#' @importFrom ggplot2 ggplot geom_point geom_pointrange
#'                     theme_minimal theme xlab ylab ggtitle
#'                     scale_colour_manual aes facet_grid
#' @importFrom stats p.adjust
#'
#' @noRd
util_margins_nom <- function(resp_vars = NULL, group_vars = NULL, co_vars = NULL,
                             min_obs_in_subgroup = 5, min_obs_in_cat = 5,
                             ds1, label_col,
                             adjusted_hint = "",
                             title = "",
                             sort_group_var_levels =
                               getOption("dataquieR.acc_margins_sort",
                                         dataquieR.acc_margins_sort_default)) {
  # preps and checks -----------------------------------------------------------
  util_ensure_suggested("nnet")
  # ensure that the response variable is not constant
  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant (after data preparation).",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  # ensure that there are enough observations for the model
  count_nom <- util_table_of_vct(ds1[[resp_vars]])
  if (any(count_nom[, 2] < min_obs_in_cat)) {
    util_error("Not enough observations per category (after data preparation).")
  }
  count_nom <- util_table_of_vct(ds1[[group_vars]])
  if (any(count_nom[, 2] < min_obs_in_subgroup)) {
    util_error("Not enough observations per group (after data preparation).")
  }
  # ensure that the model supports the specified covariates
  if (length(co_vars) > 0) {
    covar_prop <- util_dist_selection(ds1[, co_vars, drop = FALSE])
    covar_fact <- setNames(nm = co_vars,
                           vapply(ds1[, co_vars, drop = FALSE], FUN.VALUE = logical(1),
                           FUN = is.factor))
    covar_excl <- union(which(covar_prop$NCategory <= 2),
                        which(!covar_fact))
    if (length(covar_excl) > 0) { # TODO: work in progress!
      util_error(paste("Adjusting for covariates is currently",
                       "only supported for factor variables with",
                       "2 or more levels"))
    }
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

  # multinomial logistic regression
  invisible(capture.output({
    model <- suppressMessages(nnet::multinom(fmla, data = ds1))
  }))
  nom_res_fmla <- as.formula(paste0(
    "~ ",
    util_bQuote(group_vars),
    " | ",
    util_bQuote(resp_vars)
  ))
  nom_int_fmla <- as.formula(paste0(
    "~ 1 | ",
    util_bQuote(resp_vars)
  ))

  res_df <- data.frame(emmeans::emmeans(model, nom_res_fmla),
                       check.names = FALSE)

  summary_ds <- as.data.frame(
    dplyr::summarize(
      dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
                         unname(c(group_vars, resp_vars)), .drop = FALSE),
      sample_size = dplyr::n()))

  res_df <- merge(res_df, summary_ds, by = c(group_vars, resp_vars),
                  all.x = TRUE)
  res_df <- dplyr::rename(res_df, c("margins" = "prob", "LCL" = "lower.CL",
                                    "UCL" = "upper.CL"))

  # adjusted overall mean
  omv <- data.frame(emmeans::emmeans(model, nom_int_fmla))
  omv <- dplyr::rename(omv, c("margins" = "prob", "LCL" = "lower.CL",
                              "UCL" = "upper.CL"))

  # detect deviations ----------------------------------------------------------
  # proposal: use Wald tests (here z-tests, two-sided) to detect
  # outlying group var levels
  nom_test <- merge(res_df, omv[, c(resp_vars, "margins")],
                    by = resp_vars, all = TRUE, suffixes = c("", ".overall"))
  test_df <- nom_test[, c(resp_vars, group_vars)]
  test_df$z <- (nom_test$margins - nom_test$margins.overall)/nom_test$SE
  test_df$p <- (1 - pnorm(abs(test_df$z), 0, 1)) * 2
  # adjust p values for multiple testing
  test_df$GRADING <- as.numeric(p.adjust(test_df$p, method = "fdr") < 0.05)
  res_df <- merge(res_df, test_df[, c(resp_vars, group_vars, "GRADING")],
                  by = c(resp_vars, group_vars), all = TRUE)

  # figure ---------------------------------------------------------------------
  omv[, group_vars] <- "overall"
  nom_plot_data <- merge(res_df, omv,
                         by = c(resp_vars, group_vars, "margins", "SE", "df",
                                "LCL", "UCL"),
                         all = TRUE)
  nom_plot_data$GRADING[which(is.na(nom_plot_data$GRADING))] <- "-1"

  warn_code <- c("1" = "#B2182B", "0" = "#2166AC", "-1" = "grey20")

  if (sort_group_var_levels) {
    # order levels of the grouping variable by number of observations
    tbl_gr <- util_table_of_vct(ds1[[group_vars]]) %>%
      dplyr::arrange(dplyr::desc(.data$Var1)) %>%
      dplyr::arrange(.data$Freq)
    nom_plot_data[[group_vars]] <- factor(nom_plot_data[[group_vars]],
                                          levels = c(as.character(tbl_gr$Var1), "overall"))
    res_df <- res_df %>%
      dplyr::arrange(.data[[resp_vars]],
                     match(.data[[group_vars]], rev(as.character(tbl_gr$Var1))))
  } else { # original order in plot, but from top of the y-axis to the bottom
    nom_plot_data[[group_vars]] <- factor(nom_plot_data[[group_vars]],
                                          levels = c(rev(levels(ds1[[group_vars]])), "overall"))
  }

  nom_plot_data[["facet"]] <- nom_plot_data[[resp_vars]] # make facet findable to prevent util_compress_ggplots_in_res from deleting this column

  res_plot <- util_create_lean_ggplot(ggplot(nom_plot_data,
                                             aes(x = .data[["margins"]],
                                                 y = .data[[group_vars]],
                                                 col = .data[["GRADING"]])) +
                                        geom_pointrange(aes(xmin = .data[["LCL"]],
                                                            xmax = .data[["UCL"]])) +
                                        geom_point() +
                                        scale_color_manual(values = warn_code,
                                                           guide = "none") +
                                        facet_grid(.data[["facet"]] ~ .) +
                                        theme_minimal() +
                                        xlab("probability") +
                                        ylab("") +
                                        theme(strip.text = element_text(size = 14)) +
                                        ggtitle(label = title,
                                                subtitle = adjusted_hint),
                                      nom_plot_data = nom_plot_data,
                                      group_vars = group_vars,
                                      warn_code = warn_code,
                                      title = title,
                                      adjusted_hint = adjusted_hint)

  return(list("plot_data" = res_df, "plot" = res_plot))
}
