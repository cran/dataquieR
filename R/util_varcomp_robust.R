#' Utility function to compute the rank intraclass correlation
#'
#' This implementation uses the package `rankICC` to compute the rank
#' intraclass correlation, a nonparametric version of the ICC (Tu et al., 2023).
#' In contrast to model-based ICC approaches, it is less sensitive to outliers
#' and skewed distributions. It can be applied to variables with an ordinal,
#' interval or ratio scale. However, it is not possible to adjust for
#' covariables with this approach. The calculated ICC can become negative,
#' like Fisher's ICC.
#'
#' @param resp_vars the name of the response variable
#' @param group_vars the name of the grouping variable
#' @param study_data the data frame that contains the measurements
#' @param meta_data the data frame that contains metadata attributes of
#'                  study data
#' @param min_obs_in_subgroup the minimum number of observations that is
#'                            required to include a subgroup (level) of the
#'                            grouping variable (`group_vars`) in the analysis.
#'                            Subgroups with fewer observations are excluded.
#' @param min_subgroups the minimum number of subgroups (levels) of the
#'                      grouping variable (`group_vars`). If the variable has
#'                      fewer subgroups, the analysis is not performed.
#' @param label_col the name of the column in the metadata with labels of
#'                  variables
#'
#' @return a vector from rankICC::rankICC
#'
util_varcomp_robust <- function(resp_vars = NULL, group_vars = NULL,
                                study_data = study_data, meta_data = meta_data,
                                min_obs_in_subgroup = 10, min_subgroups = 5,
                                label_col = NULL) {
  # preps ----------------------------------------------------------------------
  util_ensure_suggested("rankICC")
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE)

  util_correct_variable_use("resp_vars",
                            allow_all_obs_na = FALSE,
                            need_scale = "ordinal | interval | ratio")
  util_correct_variable_use("group_vars",
                            allow_all_obs_na = FALSE,
                            need_type = "!float",
                            need_scale = "nominal | ordinal")

  util_expect_scalar(min_obs_in_subgroup,
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  util_expect_scalar(min_subgroups,
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  ds1 <- ds1[, c(resp_vars, group_vars), drop = FALSE]
  ds1 <- ds1[complete.cases(ds1), , drop = FALSE]

  # ensure that included levels of the grouping variable have the specified
  # minimum number of observations
  check_df <- util_table_of_vct(ds1[[group_vars]])
  critical_levels <- levels(check_df$Var1)[check_df$Freq <
                                             min_obs_in_subgroup]
  if (length(critical_levels) > 0) {
    util_message("Levels %s were excluded due to less than %d observations.",
                 paste0(c(vapply(head(critical_levels, 10), dQuote, ""),
                          if (length(critical_levels) <= 10)
                            character(0) else "..."),
                        collapse = ", "),
                 min_obs_in_subgroup,
                 applicability_problem = FALSE)
    # exclude levels with too few observations
    ds1 <- ds1[!(ds1[[group_vars]] %in% critical_levels), ]
    levels(ds1[[group_vars]])[
      which(levels(ds1[[group_vars]]) %in% critical_levels)] <- NA
  }
  ds1 <- ds1[complete.cases(ds1), , drop = FALSE]

  # ensure that the grouping variable has the specified minimum number of levels
  check_df <- util_table_of_vct(ds1[[group_vars]])
  if (length(check_df[, 1]) < min_subgroups) {
    util_error("%d < %d levels in %s. Will not compute ICCs for %s.",
               length(check_df[, 1]),
               min_subgroups,
               dQuote(group_vars),
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  # ensure that the response variable is not constant
  if (length(unique(ds1[[resp_vars]])) < 2) {
    util_error("The response variable is constant after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  res <- rankICC::rankICC(x = ds1[[resp_vars]],
                          cluster = ds1[[group_vars]],
                          weights = "clusters")

  return(res)
}
