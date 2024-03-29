#' Support function to identify the levels of a process variable with minimum
#' number of observations
#'
#' @description
#' utility function to subset data based on minimum number of observation per
#' level
#'
#' @details
#' This functions removes observations having less than `min_obs_in_subgroup`
#' distinct values in a group variable, e.g. blood pressure measurements
#' performed by an examiner having less than e.g. 50 measurements done. It
#' displays a warning, if samples/rows are removed and returns the modified
#' study data frame.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param group_vars [variable list] the name grouping variable
#' @param min_obs_in_subgroup [integer] optional argument if a "group_var" is
#'                                      used. This argument specifies the
#'                                      minimum no. of observations that is
#'                                      required to include a subgroup (level)
#'                                      of the "group_var" in the analysis.
#'                                      Subgroups with less observations are
#'                                      excluded. The default is 30.
#'
#' @return a data frame with:
#'   - a subsample of original data
#' @export
prep_min_obs_level <- function(study_data, group_vars, min_obs_in_subgroup) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  if (missing(group_vars) || !is.character(group_vars)) {
    util_error("%s is required to be a character(1) argument.",
               dQuote("group_vars"), applicability_problem = TRUE)
  }

  if (length(group_vars) > 1) {
    util_message("Subsets based only on one variable possible.",
                 applicability_problem = TRUE)
    group_vars <- group_vars[1]
  }

  if (length(group_vars) != 1) {
    util_error("%s is required to name exactly one variable.",
                 dQuote("group_vars"), applicability_problem = TRUE)
  }

  if (!(group_vars %in% colnames(study_data))) {
    util_error("%s is not a variable.",
               paste(sQuote("group_vars"), "=", dQuote(group_vars)),
               applicability_problem = TRUE)
  }

  if (missing(min_obs_in_subgroup) || length(min_obs_in_subgroup) != 1 ||
      is.na(min_obs_in_subgroup)) {
    if (!.called_in_pipeline || !missing(min_obs_in_subgroup))
      util_message(
        c("argument %s was missing, not of length 1 or NA, setting to its",
          "default, 30"),
        dQuote("min_obs_in_subgroup"),
        applicability_problem = TRUE)
    min_obs_in_subgroup <- 30
  }

  X <- util_table_of_vct(study_data[[group_vars]])

  if (suppressWarnings(!is.finite(min(X[["Freq"]])))) {
    util_error("For %s, observations cannot be counted.",
               paste(sQuote("group_vars"), "=", dQuote(group_vars)))
  }

  # too few observations in >1 level of group_vars
  if (min(X[, 2]) < min_obs_in_subgroup) {
    critical_levels <- levels(X$Var1)[X$Freq < min_obs_in_subgroup]
    util_message(
      "The following levels: %s have < %d observations and are disregarded",
      paste0(dQuote(critical_levels), collapse = ", "),
      min_obs_in_subgroup,
      applicability_problem = FALSE
    )
    subsdf <- study_data[!(study_data[[group_vars]] %in% critical_levels), ]
  } else {
    subsdf <- study_data
  }


  return(subsdf)
}
