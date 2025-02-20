#' Check data for observer levels
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param group_vars [variable] the name of the observer, device or reader
#'                              variable
#' @param min_obs_in_subgroup [integer] from=0. optional argument if
#'                                  `group_vars` are used. This argument
#'                                  specifies the minimum number of observations
#'                                  that is required to include a subgroup
#'                                   (level) of the group variable named by
#'                                    `group_vars` in the analysis. Subgroups
#'                                  with fewer observations are excluded.
#' @param max_obs_in_subgroup [integer] from=0. optional argument if
#'                                  `group_vars` are used. This argument
#'                                  specifies the maximum number of observations
#'                                  that is required to include a subgroup
#'                                   (level) of the group variable named by
#'                                    `group_vars` in the analysis. Subgroups
#'                                  with more observations are excluded.
#' @param min_subgroups [integer] from=0. optional argument if a "group_var" is
#'                                        used. This argument specifies the
#'                                        minimum no. of subgroups (levels)
#'                                        included "group_var". If the variable
#'                                        defined in "group_var" has fewer
#'                                        subgroups it is split for analysis.
#' @param max_subgroups [integer] from=0. optional argument if a "group_var" is
#'                                        used. This argument specifies the
#'                                        maximum no. of subgroups (levels)
#'                                        included "group_var". If the variable
#'                                        defined in "group_var" has more
#'                                        subgroups it is split for analysis.
#'
#' @return modified study data frame
#' @examples
#' \dontrun{
#' study_data <- prep_get_data_frame("study_data")
#' meta_data <- prep_get_data_frame("meta_data")
#' prep_prepare_dataframes(.label_col = LABEL)
#' util_check_group_levels(ds1, "CENTER_0")
#' dim(util_check_group_levels(ds1, "USR_BP_0", min_obs_in_subgroup = 400))
#' }
#'
#'
#' @seealso [prep_min_obs_level]
#' @family data_management
#' @concept robustness
#' @keywords internal
util_check_group_levels <- function(study_data,
                                    group_vars,
                                    min_obs_in_subgroup = -Inf,
                                    max_obs_in_subgroup = +Inf,
                                    min_subgroups = -Inf,
                                    max_subgroups = +Inf) {
  # util_correct_variable_use(group_vars) cannot work, here, should be called in the calling function, anyway
  util_expect_scalar(min_obs_in_subgroup, check_type = is.numeric)
  util_expect_scalar(max_obs_in_subgroup, check_type = is.numeric)
  util_expect_scalar(min_subgroups, check_type = is.numeric)
  util_expect_scalar(max_subgroups, check_type = is.numeric)
  util_stop_if_not(max_obs_in_subgroup >= min_obs_in_subgroup)
  util_stop_if_not(max_subgroups >= min_subgroups)
  l <- split(study_data, as.factor(study_data[, group_vars]))
  obs_per_subgroup <- vapply(l, nrow, FUN.VALUE = integer(1))
  n_subgroups <- length(l)
  if (is.finite(min_obs_in_subgroup)) {
    which_too_few <- which(obs_per_subgroup < min_obs_in_subgroup)
  } else {
    which_too_few <- integer(0)
  }
  if (is.finite(max_obs_in_subgroup)) {
    which_too_many <- which(obs_per_subgroup > max_obs_in_subgroup)
  } else {
    which_too_many <- integer(0)
  }
  which_valid <- setdiff(
    seq_along(obs_per_subgroup),
    union(which_too_few, which_too_many)
  )
  n_subgroups <- length(which_valid)

  modified_study_data <- do.call(rbind.data.frame, l[which_valid])
  if (length(which_valid) < length(l)) {
    util_message(
      c("Discarding %d observations (%d from %d levels of %s)",
        "because of too few/many observations",
        "per group"),
      nrow(study_data) - nrow(modified_study_data),
      length(l) - length(which_valid),
      length(l),
      dQuote(group_vars)
    )
  }

  if (is.finite(min_subgroups) && n_subgroups < min_subgroups) {
    util_error("Too few subgroups (%d < %d)", n_subgroups, min_subgroups)
  }
  if (is.finite(max_subgroups) && n_subgroups > max_subgroups) {
    attr(modified_study_data, "TOO_MANY") <-  TRUE
    return(modified_study_data) # TODO: SPLIT artificially
  }
  attr(modified_study_data, "TOO_MANY") <-  FALSE
  attributes(modified_study_data)[.ds1_attribute_names] <-
    attributes(study_data)[.ds1_attribute_names]
  modified_study_data
}
