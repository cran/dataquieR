#' Convert a study variable to a [factor]
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param assume_consistent_codes [logical] assume, that missing codes are
#'                                          consistent for all variables
#' @param have_cause_label_df [logical] is a missing-code table available
#' @param code_name [character] all lists from the [meta_data] to use for the
#'                              coding.
#' @param include_sysmiss [logical] add also a factor level for data values
#'                                  that were `NA` in the original study data
#'                                  (system missingness).
#'
#' @return `study_data` converted to factors using the coding provided in
#'         `code_name`
#'
#' @family data_management
#' @concept metadata_management
#' @keywords internal
util_study_var2factor <- function(resp_vars = NULL, study_data,
                                  meta_data = "item_level", # TODO: make a prep, see util_assign_levlabs, make it work also for VALUE_LABELS, not only for missing codes, optionally. Default should be FALSE or all existing calls need to be edited to turn off this feature. Maybe, the prep is just a copy of the util with modified defaults, as in  progress <- other_function; formals(progress)$is_rstudio <- force(is_rstudio)
                                  label_col = LABEL,        # TODO: This is now available in prep_prepare_dataframes(.apply_factor_metadata = ) or prep_prepare_dataframes(.apply_factor_metadata_inadm = )
                                  assume_consistent_codes = TRUE,
                                  have_cause_label_df = FALSE,
                                  code_name = c(JUMP_LIST, MISSING_LIST),
                                  include_sysmiss = TRUE) {
  util_expect_scalar(assume_consistent_codes, check_type = is.logical)
  util_expect_scalar(include_sysmiss, check_type = is.logical)
  util_expect_scalar(code_name, allow_more_than_one = TRUE, allow_null = TRUE,
                     check_type = is.character)
  util_expect_data_frame(meta_data, code_name)
  .meta_data <- meta_data # FIXME: Save metadata here, but we should not rely on VALUE_LABELS, any more
  prep_prepare_dataframes(.replace_missings = FALSE)
  meta_data <- .meta_data
  util_correct_variable_use2(resp_vars,
                             allow_more_than_one = TRUE,
                             allow_na = TRUE,
                             allow_null = TRUE,
                             allow_all_obs_na = TRUE,
                             allow_any_obs_na = TRUE)
  if (!length(resp_vars)) {
    resp_vars <- colnames(ds1)
  }
  r <- lapply(setNames(nm = resp_vars), function(rv) {
    l <- util_get_combined_code_lists(rv,
                                      mdf = meta_data,
                                      code_name = code_name,
                                      label_col = label_col,
                                      warning_if_no_list = FALSE,
                                      assume_consistent_codes = assume_consistent_codes,
                                      have_cause_label_df = have_cause_label_df)
    orig_r <- ds1[[rv]]
    r <- ds1[[rv]]
    if (lubridate::is.timepoint(r)) {
      r <- as.character(r)
      r <- gsub(" 00:00:00$", "", r) # only whole days are allowed missing codes
    }
    r <- suppressWarnings(
      factor(r,
             labels = names(l),
             levels = l))
    if (include_sysmiss) {
      levels(r) <- c(levels(r), .SM_LAB)
      r[is.na(orig_r)] <- .SM_LAB
    }
    r
  })
  ds2 <- ds1[, resp_vars, drop = FALSE]
  ds2[] <- r

  return(ds2)

}
