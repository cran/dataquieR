#' Delete rows from summary table for `SSI` or non-`SSI` variables
#'
#' @inheritParams .template_function_indicator
#' @param repsumtab [data.frame] the report summary table
#' @param vars_to_include `"study"`, `"ssi"` or `c("study", "ssi")`. variables to include
#'
#' @returns [data.frame] the filtered `repsumtab` with attribute
#'   `rownames_of_report`, also filtered
#' @keywords internal
util_filter_repsum <- function(repsumtab, vars_to_include, meta_data,
                               rownames_of_report, label_col) {
  util_expect_scalar(vars_to_include,
                     allow_more_than_one = TRUE,
                     min_length = 1,
                     max_length = 2,
                     check_type = is.character)
  util_match_arg(vars_to_include, c("study", "ssi"), several_ok = TRUE)

  ssi_vars <-
    meta_data[[VAR_NAMES]][!util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])]
  ssi_vars_lb <-
    meta_data[[label_col]][!util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])]

  if (!"ssi" %in% vars_to_include) {
    repsumtab <-
      repsumtab[!repsumtab$VAR_NAMES %in% ssi_vars, , FALSE]
    rownames_of_report <- rownames_of_report[!(rownames_of_report %in%
                                                 ssi_vars_lb)]
  }
  if (!"study" %in% vars_to_include) {
    repsumtab <-
      repsumtab[repsumtab$VAR_NAMES %in% ssi_vars, , FALSE]
    rownames_of_report <- rownames_of_report[(rownames_of_report %in%
                                                ssi_vars_lb)]
  }

  util_attach_attr(repsumtab, rownames_of_report = rownames_of_report)
}
