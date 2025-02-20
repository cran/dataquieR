#' Utility function to prepare the metadata for location checks
#'
#' @param resp_vars [variable list] the names of the measurement variables
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param report_problems [enum] Should missing metadata information be reported
#'                               as error, warning or message?
#'
#' @return a [list] with the location metric (mean or median) and expected
#'         range for the location check
#'
#' @family lookup_functions
#' @concept metadata_management
#' @keywords internal

util_prep_location_check <- function(resp_vars,
                                     meta_data,
                                     report_problems = c("error", "warning",
                                                         "message"),
                                     label_col = VAR_NAMES) {
  resp_vars <- prep_get_labels(resp_vars = resp_vars,
                               item_level = meta_data,
                               label_col = label_col,
                               force_label_col = "TRUE")
  report_problems <- match.arg(report_problems)
  rep_fun <- switch(report_problems,
                    error = util_error,
                    warning = util_warning,
                    message = util_message)
  loc_metric <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                               target = "LOCATION_METRIC",
                                               meta_data = meta_data,
                                               allowed_sources = label_col),
                         nm = resp_vars)
  loc_metric <- trimws(tolower(loc_metric))

  if (!all(loc_metric[!util_empty(loc_metric)] %in% c("median", "mean"))) {
    rep_fun("Location checks can only be performed for mean or median values.",
            applicability_problem = TRUE,
            intrinsic_applicability_problem = TRUE)
    loc_metric[which(!(loc_metric %in% c("median", "mean")))] <- NA
  }

  loc_range <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                              target = "LOCATION_RANGE",
                                              meta_data = meta_data,
                                              allowed_sources = label_col),
                        nm = resp_vars)
  loc_range <- lapply(loc_range, util_parse_interval)

  rvs_with_metric <- names(loc_metric)[which(!is.na(loc_metric))]
  rvs_with_range <- names(loc_range)[which(!is.na(loc_range))]
  rvs_with_both <- intersect(rvs_with_metric, rvs_with_range)
  rvs_with_none <- setdiff(resp_vars, unique(c(rvs_with_metric,
                                               rvs_with_range)))

  if (!all.equal(rvs_with_metric, rvs_with_range)) {
    rep_fun(paste0(
      "For ",
      paste(setdiff(union(rvs_with_metric, rvs_with_range),
                    rvs_with_both),
            collapse = ", "),
      ", the metadata for the expected location is incomplete. Both ",
      "LOCATION_RANGE and LOCATION_METRIC are required."),
      applicability_problem = TRUE)
    loc_metric[setdiff(resp_vars, rvs_with_both)] <- NA
    loc_range[setdiff(resp_vars, rvs_with_both)] <- NA
  }

  if (length(rvs_with_none) > 0) {
    rep_fun(paste0(
      "For ", paste(rvs_with_none, collapse = ", "),
      ", the metadata for a location check is missing (LOCATION_RANGE and ",
      "LOCATION_METRIC)."),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE)
  }

  loc_meta <- list("Metric" = loc_metric,
                   "Range" = loc_range)
  return(loc_meta)
}
