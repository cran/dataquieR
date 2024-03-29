#' Utility function to prepare the metadata for proportion checks
#'
#' @param resp_vars [variable list] the names of the measurement variables
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param study_data [data.frame] the data frame that contains the measurements
#'                               (hint: missing value codes should be excluded,
#'                               so the function should be called with `ds1`, if
#'                               available)
#' @param report_problems [enum] Should missing metadata information be reported
#'                               as error, warning or message?
#'
#' @return a [list] with the expected range for the proportion check
#'
#' @family lookup_functions
#' @concept metadata_management
#' @keywords internal

util_prep_proportion_check <- function(resp_vars,
                                       meta_data,
                                       study_data,
                                       report_problems = c("error", "warning",
                                                           "message")) {
  report_problems <- match.arg(report_problems)
  prop_range <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                               target = "PROPORTION_RANGE",
                                               meta_data = meta_data),
                         nm = resp_vars)
  val_labels <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                               target = "VALUE_LABELS",
                                               meta_data = meta_data),
                         nm = resp_vars)

  prop_range_int <-
    lapply(setNames(nm = resp_vars), function(rv) {
      # if (is.na(prop_range[[rv]])) {
      #   return(NA)
      # }
      if (!is.na(val_labels[[rv]])) { # value labels available
        vv_codes <- names(util_parse_assignments(val_labels[[rv]],
                                                 split_on_any_split_char = TRUE,
                                               split_char = c(SPLIT_CHAR, '<')))
      } else { # no value labels available, get unique entries in the study data
        vv_codes <- unique(study_data[[rv]])
        vv_codes <- vv_codes[which(!is.na(vv_codes))]
        vv_codes <- as.character(sort(vv_codes))
      }
      if (grepl(SPLIT_CHAR, prop_range[[rv]], fixed = TRUE) |
          grepl(" in ", prop_range[[rv]])) {
        # ranges given per category
        input <- gsub(" in ", " = ", prop_range[[rv]])
        int_list <- util_parse_assignments(input)
        range_per_cat <- lapply(setNames(nm = vv_codes), function(cc) {
          if (cc %in% names(int_list)) {
            util_parse_interval(int_list[[cc]])
          } else {
            NA
          }
        })
      } else { # one range given
        int <- util_parse_interval(prop_range[[rv]])
        range_per_cat <- lapply(setNames(nm = vv_codes), function(cc) { int })
      }
      if (all(is.na(range_per_cat))) {
        util_warning(paste0(
          "For ", rv, ", the given PROPORTION_RANGE could not be ",
          "interpreted as an interval."),
          applicability_problem = TRUE)
       # range_per_cat <- NA
      }
      range_per_cat
    })

  rvs_with_prop <- names(prop_range_int)[
    vapply(prop_range_int,
           FUN = function(x) {
             if (is.list(x)) {
               !all(is.na(x))
             } else {
               !is.na(x)
             }},
           FUN.VALUE = logical(1))]

  rep_fun <- switch(report_problems,
         error = util_error,
         warning = util_warning,
         message = util_message)

  if (length(resp_vars) > length(rvs_with_prop)) {
    rep_fun(paste0(
      "For ", paste(setdiff(resp_vars, rvs_with_prop), collapse = ", "),
      ", the metadata for a proportion check is missing (PROPORTION_RANGE)."),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE)
  }

  return(list("Range" = prop_range_int))
}
