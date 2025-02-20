#' Utility function to prepare the metadata for proportion checks
#'
#' @param resp_vars [variable list] the names of the measurement variables
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param ds1 [data.frame] the data frame that contains the measurements
#'                               (hint: missing value codes should be excluded,
#'                               so the function should be called with `ds1`, if
#'                               available)
#' @param report_problems [enum] Should missing metadata information be reported
#'                               as error, warning or message?
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with the expected range for the proportion check
#'
#' @family lookup_functions
#' @concept metadata_management
#' @keywords internal

util_prep_proportion_check <- function(resp_vars,
                                       meta_data,
                                       ds1,
                                       report_problems = c("error", "warning",
                                                           "message"),
                                       label_col = attr(ds1, "label_col")) {
  resp_vars <- prep_get_labels(resp_vars = resp_vars,
                               item_level = meta_data,
                               label_col = label_col,
                               force_label_col = "TRUE")
  if (!isTRUE(attr(ds1, "apply_fact_md_inadm"))) {
    util_error("Internal error, sorry, please report: proprotion check called with bare study data or without inadmissible values.") # TODO: I think we should also allow to have inadmissible values already removed because this utility function is called in the accuracy section.
  }
  report_problems <- match.arg(report_problems)
  prop_range <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                               target = PROPORTION_RANGE,
                                               meta_data = meta_data,
                                               allowed_sources = label_col),
                         nm = resp_vars)
  if (VALUE_LABELS %in% colnames(meta_data)) {
    util_error("Internal error, sorry, please report: VALUE_LABELS in util_prep_proportion_check.")
  }
  # meta_data <- util_normalize_value_labels(meta_data) # TODO: STANDARDIZED_VOCABULARY_TABLE
  if (VALUE_LABEL_TABLE %in% colnames(meta_data)) {
    val_label_tables <- setNames(util_find_var_by_meta(resp_vars = resp_vars,
                                                 target = VALUE_LABEL_TABLE,
                                                 meta_data = meta_data,
                                                 allowed_sources = label_col),
                           nm = resp_vars)
  } else {
    val_label_tables <- setNames(rep(NA_character_, length(resp_vars)),
                               nm = resp_vars)
  }


  prop_range_int <-
    lapply(setNames(nm = resp_vars), function(rv) {
      rv_is_factor <- is.factor(ds1[[rv]])
      if (rv_is_factor) {
        vv_codes <- levels(ds1[[rv]])
      } else {
        vv_codes <- unique(ds1[[rv]])
        vv_codes <- sort(vv_codes[!is.na(vv_codes)])
      }
      VL <- NULL
      if (rv_is_factor) {
        vlt <- val_label_tables[[rv]]
        if (!is.null(vlt) && !util_empty(vlt)) {
          vlt_rv <- prep_get_data_frame(vlt)
          VL <- setNames(vlt_rv[[CODE_VALUE]], nm = vlt_rv[[CODE_LABEL]])
        }
      }
      if (grepl(SPLIT_CHAR, prop_range[[rv]], fixed = TRUE) |
          grepl(" in ", prop_range[[rv]])) {
        # ranges given per category
        input <- gsub(" in ", " = ", prop_range[[rv]])
        int_list <- util_parse_assignments(input)
        if (rv_is_factor & !is.null(VL) & all(names(int_list) %in% VL)) {
          # If the levels are given as integers, we need to transform them to
          # value labels.
          names(int_list) <- unlist(lapply(names(int_list), function(cc) {
            names(VL)[VL == as.character(cc)]
          }))
        }
        range_per_cat <- lapply(setNames(nm = vv_codes), function(cc) {
          if (cc %in% names(int_list)) {
            util_parse_interval(int_list[[as.character(cc)]])
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
