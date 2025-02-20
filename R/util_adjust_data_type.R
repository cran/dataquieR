#' Adjust the data types of study data, if needed
#'
#' @param study_data [data.frame] the study data
#' @param meta_data [meta_data] `VAR_NAMES` relevant for warnings about
#'                              conversion errors
#' @param relevant_vars_for_warnings [character]
#'
#' @return [data.frame] modified study data
#' @keywords internal
util_adjust_data_type <- function(study_data, # TODO: Use maybe readr::type_convert
                                  meta_data,
                                  relevant_vars_for_warnings) {
  # prevent repeated data type checks if adjusted previously
  if (!isTRUE(attr(study_data, "Data_type_matches"))) {
    check_type <- util_compare_meta_with_study(
      sdf = study_data,
      mdf = meta_data,
      label_col = VAR_NAMES,
      check_convertible = TRUE, # slow, but now cached
      check_conversion_stable = FALSE,
      threshold_value = 0)

    if (any(check_type != 1)) {
      vars_to_transform <- names(check_type)[check_type != 1]
      expected_type <- setNames(util_map_labels(
        x = vars_to_transform,
        to = DATA_TYPE,
        from = VAR_NAMES,
        ifnotfound =
          DATA_TYPES$STRING,
        meta_data = meta_data,
        warn_ambiguous = FALSE),
        nm = vars_to_transform)

      transf_sd <- lapply(
        setNames(nm = vars_to_transform),
        function(rv) {
          rv2 <- util_data_type_conversion(study_data[[rv]],
                                           expected_type[[rv]])
          if (!identical(which(is.na(study_data[[rv]])),
                         which(is.na(rv2)))) {
            if (length(relevant_vars_for_warnings) == 0 || # did not find relevant vars vector, show warning in any case
                rv %in% relevant_vars_for_warnings) {
              util_message(
                paste("Data type transformation of", dQuote(rv), "introduced",
                      length(which(is.na(rv2))) -
                        length(which(is.na(study_data[[rv]]))),
                      "additional missing values."),
                applicability_problem = TRUE)
            }
          }
          return(rv2)
        })
      study_data[, vars_to_transform] <- transf_sd
    }
    attr(study_data, "Data_type_matches") <- TRUE
  }
  study_data
}
