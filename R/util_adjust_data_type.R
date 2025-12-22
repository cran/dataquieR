#' Adjust the data types of study data, if needed
#'
#' @param study_data [data.frame] the study data
#' @param meta_data [meta_data] `VAR_NAMES` relevant for warnings about
#'                              conversion errors
#' @param relevant_vars_for_warnings [character]
#'
#' @return [data.frame] modified study data
#' @noRd
util_adjust_data_type <- function(study_data,
                                  meta_data, # TODO: Use maybe readr::type_convert
                                  relevant_vars_for_warnings = character(0)) {
  # FIXME: The old adjust_type function does not interpret 19 as 0019 but as 2019. same hint in the test for test-acc_loess.R -- 1969 is the first year with 19, there.
  if (!isTRUE(as.logical(getOption("dataquieR.old_type_adjust",
                dataquieR.old_type_adjust_default)))) {
    cl <- sys.call()
    cl[[1]] <-
      rlang::call2(":::", rlang::sym("dataquieR"),
                   rlang::sym("util_adjust_data_type2"))
    return(eval(cl,
                envir = parent.frame(),
                enclos = parent.frame()))
  }
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
    logcs <- names(which(vapply(study_data, is.logical, FUN.VALUE = logical(1)))) # these should be NA only.
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
      util_stop_if_not(
        "all logical columns should be NA only." =
          all(is.na(unlist(as.vector(study_data[, logcs])))))
    }
    if (length(logcs) > 0) {
      expected_type <- setNames(util_map_labels( # convert them to the desired NA type -- can happen, if data type was compatible, so no conversion was done
        x = logcs,
        to = DATA_TYPE,
        from = VAR_NAMES,
        ifnotfound =
          DATA_TYPES$FLOAT,
        meta_data = meta_data,
        warn_ambiguous = FALSE),
        nm = logcs)
      lgcs_which_are_flts <- names(which(expected_type == DATA_TYPES$FLOAT))
      lgcs_which_are_intgs <- names(which(expected_type == DATA_TYPES$INTEGER))
      study_data[, lgcs_which_are_intgs] <-
        lapply(study_data[, lgcs_which_are_intgs, drop = FALSE],
               as.integer)
      study_data[, lgcs_which_are_flts] <-
        lapply(study_data[, lgcs_which_are_flts, drop = FALSE],
               as.double)
    }
    attr(study_data, "Data_type_matches") <- TRUE
  }
  study_data
}
