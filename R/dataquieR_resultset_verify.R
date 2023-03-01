#' Verify an object of class `r dataquieR_resultset_class`
#'
#' stops on errors
#'
#' @param list_to_verify object to be checked
#'
#' @return `invisible(TRUE)` -- stops on errors.
#'
dataquieR_resultset_verify <- function(list_to_verify) {
  if (!is.list(list_to_verify)) {
    util_error(
      "Tried to bless something else than a list in class %s. Internal error.",
      dQuote("dataquieR_resultset"), applicability_problem = FALSE)
  }

  if (!is.null(list_to_verify$meta_data_segment)) {
      prep_check_meta_data_segment(list_to_verify$meta_data_segment)
  }

  if (!is.null(list_to_verify$meta_data_dataframe)) {
    prep_check_meta_data_dataframe(list_to_verify$meta_data_dataframe)
  }

  if (!(length(list_to_verify$author) %in% 0:1)) {
    util_error(
      "%s must be of length 0 or 1. Internal error.",
      dQuote("author"),
      applicability_problem = FALSE)
  }

  if (length(list_to_verify$author) == 1 &&
      !is.character(list_to_verify$author)) {
    util_error(
      "%s must be a character. Internal error.",
      dQuote("author"),
      applicability_problem = FALSE)
  }

  for (slot in c("long_format", "app_mat")) {
    if (!is.list(list_to_verify[[slot]])) {
      util_error(
        "Not a list %s. Internal error.",
        dQuote(slot),
        applicability_problem = FALSE)
    }
  }

  is_dfrm <- vapply(list_to_verify$long_format, is.data.frame, FUN.VALUE =
                      logical(1))
  is_list0 <- vapply(list_to_verify$long_format, identical, list(), FUN.VALUE =
                      logical(1))
  if (!all(is_dfrm | is_list0)) {
    util_error(c("Some outputs are neither a data frame nor an empty list",
               "(empty lists means, an indicator is not applicable for any",
               "variable) in %s: %s. Internal error."),
               dQuote("long_format"),
               paste0(dQuote(names(is_dfrm[!is_dfrm & !is_list0])),
                      collapse = ", "),
               applicability_problem = FALSE)
  }

  if (!is.data.frame(list_to_verify$app_mat$SummaryTable)) {
    util_error("Not a data frame in %s. Internal error.",
               dQuote("this$app_mat$SummaryTable"),
               applicability_problem = FALSE)
  }

  for (slot in c("study_data",
                 "meta_data")) {
    if (!is.data.frame(list_to_verify[[slot]])) {
      util_error(
        "Not a data frame %s. Internal error.",
        dQuote(slot),
        applicability_problem = FALSE)
    }
  }
  if (list_to_verify$strata_attribute != STUDY_SEGMENT &&
      !is.na(list_to_verify$strata_attribute)) {
    util_error(
      "Not a supported %s: %s. Internal error.",
      sQuote("strata_attribute"),
      dQuote(list_to_verify$strata_attribute),
      applicability_problem = FALSE)
  }
  if (!is.null(list_to_verify$strata_vars) &&
      !is.character(list_to_verify$strata_vars)) {
    util_error(
      "All %s should be of type chracter. Internal error.",
      sQuote("strata_vars"),
      applicability_problem = FALSE)
  }
  if (!is.character(list_to_verify$label_col) ||
      length(list_to_verify$label_col) != 1 ||
      !(list_to_verify$label_col %in%
       colnames(list_to_verify$meta_data))) {
    util_error("Invalid %s set.",
               sQuote("label_col"),
               applicability_problem = FALSE)

  }
  if (!is.null(list_to_verify$strata_vars) &&
      is.character(list_to_verify$strata_vars)) {
    vars <- util_map_labels(list_to_verify$app_mat$SummaryTable$Variables,
                            list_to_verify$meta_data,
                            to = list_to_verify$label_col,
                            from = LABEL
    )
    vars_exist <- list_to_verify$strata_vars %in% vars

    if (!all(vars_exist)) {
      util_error(
        "All %s should name variables: %s don't. Internal error.",
        sQuote("strata_vars"),
        paste(dQuote(list_to_verify$strata_vars[!vars_exist]), collapse = ", "),
        applicability_problem = FALSE
      )
    }
  }
  invisible(TRUE)
}
