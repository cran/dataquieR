#' Extract old function based summary from data quality results
#'
#' @param r [dq_report2]
#'
#' @return [data.frame] long format, compatible with [prep_summary_to_classes()]
#' @export
#'
#' @family summary_functions
prep_extract_classes_by_functions <- function(r) {

  res <-
    prep_summary_to_classes(
      prep_extract_summary(r))

  res$function_name <-
    vapply(FUN.VALUE = character(1),
           setNames(nm = res$call_names),
           util_cll_nm2fkt_nm,
           report = r)

  if (!"class" %in% colnames(res)) {
    res$class <- rep(util_as_cat(NA), nrow(res))
  }

  if (!"indicator_metric" %in% colnames(res)) {
    res$indicator_metric <- rep(NA_character_, nrow(res))
  }

  if (!"value" %in% colnames(res)) {
    res$value <- rep(NA_character_, nrow(res))
  }

  if (!"values_raw" %in% colnames(res)) {
    res$values_raw <- rep(NA_character_, nrow(res))
  }

  if (!"n_classes" %in% colnames(res)) {
    res$n_classes <- rep(NA_character_, nrow(res))
  }

  if (!"call_names" %in% colnames(res)) {
    res$call_names <- rep(NA_character_, nrow(res))
  }

  res[, c("VAR_NAMES", "class", "indicator_metric", "value", "values_raw",
          "n_classes",
          "STUDY_SEGMENT", "call_names", "function_name"), drop = FALSE]

}
