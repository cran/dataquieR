#' Distribute `CODE_LIST_TABLE` in item level metadata
#'
#' fills the columns `MISSING_LIST_TABLE` and `VALUE_LABEL_TABLE` from
#' `CODE_LIST_TABLE`, if applicable
#'
#' @inheritParams .template_function_indicator
#'
#' @return [meta_data], but `CODE_LIST_TABLE` column is distributed to the
#'         columns `VALUE_LABEL_TABLE` and `MISSING_LIST_TABLE`, respectively.
#' @noRd
util_normalize_clt <- function(meta_data) {
  if (!(CODE_LIST_TABLE %in% names(meta_data))) {
    return(meta_data)
  }
  util_handle_val_tab()
  is_mlt <- vapply(
    FUN.VALUE = logical(1),
    setNames(nm = na.omit(unique(meta_data[[CODE_LIST_TABLE]]))),
    function(tb) {
      "MISSING" %in% prep_get_data_frame(tb)[[CODE_CLASS]] ||
        "JUMP" %in% prep_get_data_frame(tb)[[CODE_CLASS]]
    })
  is_vlt <- vapply(
    FUN.VALUE = logical(1),
    setNames(nm = na.omit(unique(meta_data[[CODE_LIST_TABLE]]))),
    function(tb) {
      tb <- prep_get_data_frame(tb)
      (!(CODE_CLASS %in% names(tb))) || "VALUE" %in% tb[[CODE_CLASS]]
    })
  if (any(!util_empty(meta_data[[MISSING_LIST_TABLE]][is_mlt[
    meta_data[[CODE_LIST_TABLE]]]]))) {
    util_message(c("Found %s where also %s has been assigned, discarding %s",
                   "for such items"),
                 sQuote(MISSING_LIST_TABLE),
                 sQuote(CODE_LIST_TABLE),
                 sQuote(MISSING_LIST_TABLE),
                 applicability_problem = TRUE)
  }
  if (any(!util_empty(meta_data[[VALUE_LABEL_TABLE]][is_vlt[
    meta_data[[CODE_LIST_TABLE]]]]))) {
    util_message(c("Found %s where also %s has been assigned, discarding %s",
                   "for such items"),
                 sQuote(VALUE_LABEL_TABLE),
                 sQuote(CODE_LIST_TABLE),
                 sQuote(VALUE_LABEL_TABLE),
                 applicability_problem = TRUE)
  }
  meta_data[[MISSING_LIST_TABLE]][is_mlt[meta_data[[CODE_LIST_TABLE]]]] <-
    meta_data[[CODE_LIST_TABLE]][is_mlt[meta_data[[CODE_LIST_TABLE]]]]
  meta_data[[VALUE_LABEL_TABLE]][is_vlt[meta_data[[CODE_LIST_TABLE]]]] <-
    meta_data[[CODE_LIST_TABLE]][is_vlt[meta_data[[CODE_LIST_TABLE]]]]
  meta_data[[CODE_LIST_TABLE]] <- NULL
  meta_data
}
