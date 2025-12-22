#' Create an environment with several alias names for the study data variables
#'
#' generates an environment similar to `as.environment(ds1)`, but makes
#' variables available by their `VAR_NAME`, `LABEL`, and `label_col` - names.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables. If
#'                                       `study_data` has already been mapped,
#'                                       i.e., `util_ds1_eval_env(ds1, ...)` is
#'                                       called, this will work too
#'
#' @family rule_functions
#' @concept process
#' @noRd
util_ds1_eval_env <- function(study_data,
                              meta_data = "item_level",
                              label_col = LABEL) {
  if (isTRUE(attr(study_data, "MAPPED"))) {
    ds1 <- study_data
  } else {
    prep_prepare_dataframes()
  }
  label_col_from <- attr(ds1, "label_col")
  label_col_to <- label_col
  res <- ds1
  lct <- setdiff(c(VAR_NAMES, LABEL, LONG_LABEL, label_col_to), label_col_from)
  lct <- intersect(lct, colnames(meta_data))
  for (cur_nm in lct) {
    res[, util_map_labels(colnames(ds1),
                          from = label_col_from,
                          to = cur_nm,
                          meta_data = meta_data)] <-
      ds1[, colnames(ds1), FALSE]
  }
  as.environment(res)
}
