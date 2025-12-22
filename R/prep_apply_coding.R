#' Re-Code labels with their respective codes according to the `meta_data`
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return [data.frame] modified study data with labels replaced by the codes
#' @export
prep_apply_coding <- function(study_data, meta_data_v2,
                              item_level = "item_level",
                              meta_data = item_level) {
  util_maybe_load_meta_data_v2()
  util_expect_data_frame(study_data, keep_types = TRUE)
  prep_prepare_dataframes(.adjust_data_type = FALSE)
  util_expect_data_frame(meta_data, c(VAR_NAMES, VALUE_LABEL_TABLE))
  resp_vars <-
    meta_data[!util_empty(meta_data[[VALUE_LABEL_TABLE]]), VAR_NAMES, drop = TRUE]
  value_label_tables <- prep_map_labels(resp_vars,
                                  VALUE_LABEL_TABLE,
                                  item_level = meta_data)
  sdrv <-
    mapply(resp_vars, value_label_tables, SIMPLIFY = FALSE, FUN =
             function(rv, vlt) {
      vltdf <- prep_get_data_frame(vlt)
      VL <- setNames(vltdf[[CODE_VALUE]], nm = vltdf[[CODE_LABEL]])
      dt <- study_data[, rv, drop = TRUE]
      r <- VL[as.character(dt)]
      r[is.na(r) & ! is.na(dt)] <- dt[is.na(r) & ! is.na(dt)]
      if (
        all(!is.na(suppressWarnings(as.integer(vltdf[[CODE_VALUE]]))))
        &&
        (all(vltdf[[CODE_VALUE]] ==
             suppressWarnings(as.integer(vltdf[[CODE_VALUE]]))))
      ) {
        r <- as.integer(r)
      }
      r
    })
  study_data[, resp_vars] <- sdrv
  study_data
}
