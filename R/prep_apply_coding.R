#' Re-Code labels with their respective codes according to the `meta_data`
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#'
#' @return [data.frame] modified study data with labels replaced by the codes
#' @export
prep_apply_coding <- function(study_data, meta_data = "item_level") {
  util_expect_data_frame(study_data)
  util_expect_data_frame(meta_data, c(VAR_NAMES, VALUE_LABELS))
  prep_prepare_dataframes()
  resp_vars <-
    meta_data[!util_empty(meta_data[[VALUE_LABELS]]), VAR_NAMES, drop = TRUE]
  value_labels <- prep_map_labels(resp_vars,
                                  VALUE_LABELS,
                                  meta_data = meta_data)
  sdrv <-
    mapply(resp_vars, value_labels, SIMPLIFY = FALSE, FUN = function(rv, vl) {
      .vl <- util_parse_assignments(vl)
      VL <- setNames(names(.vl), nm = .vl)
      dt <- study_data[, rv, drop = TRUE]
      r <- VL[as.character(dt)]
      r[is.na(r) & ! is.na(dt)] <- dt[is.na(r) & ! is.na(dt)]
      if (
        all(!is.na(suppressWarnings(as.integer(names(.vl)))))
        &&
        (all(names(.vl) == suppressWarnings(as.integer(names(.vl)))))
      ) {
        r <- as.integer(r)
      }
      r
    })
  study_data[, resp_vars] <- sdrv
  study_data
}
