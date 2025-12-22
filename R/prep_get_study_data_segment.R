#' Get data frame for a given segment
#'
#' @inheritParams .template_function_indicator
#' @param segment [character] name of the segment to return data for
#'
#' @return [data.frame] the data for the segment
#' @export
prep_get_study_data_segment <- function(
    segment,
    study_data,
    item_level = "item_level",
    meta_data = item_level,
    meta_data_v2,
    segment_level,
    meta_data_segment = "segment_level") {
  # Preps and checks ----
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  util_expect_scalar(segment, check_type = is.character,
                     error_message =
                       sprintf("Need a segment name in %s", sQuote("segment")))
  meta_data_segment <- try(prep_check_meta_data_segment(meta_data_segment),
                           silent = TRUE)
  if (util_is_try_error(meta_data_segment)) {
    util_warning("%s metadata missing/corrupted: %s",
                 sQuote("segment_level"),
                 conditionMessage(attr(meta_data_segment, "condition",
                                       exact = TRUE)))
    meta_data_segment <- data.frame()
  }
  # map metadata to study data
  prep_prepare_dataframes(.allow_empty =  TRUE,
                          .adjust_data_type = FALSE,
                          .replace_hard_limits = FALSE,
                          .amend_scale_level = FALSE,
                          .replace_missings = FALSE,
                          .label_col = VAR_NAMES)
  if (!(STUDY_SEGMENT %in% colnames(meta_data))) {
    meta_data[[STUDY_SEGMENT]] <- "ALL" # TODO: Warn
  }
  util_ensure_in(applicability_problem = TRUE, error = TRUE,
    segment, meta_data[[STUDY_SEGMENT]],
    sprintf("Did not find segment %s in item_level metadata %s",
            dQuote(segment), sQuote(STUDY_SEGMENT)
    )
  )

  id_vars <- meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] == segment,
                               SEGMENT_ID_VARS,
                               drop = TRUE]
  if (length(id_vars) != 1) {
    util_warning("No ID-vars in segment level metadata for segment %s found",
                 sQuote(segment), applicability_problem = TRUE)
    id_vars <- character(0)
  } else {
    id_vars <-
      util_parse_assignments(id_vars)[[1]]
    id_vars <- util_find_var_by_meta(id_vars,
                                     meta_data = meta_data,
                                     label_col = label_col,
                                     target = VAR_NAMES,
                                     ifnotfound = id_vars)
    id_vars <- intersect(colnames(ds1), id_vars)
  }

  util_remove_empty_rows(
    id_vars = id_vars,
    ds1[, c(id_vars, util_get_vars_in_segment(segment,
                             meta_data = meta_data,
                             label_col = label_col)), FALSE]
  )
}
