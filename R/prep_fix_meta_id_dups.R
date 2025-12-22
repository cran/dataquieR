#' Fix metadata duplicates
#'
#' if [VAR_NAMES] have duplicates, maybe, it's because of ID-vars assigned
#' to different study segments multiple times (they should be in one "intro"-
#' segment, only), which is not the intended use of `STUDY_SEGMENT`.
#' Naturally, they will be part of more than one data-frame, so
#' this would also qualify for a dump duplicate, only, which can safely be
#' removed. Only ID-vars are by default assumed to have such duplicates in item
#' level metadata allowed.
#'
#' @inheritParams .template_function_indicator
#'
#' @returns [meta_data]
#' @export
#' @examples
#' \dontrun{
#' il <- prep_get_data_frame("item_level")
#' il <- rbind(il, il)
#' il$STUDY_SEGMENT[2] <- "X"
#' il2 <- prep_fix_meta_id_dups(meta_data_v2 = "meta_data_v2", item_level = il)
#' il2$STUDY_SEGMENT
#' il$STUDY_SEGMENT[3] <- "X"
#' il3 <- prep_fix_meta_id_dups(meta_data_v2 = "meta_data_v2", item_level = il)
#' il3$STUDY_SEGMENT
#' }
prep_fix_meta_id_dups <- function(meta_data_segment = "segment_level",
                                  meta_data_dataframe =
                                    "dataframe_level",
                                  item_level = "item_level",
                                  meta_data = item_level,
                                  meta_data_v2,
                                  segment_level,
                                  dataframe_level) {
  # Preps and checks ----
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  util_expect_data_frame(meta_data)
  meta_data <- meta_data[!duplicated(meta_data), , FALSE]
  prep_check_meta_names(meta_data = meta_data, level = REQUIRED)
  duplicated_vn <- duplicated(meta_data[[VAR_NAMES]])
  if (!any(duplicated_vn)) return(meta_data);

  meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
  meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)
  if (!DF_ID_VARS %in% names(meta_data_dataframe))
    meta_data_dataframe[[DF_ID_VARS]] <- NA_character_
  if (!SEGMENT_ID_VARS %in% names(meta_data_segment))
    meta_data_segment[[SEGMENT_ID_VARS]] <- NA_character_
  all_id_vars <-
    trimws(unique(unlist(util_parse_assignments(multi_variate_text = TRUE,
                                                      c(
    as.vector(unlist(meta_data_segment[, SEGMENT_ID_VARS, drop = TRUE])),
    as.vector(unlist(meta_data_dataframe[, DF_ID_VARS, drop = TRUE]))
  )))))
  all_id_vars <- all_id_vars[!is.na(all_id_vars)]

  all_id_vars <- util_find_var_by_meta(all_id_vars,
                                       meta_data = meta_data)

  duplicated_il_wo <- duplicated(meta_data[, setdiff(colnames(meta_data),
                                                     c(STUDY_SEGMENT,
                                                       DATAFRAMES))])
  if (any(duplicated_vn & !duplicated_il_wo)) {
    util_error( # FIXME: With dq_report_by and fixing colnames(of_affected_study_data), too.
      "Fixing of different variables using the same name not yet supported",
      applicability_problem = TRUE)
  } else {
    dup_names <- as.vector(meta_data[duplicated_vn, VAR_NAMES, TRUE])
    if (!all(dup_names %in% all_id_vars)) {
      util_warning(
        c("Some none-id-vars are duplicated but differ only in %s or %s",
          "will fix that, but you may have very inconsistent metadata"),
        sQuote(STUDY_SEGMENT),
        sQuote(DATAFRAMES), applicability_problem = TRUE)
    } else {
      util_message("Overwriting inconsistent parts of metadata...", # TODO: Better message
                   applicability_problem = TRUE)
    }
    meta_data[meta_data[[VAR_NAMES]] %in% dup_names,
              intersect(colnames(meta_data),
                                       c(DATAFRAMES,
                                         STUDY_SEGMENT))] <-
      "**INTRO**" ## TODO: Better name
    r <- meta_data[!duplicated_vn, , FALSE]
    return(r)
  }
}
