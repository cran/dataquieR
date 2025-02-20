#' Verify item-level metadata
#'
#' @description
#'
#' are the provided item-level `meta_data` plausible given `study_data`?
#'
#' @inheritParams .template_function_indicator
#'
#' @return an [invisible()] list with the entries.
#'   - `pred` [data.frame] metadata predicted from `study_data`,
#'     reduced to such metadata also available in the provided metadata
#'   - `prov` [data.frame] provided metadata,
#'     reduced to such metadata also available in the provided `study_data`
#'   - `ml_error` [character] `VAR_NAMES` of variables with potentially wrong
#'     `MISSING_LIST`
#'   - `sl_error` [character] `VAR_NAMES` of variables with potentially wrong
#'     `SCALE_LEVEL`
#'   - `dt_error` [character] `VAR_NAMES` of variables with potentially wrong
#'     `DATA_TYPE`
#' @export
prep_compare_meta_with_study <- function(study_data,
         label_col,
         item_level = "item_level",
         meta_data = item_level,
         meta_data_v2) {

  util_maybe_load_meta_data_v2()

  prep_prepare_dataframes(
    .replace_hard_limits = FALSE,
    .replace_missings = FALSE,
    .adjust_data_type = FALSE,
    .amend_scale_level = FALSE,
    .apply_factor_metadata = FALSE,
    )

  prov <- meta_data

  suppressMessages(suppressWarnings(
    pred <- prep_study2meta(study_data = study_data,
                            guess_missing_codes = TRUE)
  ))

  rownames(pred) <- pred$VAR_NAMES
  rownames(prov) <- prov$VAR_NAMES

  both <- intersect(rownames(pred), rownames(prov))

  pred <- pred[both, , FALSE]
  prov <- prov[both, , FALSE]

  dt_error <- both[pred$DATA_TYPE != prov$DATA_TYPE]
  sl_error <- both[pred$SCALE_LEVEL != prov$SCALE_LEVEL]
  ml_error <- both[pred$MISSING_LIST != prov$MISSING_LIST]

  if (length(dt_error) > 0) {
    util_warning(
      "In the item-level %s %s may have assigned the wrong %s given %s",
      sQuote("meta_data"),
      util_pretty_vector_string(dt_error),
      sQuote(DATA_TYPE),
      sQuote("study_data")
    )
  }
  if (length(sl_error) > 0) {
    util_warning(
      "In the item-level %s %s may have assigned the wrong %s given %s",
      sQuote("meta_data"),
      util_pretty_vector_string(sl_error),
      sQuote(SCALE_LEVEL),
      sQuote("study_data")
    )
  }
  if (length(ml_error) > 0) {
    util_warning(
      "In the item-level %s %s may have assigned the wrong %s given %s",
      sQuote("meta_data"),
      util_pretty_vector_string(ml_error),
      sQuote(MISSING_LIST),
      sQuote("study_data")
    )
  }
  return(invisible(list(
    pred = pred,
    prov = prov,
    ml_error = ml_error,
    sl_error = sl_error,
    dt_error = dt_error
  )))
}
