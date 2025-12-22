#' Align column name case to meta_data VAR_NAMES
#'
#' Case-insensitively matches study-data column names to VAR_NAMES from
#' the meta_data and adjusts the capitalization of the column names so that
#' they match the corresponding VAR_NAMES.
#'
#' If there are case-insensitive duplicates in VAR_NAMES that differ only in
#' capitalization (e.g., "AGE" and "age"), the function stops with
#' util_error(..., applicability_error = TRUE).
#'
#' @param .colnames character vector of column names to be aligned.
#' @param .var_names character vector of VAR_NAMES from meta_data.
#'
#' @return character vector of the same length as .colnames, with
#'   capitalization aligned to .var_names where possible.
#' @noRd
util_align_colnames_case <- function(.colnames, .var_names) {
  .colnames  <- as.character(.colnames)
  .var_names <- as.character(.var_names)

  # check for case-insensitive duplicates in .var_names
  lv <- tolower(.var_names)

  if (any(duplicated(lv))) {
    # find all lower-case keys that appear multiple times
    dup_keys <- unique(lv[duplicated(lv)])

    # keep only those where the originals differ in case / spelling
    bad_keys <- dup_keys[
      vapply(
        dup_keys,
        function(k) {
          length(unique(.var_names[lv == k])) > 1L
        },
        logical(1L)
      )
    ]

    if (length(bad_keys) > 0L) {
      util_error(
        c("for case-insensitive study-data-column-names, I need",
        "case-insenstively distinct %s in the meta_data, but I have %s.",
        "either fix your meta_data or set the option %s to its default %s"),
        sQuote(VAR_NAMES),
        util_pretty_vector_string(bad_keys),
        sQuote("dataquieR.study_data_colnames_case_sensitive"),
        sQuote(dataquieR.study_data_colnames_case_sensitive_default),
        applicability_error = TRUE
      )
    }
  }

  # map .colnames (case-insensitive) to .var_names
  idx <- match(tolower(.colnames), lv)

  res <- .colnames
  hit <- !is.na(idx)
  res[hit] <- .var_names[idx[hit]]

  res
}
