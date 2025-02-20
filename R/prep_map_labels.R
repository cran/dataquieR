#' Support function to allocate labels to variables
#'
#' @description
#' Map variables to certain attributes, e.g. by default their labels.
#'
#' @details
#' This function basically calls `colnames(study_data) <- meta_data$LABEL`,
#' ensuring correct merging/joining of study data columns to the corresponding
#' metadata rows, even if the orders differ. If a variable/study_data-column
#' name is not found in `meta_data[[from]]` (default `from = VAR_NAMES`),
#' either stop is called or, if `ifnotfound` has been assigned a value, that
#' value is returned. See [`mget`], which is internally used by this function.
#'
#' The function not only maps to the `LABEL` column, but `to` can be any
#' metadata variable attribute, so the function can also be used, to get, e.g.
#' all `HARD_LIMITS` from the metadata.
#'
#' @param x [character] variable names, character vector, see parameter from
#' @param item_level [data.frame] metadata data frame, if, as a `dataquieR`
#'                               developer, you do not have
#'                               **item-level-metadata**, you should use
#'                               [util_map_labels] instead to avoid consistency
#'                               checks on for item-level `meta_data`.
#' @param meta_data [data.frame] old name for `item_level`
#' @param to [character] variable attribute to map to
#' @param from [character] variable identifier to map from
#' @param ifnotfound [list] A list of values to be used if the item is not
#'                          found: it will be coerced to a list if necessary.
#' @param warn_ambiguous [logical] print a warning if mapping variables from
#'                            `from` to `to` produces ambiguous identifiers.
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return a character vector with:
#'   - mapped values
#' @export
#'
#' @examples
#' \dontrun{
#' meta_data <- prep_create_meta(
#'   VAR_NAMES = c("ID", "SEX", "AGE", "DOE"),
#'   LABEL = c("Pseudo-ID", "Gender", "Age", "Examination Date"),
#'   DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$INTEGER,
#'                  DATA_TYPES$DATETIME),
#'   MISSING_LIST = ""
#' )
#' stopifnot(all(prep_map_labels(c("AGE", "DOE"), meta_data) == c("Age",
#'                                                  "Examination Date")))
#' }
prep_map_labels <- function(x, item_level = "item_level",
                            to = LABEL, from = VAR_NAMES, ifnotfound,
                            warn_ambiguous = FALSE,
                            meta_data_v2,
                            meta_data = item_level) {
  if (!missing(item_level) && !missing(meta_data) &&
      !identical(item_level, meta_data)) {
    util_error(c("You cannot provide both, %s as well as %s",
                 "these arguments are synonyms and must be",
                 "used mutually exclusively"),
               sQuote("item_level"),
               sQuote("meta_data"))
    # see prep_prepare_dataframes
  }

  util_maybe_load_meta_data_v2()
  util_expect_data_frame(meta_data)

  # This is not needed, since labels have not significantly changed
  # meta_data <- suppressMessages(
  #   prep_meta_data_v1_to_item_level_meta_data(
  #     meta_data = meta_data, verbose = TRUE, label_col = LABEL))
  if (!is.data.frame(meta_data)) {
    util_error(
      c("Need metadata as a data frame for mapping",
        "variables to their attributes"),
      applicability_problem = TRUE
    )
  }
  .x <- try(as.character(x), silent = TRUE)
  if (inherits(.x, "try-error") || any(is.na(x) != is.na(.x))) {
    util_error("all variable source names must be characters",
               applicability_problem = TRUE)
  }
  x <- .x
  if (!is.character(to) || length(to) != 1 || !(to %in% colnames(meta_data))) {
    util_error(
      "Need exactly one existing variable attribute name to map variables to",
      applicability_problem = TRUE)
  }
  if (!is.character(from) || length(from) != 1 ||
      !(from %in% colnames(meta_data))) {
    util_error(
      c("Need exactly one variable attribute name",
        "to use as variable name on mapping"), applicability_problem = TRUE)
  }
  util_expect_scalar(warn_ambiguous, check_type = is.logical)
  util_map_labels(x, meta_data, to, from, ifnotfound, warn_ambiguous)
}
