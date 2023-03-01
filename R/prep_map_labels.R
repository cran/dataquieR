#' Support function to allocate labels to variables
#'
#' @description
#' Map variables to certain attributes, e.g. by default their labels.
#'
#' @details
#' This function basically calls `colnames(study_data) <- meta_data$LABEL`,
#' ensuring correct merging/joining of study data columns to the corresponding
#' meta data rows, even if the orders differ. If a variable/study_data-column
#' name is not found in `meta_data[[from]]` (default `from = VAR_NAMES`),
#' either stop is called or, if `ifnotfound` has been assigned a value, that
#' value is returned. See [`mget`], which is internally used by this function.
#'
#' The function not only maps to the `LABEL` column, but `to` can be any
#' metadata variable attribute, so the function can also be used, to get, e.g.
#' all `HARD_LIMITS` from the metadata.
#'
#' @param x [character] variable names, character vector, see parameter from
#' @param meta_data [data.frame] meta data frame, if, as a `dataquieR`
#'                               developer, you do not have
#'                               **item-level-metadata**, you should use
#'                               [util_map_labels] instead to avoid consistency
#'                               checks on for item-level `meta_data`.
#' @param to [character] variable attribute to map to
#' @param from [character] variable identifier to map from
#' @param ifnotfound [list] A list of values to be used if the item is not
#'                          found: it will be coerced to a list if necessary.
#' @param warn_ambiguous [logical] print a warning if mapping variables from
#'                            `from` to `to` produces ambiguous identifiers.
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
prep_map_labels <- function(x, meta_data = "item_level",
                            to = LABEL, from = VAR_NAMES, ifnotfound,
                            warn_ambiguous = FALSE) {
  util_expect_data_frame(meta_data)
  meta_data <- prep_meta_data_v1_to_item_level_meta_data(
    meta_data = meta_data, verbose = TRUE, label_col = LABEL)
  if (!is.data.frame(meta_data)) {
    util_error(
      c("Need meta data as a data frame for mapping",
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
