#' Fetch a missing code list from the metadata
#'
#' get missing codes from metadata (e.g. [MISSING_LIST] or [JUMP_LIST])
#'
#' @param x [variable] the name of the variable to retrieve code lists for.
#'                     only one variable at a time is supported, *not*
#'                     vectorized!!
#' @param code_name [variable attribute] [JUMP_LIST] or [MISSING_LIST]:
#'                                       Which codes to retrieve.
#' @param split_char [character] len = 1. Character(s) used to separate
#'                               different codes in the metadata, usually `|`,
#'                               as in `99999|99998|99997`.
#' @param mdf [data.frame] the data frame that contains metadata
#'                         attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param warning_if_no_list [logical] len = 1. If `TRUE`, a warning is
#'                                              displayed, if no missing
#'                                              codes are available for a
#'                                              variable.
#' @param warning_if_unsuitable_list [logical] len = 1. If `TRUE`, a warning is
#'                                              displayed, if missing
#'                                              codes do not match with a
#'                                              variable' data type.
#'
#' @return [numeric] vector of missing codes.
#'
#' @family missing_functions
#' @concept metadata_management
#' @noRd
util_get_code_list <- function(x, code_name, split_char = SPLIT_CHAR, mdf,
                               label_col = VAR_NAMES,
                               warning_if_no_list = TRUE,
                               warning_if_unsuitable_list = TRUE) {
  if (!(label_col %in% names(mdf))) {
    if (warning_if_no_list) {
      util_warning(
        "Metadata does not provide a column called %s for the labels.",
        dQuote(label_col),
        applicability_problem = TRUE)
    }
    return(numeric(0))
  }
  if (!(code_name %in% names(mdf))) {
    if (warning_if_no_list) {
      util_warning(
        c("Metadata does not provide a column called %s",
          "for replacing codes with NAs."),
        dQuote(code_name),
        applicability_problem = TRUE)
    }
    return(numeric(0))
  }

  c_list <- mdf[[code_name]][mdf[[label_col]] == x]

  if (length(c_list) != 1 || is.na(c_list)) {
    if (warning_if_no_list) {
      util_warning(
        c("Could not find %s for %s in the meta_data",
          "for replacing codes with NAs."),
        dQuote(code_name),
        dQuote(x),
        applicability_problem = TRUE)
    }
    return(numeric(0))
  }

  cl <- c_list[[1]] # ?

  if (length(cl) != 1) { # nocov start
    util_error(c("Internal error: Have more than one codelist for a",
                 "variable in the metadata -- this should not happen."))
  }  # nocov end

  # res <- unlist(strsplit(cl, split_char, fixed = TRUE))
  if (code_name != VALUE_LABELS) {
    r <- util_parse_assignments(cl, split_char = SPLIT_CHAR)
  } else {
    # FIXME: "Deprecated, should never be used"
    r <- util_parse_assignments(cl,
                                split_on_any_split_char = TRUE,
                                split_char = c(SPLIT_CHAR, '<'),)
  }
  if (length(r) == 0) {
    r <- setNames(character(0), character(0))
  } else {
    r <- setNames(unlist(r, recursive = FALSE), nm = names(r))
  }
  res <- names(r)
  res[util_empty(res)] <- NA
  if (DATA_TYPE %in% colnames(mdf) &&
      any(!is.na(mdf[[DATA_TYPE]])) &&
      all(na.rm = TRUE, mdf[[DATA_TYPE]][
        !is.na(mdf[[label_col]]) & mdf[[label_col]] == x] ==
      DATA_TYPES$DATETIME)) {
    dt_res <- suppressWarnings(util_parse_date(res))
    if (sum(is.na(res)) < sum(is.na(dt_res))) {
      if (warning_if_unsuitable_list) util_warning(
"Some codes (%s) were not datetime/assignment for %s: %s, these will be ignored",
        dQuote(code_name),
        dQuote(x),
        paste(sQuote(res[is.na(dt_res) != is.na(res)]),
              collapse = ", "),
        applicability_problem = TRUE)
    }
    names(dt_res) <- r
    r <- dt_res[!is.na(dt_res)]
  } else if (DATA_TYPE %in% colnames(mdf) &&
              any(!is.na(mdf[[DATA_TYPE]])) &&
              all(na.rm = TRUE, mdf[[DATA_TYPE]][
                !is.na(mdf[[label_col]]) & mdf[[label_col]] == x] ==
                DATA_TYPES$TIME)) {
    to_res <- suppressWarnings(util_parse_time(res))
    if (sum(is.na(res)) < sum(is.na(to_res))) {
      if (warning_if_unsuitable_list) util_warning(
        "Some codes (%s) were not time/assignment for %s: %s, these will be ignored",
        dQuote(code_name),
        dQuote(x),
        paste(sQuote(res[is.na(to_res) != is.na(res)]),
              collapse = ", "),
        applicability_problem = TRUE)
    }
    names(to_res) <- r
    r <- to_res[!is.na(to_res)]
  } else {
    numeric_res <- suppressWarnings(as.numeric(res))
    if (sum(is.na(res)) < sum(is.na(numeric_res))) {
      if (warning_if_unsuitable_list) util_warning(
"Some codes (%s) were not numeric/assignment for %s: %s, these will be ignored",
        dQuote(code_name),
        dQuote(x),
        paste(sQuote(res[is.na(numeric_res) != is.na(res)]),
              collapse = ", "),
        applicability_problem = TRUE)
    }
    names(numeric_res) <- r
    r <- numeric_res[!is.na(numeric_res)]
  }
  r
}
