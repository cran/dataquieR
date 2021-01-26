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
#'                                              displayed, if not missing
#'                                              codes are available for a
#'                                              variable.
#'
#' @return [numeric] vector of missing codes.
#'
util_get_code_list <- function(x, code_name, split_char = SPLIT_CHAR, mdf,
                               label_col = VAR_NAMES,
                               warning_if_no_list = TRUE) {
  if (!(label_col %in% names(mdf))) {
    if (warning_if_no_list) {
      util_warning(
        "Meta data does not provide a column called %s for the labels.",
        dQuote(label_col))
    }
    return(numeric(0))
  }
  if (!(code_name %in% names(mdf))) {
    if (warning_if_no_list) {
      util_warning(
        c("Meta data does not provide a column called %s",
          "for replacing codes with NAs."),
        dQuote(code_name))
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
        dQuote(x))
    }
    return(numeric(0))
  }

  cl <- c_list[[1]] # ?

  if (is.character(cl) && length(cl) == 1) {
    res <- unlist(strsplit(cl, split_char, fixed = TRUE))
    res[res == ""] <- NA
    numeric_res <- suppressWarnings(as.numeric(res))
    if (sum(is.na(res)) < sum(is.na(numeric_res))) {
      util_warning(
        "Some codes (%s) were not numeric for %s: %s, these will be ignored",
         dQuote(code_name),
         dQuote(x),
         paste(sQuote(res[is.na(numeric_res) != is.na(res)]),
               collapse = ", "))
    }
    numeric_res
  } else {
    cl
  }
}
