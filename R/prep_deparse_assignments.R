#' Convert two vectors from a code-value-table to a key-value list
#'
#'
#' @param codes codes, numeric or dates (as default, but string codes can be
#'              enabled using the option 'mode', see below)
#' @param labels [character] labels, same length as codes
#' @param split_char [character] split character character to split
#'                               code assignments
#' @param mode [character] one of two options to insist on numeric or datetime
#'                         codes (default) or to allow for string codes
#'
#' @return a vector with assignment strings for each row of
#'         `cbind(codes, labels)`
#' @export
prep_deparse_assignments <- function(codes, labels, split_char = SPLIT_CHAR,
                                     mode = c("numeric_codes",
                                              "string_codes")) {
  mode <- util_match_arg(mode)
  if (length(codes) != length(labels)) {
    util_error("%s and %s must have the same length",
               dQuote("values"),
               dQuote("labels"), applicability_problem = TRUE)
  }
  if (mode == "numeric_codes" && (
    suppressWarnings(!all(
      is.na(as.numeric(codes)) == is.na(codes) |
      is.na(lubridate::as_datetime(codes)) == is.na(codes)
    )))) {
    util_error("All codes must be finite numeric or date/time values",
               applicability_problem = TRUE)
  } else {
    codes <- as.character(codes)
  }
  if (any(grepl(split_char, labels, fixed = TRUE))) {
    util_message("Removed seperator characters (%s) from the labels",
                 dQuote(split_char), applicability_problem = TRUE)
    labels <- gsub(split_char, "", labels, fixed = TRUE)
  }
  if (!!length(codes) && length(codes) == length(labels)) {
    paste(codes, "=", labels,
          collapse = sprintf(" %s ", split_char))
  } else {
    split_char
  }
}
