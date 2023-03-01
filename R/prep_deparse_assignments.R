#' Convert two vectors from a code-value-table to a key-value list
#'
#'
#' @param codes [integer] codes
#' @param labels [character] labels, same length as codes
#' @param split_char [character] split character character to split
#'                               code assignments
#'
#' @return a vector with assignment strings for each row of
#'         `cbind(codes, labels)`
#' @export
prep_deparse_assignments <- function(codes, labels, split_char = SPLIT_CHAR) {
  if (length(codes) != length(labels)) {
    util_error("%s and %s must have the same length",
               dQuote("values"),
               dQuote("labels"))
  }
  if (suppressWarnings(!all(util_is_integer(as.numeric(codes)) |
           !is.na(lubridate::as_datetime(codes))))) {
    util_error("All codes must be integer or date/time values")
  }
  if (any(grepl(split_char, labels, fixed = TRUE))) {
    util_warning("Removed seperator characters (%s) from the labels",
                 dQuote(split_char))
    labels <- gsub(split_char, "", labels, fixed = TRUE)
  }
  if (!!length(codes) && length(codes) == length(labels)) {
    paste(codes, "=", labels,
          collapse = sprintf(" %s ", split_char))
  } else {
    split_char
  }
}
