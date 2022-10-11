#' Utility function to parse assignments
#'
#' This function parses labels & level assignments in the format
#' `1 = male | 2 = female`. The function also handles `m = male | f = female`,
#' but this would not match the metadata concept. The split-character can
#' be given, if not the default from [SPLIT_CHAR] is to be used, but this
#' would also violate the metadata concept.
#'
#' @param text Text to be parsed
#' @param split_char Character separating assignments
#'
#' @return the parsed assignments as a named list
#'
#' @importFrom stats setNames
#'
util_parse_assignments <- function(text, split_char = SPLIT_CHAR) {
  text <- paste0(text, collapse = "\n")
  assignments <- base::strsplit(x = text, split = SPLIT_CHAR, fixed = TRUE)[[1]]

  keys <- trimws(gsub(pattern = "(?ms)\\s*=\\s*.*$", replacement = "",
                      x = assignments, perl = TRUE))
  values <- trimws(gsub(pattern = "(?ms)^.*?\\s*=\\s*", replacement = "\\1",
                        x = assignments, perl = TRUE))

  return(as.list(setNames(values, keys)))
}
