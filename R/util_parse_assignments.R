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
#' @param multi_variate_text don't paste text but parse element-wise
#'
#' @return the parsed assignments as a named list
#'
#' @importFrom stats setNames
#'
util_parse_assignments <- function(text, split_char = SPLIT_CHAR,
                                   multi_variate_text = FALSE) { # Dont change default here, many calls of this function rely on a non-list-result
  if (!multi_variate_text) {
    if (all(util_empty(text))) {
      text <- NA_character_
    } else {
      text <- paste0(text, collapse = "\n")
    }
  }
  res <- lapply(text, function(x) {
    if (all(util_empty(gsub(split_char, "", fixed = TRUE, x)))) {
      return(setNames(list(), nm = character(0)))
    }
    assignments <- base::strsplit(x = as.character(x),
                                  split = split_char, fixed = TRUE)[[1]]

    keys <- trimws(gsub(pattern = "(?ms)\\s*=\\s*.*$", replacement = "",
                        x = assignments, perl = TRUE))
    values <- trimws(gsub(pattern = "(?ms)^.*?\\s*=\\s*", replacement = "\\1",
                          x = assignments, perl = TRUE))
    as.list(setNames(values, keys))
  })
  if (!multi_variate_text) {
    if (length(res) != 1) { # nocov start
      util_error(c("univariate use of util_parse_assignments returned %d",
                   "results. Sorry, this should not happen, internal error."),
                 length(res))
    } # nocov end
    return(res[[1]])
  } else {
    return(res)
  }
}
