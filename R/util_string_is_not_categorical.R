#' Utility function for judging whether a character vector does not appear to
#' be a categorical variable
#'
#' The function considers the following properties:
#' - the maximum number of characters (to identify free text fields with
#' long entries),
#' - the relative frequency of punctuation and space characters per element (to
#' identify, e.g., JSON or XML elements, which are structured by those
#' characters),
#' - the relative frequency of elements (categorical variables would have
#' a low proportion of unique values in comparison to other variables).
#'
#' @param vec a character vector
#'
#' @return TRUE or FALSE
#'
#' @noRd
util_string_is_not_categorical <- function(vec) {
  vec <- vec[!util_empty(vec)]
  if (length(vec) == 0) {
    return(TRUE)
  }
  if (!is.character(vec)) {
    util_warning("Wrong use of util_string_is_not_categorical!")
    return(FALSE)
  }
  vec_uniq <- unique(vec)
  # free-text fields can contain very long strings
  long_elements <- max(nchar(vec_uniq,
                             type = "bytes", # must also work for wrong encoding
                             allowNA = TRUE,
                             keepNA = FALSE)) > 100
  # JSON, XML or similar elements contain more punctuation symbols and possibly
  # space characters than expected for categorical variables
  many_non_alphanum_char <- median(
    vapply(vec_uniq, FUN.VALUE = numeric(1),
         function(vv) {
           sum(
             grepl('[[:punct:]|[:space:]]',
                   unlist(strsplit(vv, split = ""))
                   )
             ) / nchar(vv,
                       type = "bytes", # must also work for wrong encoding
                       allowNA = TRUE,
                       keepNA = FALSE)
         })
  ) > 0.4
  # For categorical variables, we expect a low proportion of unique values. So
  # if there are only few duplicates, the vector is most likely not a
  # categorical variable.
  if (length(vec) >= 20) {
    few_duplicates <- (length(vec) - length(vec_uniq)) / length(vec) < 0.2
  } else {
    few_duplicates <- FALSE
  }
  return(long_elements | many_non_alphanum_char | few_duplicates)
}
