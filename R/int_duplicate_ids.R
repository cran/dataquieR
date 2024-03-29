#' Check for duplicated IDs
#'
#' @description
#' This function tests for duplicates entries in identifiers. It is possible to
#' check duplicated identifiers by study segments or to consider only selected
#' segments.
#'
#' [Indicator]
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param ... Depending on `level`, passed to either
#'            [util_int_duplicate_ids_segment] or
#'            [util_int_duplicate_ids_dataframe]
#'
#' @return a [list]. Depending on `level`, see
#'   [util_int_duplicate_ids_segment] or
#'   [util_int_duplicate_ids_dataframe] for a description of the outputs.
#'
#' @export
#'
int_duplicate_ids <- function(level = c("dataframe", "segment"),
                              ...) {
  level <- util_match_arg(level)
  cl <- sys.call()
  fname <- paste("util", util_deparse1(cl[[1]]), level, sep = "_")
  cl2 <- do.call("call",
                 list(fname, level = level, ...))
  eval(cl2)
}
