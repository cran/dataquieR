#' Find a free variable name in the metadata
#'
#' looks up the column `target` in `meta_data` and searches for an unused entry
#' starting with `prefix`. Also excludes `also_not` from the possible results.
#'
#' @param meta_data item-level metadata.
#' @param prefix prefix for the returned variable name
#' @param target target column in metadata for uniqueness
#' @param also_not other possible ambiguous names to exclude
#'
#' @returns an unused unique name
#' @noRd
util_free_varname <- function(meta_data, prefix, target, also_not) {
  res <- prefix
  i <- 0
  while (res %in% c(meta_data[[target]], also_not)) {
    i <- i + 1
    res <- paste0(prefix, "_", i)
  }
  res
}
