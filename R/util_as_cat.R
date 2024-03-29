#' Convert a category to an ordered factor (`1:5`)
#'
#' @param category vector with categories
#'
#' @return an ordered factor
#' @family summary_functions
#' @keywords internal
util_as_cat <- function(category) {
  if (is.character(category)) {
    category <- gsub("^cat", "", category)
  }
  category <- suppressWarnings(as.integer(category))
  range <- 1:max(5L, category, na.rm = TRUE)
  factor(
    category,
    levels = range,
    labels = paste0("cat", range),
    ordered = TRUE
  )
}
