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

#' Convert a category to a number (`1:5`)
#'
#' @param category vector with categories
#'
#' @return an integer
#' @family summary_functions
#' @keywords internal
util_as_integer_cat <- function(category) {
  if (!is.ordered(category)) category <- util_as_cat(category)
  util_stop_if_not(all(startsWith(levels(category), "cat")))
  numcat <- substring(levels(category), 4)
  util_stop_if_not(all(numcat == as.integer(numcat)))
  levels(category) <- gsub("^cat", "", levels(category))
  util_as_numeric(category)
}
