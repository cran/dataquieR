#' Get labels for each russet `DQ` category
#'
#' @return named vector of labels, names are categories (e.g, "1" to "5")
#'         values are labels
#' @family summary_functions
#' @noRd
util_get_labels_grading_class <- function() {

  rsts <- util_get_ruleset_formats()

  cat_names <- as.character(rsts$category)
  if (!"color" %in% colnames(rsts)) {
    rsts[["color"]] <-
      rep("128 128 128", nrow(rsts))
  }
  labels <- rsts[["label"]]
  setNames(labels, nm = cat_names)
}
