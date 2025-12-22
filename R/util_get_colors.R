#' Get colors for each russet `DQ` category
#'
#' @return named vector of colors, names are categories (e.g, "1" to "5")
#'         values are colors as `HTML` `RGB` hexadecimal strings
#' @family summary_functions
#' @concept process
#' @noRd
util_get_colors <- function() {

  rsts <- util_get_ruleset_formats()

  cat_names <- as.character(rsts$category)
  if (!"color" %in% colnames(rsts)) {
    rsts[["color"]] <-
      rep("128 128 128", nrow(rsts))
  }
  colors <- rsts[["color"]]
  colors <- util_col2rgb(colors)
  setNames(colors, nm = cat_names)
}
