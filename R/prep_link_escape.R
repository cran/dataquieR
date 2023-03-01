#' Prepare a label as part of a link for `RMD` files
#'
#' @param s the label
#' @param html prepare the label for direct `HTML` output instead of `RMD`
#'
#' @return the escaped label
#' @export
#'
prep_link_escape <- function(s, html = FALSE) {
  r <- gsub("/", "", s, fixed = TRUE)
  r <- gsub("'", "", r, fixed = TRUE)
  r <- gsub(":", "", r, fixed = TRUE)
  r <- gsub("\u20ac", "", r, fixed = TRUE)
  r <- gsub("`+", "", r, perl = TRUE)
  r <- gsub("\\$+", "", r, perl = TRUE)
  r <- gsub("^#+", "", r, perl = TRUE)
  r <- gsub("@", "", r, fixed = TRUE)
  r <- gsub("<", "", r, fixed = TRUE)
  r <- gsub(">", "", r, fixed = TRUE)
  r <- gsub("&", "", r, fixed = TRUE)
  r <- gsub("\"", "", r, fixed = TRUE)
  r <- gsub("_", "", r, fixed = TRUE)
  r <- gsub("~", "", r, fixed = TRUE)
  r <- gsub("\u00b5", "", r, fixed = TRUE)
  r <- gsub(";", "", r, fixed = TRUE)
  r <- gsub("\u00b0", "", r, fixed = TRUE)
  r <- gsub("?", "", r, fixed = TRUE)
  r <- gsub("#", "", r, fixed = TRUE)
  # r <- gsub("[\"/ <>&;°@µ~]", "", r, perl = TRUE)
  gsub("\\s+", "", r, perl = TRUE)
}
