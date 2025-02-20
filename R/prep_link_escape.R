#' Prepare a label as part of a link for `RMD` files
#'
#' @param s the label
#' @param html prepare the label for direct `HTML` output instead of `RMD`
#'
#' @return the escaped label
#' @export
#'
prep_link_escape <- function(s, html = FALSE) { # TODO: Maybe use URLencode(), but take care: also file names are entered, here
  r <- gsub("*", "X", s, fixed = TRUE)
  r <- gsub("\u00b2", "", r, fixed = TRUE)
  r <- gsub("\u00b3", "", r, fixed = TRUE)
  r <- gsub("\u00B0", "", r, fixed = TRUE)
  r <- gsub("%", "", r, fixed = TRUE)
  r <- gsub("/", "", r, fixed = TRUE)
  r <- gsub("\\", "", r, fixed = TRUE)
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
  r <- gsub("?", "Q", r, fixed = TRUE)
  r <- gsub("#", "", r, fixed = TRUE)
  r <- gsub(")", "", r, fixed = TRUE)
  r <- gsub("(", "", r, fixed = TRUE)
  r <- gsub("|", "\uFF5C", r, fixed = TRUE)
  # r <- gsub("[\"/ <>&;°@µ~]", "", r, perl = TRUE)
  gsub("\\s+", "", r, perl = TRUE)
}
