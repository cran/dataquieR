#' Prepare a label as part of a title text for `RMD` files
#'
#' @param s the label
#' @param html prepare the label for direct `HTML` output instead of `RMD`
#'
#' @return the escaped label
#' @export
#'
prep_title_escape <- function(s, html = FALSE) {
  # r <- gsub("&", "&amp;", s, fixed = TRUE)
  # r <- gsub("^#+", "", r, perl = TRUE)
  # r <- gsub("/", "&#47;", r, fixed = TRUE)
  # r <- gsub("@", "&#64;", r, fixed = TRUE)
  # r <- gsub("<", "&lt;", r, fixed = TRUE)
  # r <- gsub(">", "&gt;", r, fixed = TRUE)
  # r <- gsub("\"", "&#92;", r, fixed = TRUE)
  # r <- gsub("_", "&#95;", r, fixed = TRUE)
  # r <- gsub("~", "&#126;", r, fixed = TRUE)
  if (html) {
    r <- lapply(s, htmltools::pre)
    r <- vapply(s, as.character, FUN.VALUE = character(1))
  } else {
    r <- s
    r <- gsub("`", "", r, fixed = TRUE)
    r <- paste0("`", r, "`")
  }
  r
}
