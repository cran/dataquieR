#' Extract all ids from a list of `htmltools` objects
#'
#' @param pages the list of objects
#'
#' @return a character vector with valid targets
#' @family html
#' @keywords internal
util_extract_all_ids <- function(pages) {
  all_ids <- lapply(lapply(pages, .util_extract_all_ids_from_htmltools), unlist)
  all_ids <- lapply(all_ids, unname)
  all_ids <- all_ids[!vapply(all_ids, is.null, FUN.VALUE = logical(1))]
  all_ids <- unlist(
    lapply(names(all_ids), function(fn) { paste0(fn, "#", all_ids[[fn]]) } ))
  all_ids
}
.util_extract_all_ids_from_htmltools <- function(html_stuff) {
  r <- list()
  if ("children" %in% names(html_stuff)) {
    r <- c(r, .util_extract_all_ids_from_htmltools(html_stuff$children))
    html_stuff$children <- NULL
  }
  if ("attribs" %in% names(html_stuff)) {
    if ("id" %in% names(html_stuff$attribs)) {
      r <- c(r, html_stuff$attribs$id)
    }
    html_stuff$attribs <- NULL
  }
  if (is.list(html_stuff) || length(html_stuff) > 1) {
    c(r, lapply(html_stuff, .util_extract_all_ids_from_htmltools))
  } else {
    r
  }
}
