#' Paste strings but keep NA
#'
#' @param ... other arguments passed to `paste`
#'
#' @return [character] pasted strings
#'
#' @noRd
util_paste_with_na <- function(...) {
  res <- paste(...)
  res[as.logical(rowSums(do.call(data.frame, lapply(list(...), is.na))))] <-
    NA_character_
  res
}

#' Paste strings but keep NA (`paste0`)
#'
#' @param ... other arguments passed to `paste0`
#'
#' @return [character] pasted strings
#'
#' @noRd
util_paste0_with_na <- function(...) {
  res <- paste0(...)
  res[as.logical(rowSums(do.call(data.frame, lapply(list(...), is.na))))] <-
    NA_character_
  res
}
