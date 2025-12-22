#' Tail for R
#'
#' @param file_name [character] a file name
#' @param bytes [integer] number of bytes to return, careful with multi-character
#'.             encodings like `utf-8`
#'
#' @returns last `bytes` bytes from `file_name`
#' @noRd
util_tail_file <- function(file_name, bytes) {
  s <- file.size(file_name)
  offs <- max(0, s - bytes)
  con <- file(file_name, open = "rb")
  withr::defer(close(con))
  seek(con, offs, rw = "rb")
  rawToChar(readBin(con, "raw", bytes))
}

#' Verify, if an `HTML` file looks complete
#'
#' @param file_name [character] file name
#'
#' @returns `TRUE`, if the file features an `HTML`-end tag within its last 20
#'                  bytes.
#' @noRd
util_is_html_file_complete <- function(file_name) {
  r <- suppressWarnings(try(
    file.exists(file_name) &&
    file.access(file_name, 2) == 0 &&
    endsWith(tolower(trimws(util_tail_file(file_name, 20))),  "</html>"),
    silent = TRUE))
  r <- !util_is_try_error(r) && r
}
