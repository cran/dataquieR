#' If on Windows, hide a file
#'
#' @param fn the file path + name
#'
#' @return `invisible(NULL)`
#'
#' @noRd
util_hide_file_windows <- function(fn) {
  if (.Platform$OS.type != "windows") return();
  fn <- normalizePath(fn)
  if (file.exists(fn)) {
    system(sprintf("attrib +h %s", shQuote(fn, type = "cmd")))
  }
  invisible(NULL)
}
