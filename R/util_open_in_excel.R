#' Open a data frame in Excel
#'
#' @param dfr the data frame
#'
#' @keywords internal
#'
#' @return 42
util_open_in_excel <- function(dfr) {
  withr::with_tempdir({
    fn <- paste0(
      as.character(substitute(dfr)), ".xlsx")
    rio::export(dfr, format = "xlsx", file = fn)
    fn <- normalizePath(fn)
    browseURL(fn)
    rstudioapi::showPrompt("Excel", "Close, when you are finished using Excel")
  })
  invisible(42)
}
