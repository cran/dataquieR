#' Open a data frame in Excel
#'
#' @details
#' if the file cannot be read on function exit, NULL will be returned
#'
#'
#' @param dfr the data frame
#'
#' @export
#'
#' @return potentially modified data frame after dialog was closed
prep_open_in_excel <- function(dfr) { # nocov start
  nms <- as.character(substitute(dfr))
  if (length(nms) != 1)
    nms <- "dataframe"
  fn <- paste0(nms, ".xlsx")
  if (is.character(dfr)) {
    fn <- paste0(dfr, ".xlsx")
  }
  util_expect_data_frame(dfr)
  withr::with_tempdir({
    if (util_ensure_suggested("openxlsx2", "Freeze the header row",
                              err = FALSE)) {
      openxlsx2::wb_workbook()$
        add_worksheet()$
        add_data(x = dfr)$
        add_filter(sheet = 1, rows = 1, seq_len(ncol(dfr)))$
        save(fn)
    } else {
      rio::export(dfr, format = "xlsx", file = fn)
    }
    fn <- normalizePath(fn)
    browseURL(fn)
    rstudioapi::showDialog("Excel", "Close, when you have finished using Excel")
    res <- NULL
    try({
      res <- rio::import(fn)
    })
  })
  invisible(res)
} # nocov end
