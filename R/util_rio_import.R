#' Import a data frame
#'
#' see [`rio::import`], but with argument `keep_types` and modified error
#' handling.
#'
#' @param fn the file name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param ... additional arguments for [rio::import]
#'
#' @return [data.frame] as in [rio::import]
#' @keywords internal
util_rio_import <- function(fn, keep_types, ...) { # FIXME: Detect invalid utf-8 codes and remove such.
  util_expect_scalar(keep_types,
                     check_type = is.logical,
                     error_message =
                       sprintf("%s needs to be to be a logical value.",
                               sQuote("keep_types")))
  if (endsWith(tolower(fn), ".ods")) {
    col_types <- NA
  } else if (!endsWith(tolower(fn), ".xls") &&
             !endsWith(tolower(fn), ".xlsx")) {
    col_types <- "character"
  } else {
    col_types <- "text"
  }
  r <- suppressWarnings(
    util_suppress_output(try(rio::import(fn, ...), silent = TRUE)))
  if (!keep_types && !inherits(r, "try-error")) {
    r2 <- util_suppress_output(suppressWarnings(
      try(rio::import(fn, ..., col_types = col_types,
                      colClasses = "character"), silent = TRUE)))
    if (!inherits(
      r2, "try-error"
    ) && !!prod(dim(r2))) {
      r <- r2
    } else if (!!prod(dim(r))) {
      util_warning("Could not read %s as text-only data frame",
                   dQuote(fn),
                   applicability_problem = TRUE)
    }
  }
  r
}
