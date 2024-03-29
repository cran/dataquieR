#' Import list of data frames
#'
#' see [`rio::import_list`], but with argument `keep_types` and modified error
#' handling.
#'
#' @param fn the file name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param ... additional arguments for [rio::import_list]
#'
#' @return [list] as in [rio::import_list]
#' @keywords internal
util_rio_import_list <- function(
    fn,
    keep_types,
    ...
    ) {
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

  r <-
    util_suppress_output(suppressMessages(suppressWarnings(
      try(rio::import_list(fn, ...),
                                            silent = TRUE))))
  if (!keep_types && !inherits(r, "try-error")) {
    r2 <-
      suppressMessages(suppressWarnings(util_suppress_output(
                                        try(rio::import_list(fn,
                                                             col_types =
                                                               col_types,
                                                             colClasses =
                                                               "character",
                                                             ...),
                                            silent = TRUE))))
    if (!inherits(
      r2, "try-error"
    ) && !all(vapply(r2, length, FUN.VALUE = integer(1)) == 0)) {
      r <- r2
    } else if (!all(vapply(r, length, FUN.VALUE = integer(1)) == 0)) {
      util_warning("Could not read %s as text-only data frames",
                   dQuote(fn),
                   applicability_problem = TRUE)
    }
  }
  r
}
