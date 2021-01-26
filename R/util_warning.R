#' Produce a warning message with a useful short stack trace.
#'
#' @param m warning message or a [simpleWarning]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param immediate. logical, indicating if the call should be output
#'                   immediately, even if `getOption("warn") <= 0`.
#'                   See also [base::warning].
#'
#' @return invisible(NULL).
#'
util_warning <- function(m, ..., immediate. = FALSE) {
  start_from_call <- util_find_first_externally_called_functions_in_stacktrace()
  caller. <- as.character(sys.call(1 - start_from_call))[[1]]
  if (caller. == "do.call") {
    what <- try(sys.frame(1 - start_from_call)[["what"]], silent = TRUE)
    if (is.function(what)) {
      what <- as.character(sys.call(1 - start_from_call))[[2]]
    }
    caller. <- sprintf("%s (%s)", caller., what)
  }

  stacktrace <- (paste0(paste0("> ", vapply(FUN.VALUE = character(1), rev(
    sys.calls()), function(sc) paste0(deparse(sc, nlines = 2),
                                      collapse = "\n"))[[
                                        start_from_call]]), collapse = "\n"))
    # https://stat.ethz.ch/pipermail/r-help/2011-November/295273.html
  if (inherits(m, "simpleWarning")) {
    m <- conditionMessage(m)
    if (m == "") {
      m <- "Warning"
    }
    warning(paste0("In ", caller., ": ", m, "\n", stacktrace, "\n"),
            call. = FALSE,
            immediate. = immediate.)
  } else {
    warning(paste0("In ", caller., ": ", sprintf(paste0(m, collapse = " "),
                                                 ...), "\n", stacktrace, "\n"),
            call. = FALSE, immediate. = immediate.)
  }
  invisible(NULL)
}
