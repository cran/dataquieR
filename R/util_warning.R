#' Produce a warning message with a useful short stack trace.
#'
#' @param m warning message or a [simpleWarning]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] warning indicates unsuitable resp_vars
#'
#' @return invisible(NULL).
#'
util_warning <- function(m, ...,
                         applicability_problem = NA) {
  stopifnot(length(applicability_problem) == 1 &&
              is.logical(applicability_problem))
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
    wc <-
      warningCondition(paste0("In ", caller., ": ", m, "\n", stacktrace, "\n"))
  } else {
    mm <- paste0(m,
                 collapse =
                   " ")
    if (nchar(mm) > 8192) {
      mm <- substr(mm, 1, 8192)
      mm <- sub("(%[^%]$)", "\\1", mm, perl = TRUE)
    }

    wc <-
      warningCondition(paste0("In ", caller., ": ", sprintf(mm,
                                                 ...), "\n", stacktrace, "\n"))
  }
  attr(wc, "applicability_problem") <- applicability_problem
  warning(wc)
  invisible(NULL)
}
