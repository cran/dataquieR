#' Produce an error message with a useful short stack trace. Then it stops the
#' execution.
#'
#' @param m error message or a [simpleError]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] error indicates unsuitable resp_vars
#'
#' @return nothing, its purpose is to stop.
#'
util_error <- function(m, ..., applicability_problem = NA) {
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

  stacktrace <- (paste0(paste0("> ", vapply(FUN.VALUE = character(1),
                                            rev(sys.calls()), function(sc)
                                              paste0(deparse(sc, nlines = 2),
                                                     collapse = "\n"))[[
                                                       start_from_call]]),
                        collapse = "\n"))
    # https://stat.ethz.ch/pipermail/r-help/2011-November/295273.html

  if (inherits(m, "simpleError")) {
    m <- conditionMessage(m)
    if (m == "") {
      m <- "Error"
    }
    ec <-
      errorCondition(paste0("In ", caller., ": ", m, "\n", stacktrace, "\n"))
  } else {
    mm <- paste0(m,
           collapse =
             " ")
    if (nchar(mm) > 8192) {
      mm <- substr(mm, 1, 8192)
      mm <- sub("(%[^%]$)", "\\1", mm, perl = TRUE)
    }
    ec <-
      errorCondition(paste0("In ", caller., ": ", sprintf(mm, ...),
                "\n", stacktrace, "\n"), call. = FALSE)
  }
  attr(ec, "applicability_problem") <- applicability_problem
  stop(ec)
}
