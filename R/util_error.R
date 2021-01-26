#' Produce an error message with a useful short stack trace. Then it stops the
#' execution.
#'
#' @param m error message or a [simpleError]
#' @param ... arguments for [sprintf] on m, if m is a character
#'
#' @return nothing, its purpose is to stop.
#'
util_error <- function(m, ...) {
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
    stop(paste0("In ", caller., ": ", m, "\n", stacktrace, "\n"), call. = FALSE)
  } else {
    stop(paste0("In ", caller., ": ", sprintf(paste0(m, collapse = " "), ...),
                "\n", stacktrace, "\n"), call. = FALSE)
  }
}
