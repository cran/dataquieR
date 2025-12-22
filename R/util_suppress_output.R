#' Suppress any output to `stdout` using [sink()]
#'
#' @param expr [expression()]
#' @param warns [environment()] if given, warnings will be stored in this
#'                              environment in a newly initialized object
#'                              named `warns`.
#'
#' @return [invisible()] result of `expr`
#'
#' @family process_functions
#' @noRd
util_suppress_output <- function(expr, warns) {
  if (missing(warns)) warns <- new.env(parent = emptyenv())
  warns$warns <- list()
  sink(type = "output", file = nullfile())
  on.exit((function(){sink()})())
  withCallingHandlers(
    invisible(force(eval(expr, envir = parent.frame()))),
    warning = function(w) {
      warns$warns <- c(warns$warns, list(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      invokeRestart("muffleMessage")
    }
  )
}
