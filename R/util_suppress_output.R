#' Suppress any output to `stdout` using [sink()]
#'
#' @param expr [expression()]
#'
#' @return [invisible()] result of `expr`
#'
#' @family process_functions
#' @keywords internal
util_suppress_output <- function(expr) {
  sink(type = "output", file = nullfile())
  on.exit((function(){sink()})())
  invisible(force(eval(expr, envir = parent.frame())))
}
