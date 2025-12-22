#' Suppress warnings conditionally
#'
#' @param expr [expression] to evaluate
#' @param classes [character] classes of warning-conditions to suppress
#'
#' @returns the result of `expr`
#'
#' @family condition_functions
#' @concept process
#' @noRd
util_suppress_warnings <- function(expr, classes = "warning") {
  withCallingHandlers(
    expr = expr,
    warning = function(cnd) {
      if (inherits(cnd, classes)) {
        tryInvokeRestart("muffleWarning")
      }
    }
  )
}
