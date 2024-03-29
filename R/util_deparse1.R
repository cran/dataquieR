#' @name util_deparse1
#'
#' @title Expression De-Parsing
#'
#' @description
#'
#' Turn unevaluated expressions into character strings.
#'
#' @details
#'
#' This is a simple utility function for R < 4.0.0 to ensure a string
#' result (character vector of length one),
#' typically used in name construction, as `util_deparse1(substitute(.))`.
#'
#' This avoids a dependency on `backports` and on R >= 4.0.0.
#'
#' @param expr any R expression.
#' @param collapse a string, passed to `paste()`
#' @param width.cutoff integer in \[20, 500\] determining the
#'                     cutoff (in bytes) at which line-breaking is tried.
#' @param ... further arguments passed to `deparse()`.
#'
#' @return the deparsed expression
#'
#' @family condition_functions
#' @concept process
#' @keywords internal

NULL # Documentation is assigned to null (@name in line 1)

if (exists("deparse1", envir = baseenv())) {
  util_deparse1 <- base::deparse1
} else {
  util_deparse1 <-
    function (expr,
              collapse = " ",
              width.cutoff = 500L,
              ...) {
      paste(
        deparse(
          expr = expr,
          width.cutoff = width.cutoff,
          ...),
        collapse = collapse
      )
    }
}
