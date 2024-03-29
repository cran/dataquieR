#' Test, if package `anytime` is installed
#'
#' @return `TRUE` if `anytime` is installed.
#'
#' @seealso [requireNamespace]
#' @seealso [https://forum.posit.co/t/how-can-i-make-testthat-think-i-dont-have-a-package-installed/33441/2](https://forum.posit.co/t/how-can-i-make-testthat-think-i-dont-have-a-package-installed/33441/2)
#'
#' @seealso [util_ensure_suggested]
#' @keywords internal
util_anytime_installed <- function() {
  requireNamespace("anytime", quietly = TRUE) # nocov
}
