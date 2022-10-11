#' Test, if package `anytime` is installed
#'
#' @return `TRUE` if `anytime` is installed.
#'
#' @seealso [requireNamespace]
#' @seealso [https://community.rstudio.com/t/how-can-i-make-testthat-think-i-dont-have-a-package-installed/33441/2](https://community.rstudio.com/t/how-can-i-make-testthat-think-i-dont-have-a-package-installed/33441/2)
util_anytime_installed <- function() {
  requireNamespace("anytime", quietly = TRUE) # nocov
}
