#' Internal constructor for the internal class [`r dataquieR_resultset_class`].
#'
#' creates an object of the class [`r dataquieR_resultset_class`].
#'
#' @param ... properties stored in the object
#'
#' @details
#' The class features the following methods:
#'  `r paste0(" * [", methods(class = "dataquieR_resultset"), "]")`
#'
#' @return an object of the class [`r dataquieR_resultset_class`].
#' @seealso [dq_report]
dataquieR_resultset <- function(...) {
  this <- list(...)
  dataquieR_resultset_verify(this)
  class(this) <- dataquieR_resultset_class
  this
}
dataquieR_resultset_class <- "dataquieR_resultset"
