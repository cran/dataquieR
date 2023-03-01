#' Internal constructor for the internal class [`r dataquieR_resultset_class2`].
#'
#' creates an object of the class [`r dataquieR_resultset_class2`].
#'
#' @param ... properties stored in the object
#'
#' @details
#' The class features the following methods:
#'  `r paste0(" - [\u0060", methods(class = dataquieR_resultset_class2), "\u0060]", collapse = "\n")`
#'
#' @return an object of the class [`r dataquieR_resultset_class2`].
#' @seealso [dq_report2]
#' @aliases .dataquieR_resultset2
dataquieR_resultset2 <- function(...) {
  this <- list(...)
  # dataquieR_resultset_verify2(this) # TODO: Implement me!!
  class(this) <- dataquieR_resultset_class2
  this
}
dataquieR_resultset_class2 <- "dataquieR_resultset2"
