#' Extract Parts of a `dataquieR` Result Object
#'
#' @param x the `dataquieR` result object
#'
#' @param ... arguments passed to the implementation for lists.
#'
#' @return the sub-list of the `dataquieR` result object with all messages
#'         still attached
#'
#' @seealso  [base::Extract]
#' @export
#'
#' @keywords internal
`[.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  attr(r, "error") <- attr(x, "error")
  attr(r, "message") <- attr(x, "message")
  attr(r, "warning") <- attr(x, "warning")
  class(r) <- unique(c("dataquieR_result", class(r)))
  r
}

#' Extract Elements of a `dataquieR` Result Object
#'
#' @param x the `dataquieR` result object
#'
#' @param ... arguments passed to the implementation for lists.
#'
#' @return the element of the `dataquieR` result object with all messages
#'         still attached
#'
#' @seealso  [base::Extract]
#' @export
#'
#' @keywords internal
`[[.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  if (!is.null(r) && !inherits(x, "ggplot")) {
    attr(r, "error") <- attr(x, "error")
    attr(r, "message") <- attr(x, "message")
    attr(r, "warning") <- attr(x, "warning")
    class(r) <- unique(c("dataquieR_result", class(r)))
  }
  r
}

#' Extract elements of a `dataquieR` Result Object
#'
#' @param x the `dataquieR` result object
#'
#' @param ... arguments passed to the implementation for lists.
#'
#' @return the element of the `dataquieR` result object with all messages
#'         still attached
#'
#' @seealso  [base::Extract]
#' @export
#'
#' @keywords internal
`$.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  if (!is.null(r) && !inherits(x, "ggplot")) {
    attr(r, "error") <- attr(x, "error")
    attr(r, "message") <- attr(x, "message")
    attr(r, "warning") <- attr(x, "warning")
    class(r) <- unique(c("dataquieR_result", class(r)))
  }
  r
}
