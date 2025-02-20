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
  attr(r, "as_plotly") <- attr(x, "as_plotly")
  attr(r, "dont_util_adjust_geom_text_for_plotly") <- attr(x, "dont_util_adjust_geom_text_for_plotly")
  attr(r, "function_name") <- attr(x, "function_name")
  attr(r, "cn") <- attr(x, "cn")
  attr(r, "call") <- attr(x, "call")
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
  if (is.null(r)) {
    r <- list()
    class(r) <- union("dataquieR_NULL", class(r))
  }
  if (!inherits(x, "ggplot")) {
    attr(r, "error") <- attr(x, "error")
    attr(r, "message") <- attr(x, "message")
    attr(r, "warning") <- attr(x, "warning")
    # do not assign this class, here:
    # class(r) <- unique(c("dataquieR_result", class(r)))
    class(r) <- unique(c("Slot", class(r)))
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
`$.dataquieR_result` <- `[[.dataquieR_result`
