#' @inherit `[`
#' @export
`[.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  attr(r, "error") <- attr(x, "error")
  attr(r, "message") <- attr(x, "message")
  attr(r, "warning") <- attr(x, "warning")
  class(r) <- unique(c("dataquieR_result", class(r)))
  r
}

#' @inherit `[[`
#' @export
`[[.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  if (!is.null(r)) {
    attr(r, "error") <- attr(x, "error")
    attr(r, "message") <- attr(x, "message")
    attr(r, "warning") <- attr(x, "warning")
    class(r) <- unique(c("dataquieR_result", class(r)))
  }
  r
}

#' @inherit `$`
#' @export
`$.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  if (!is.null(r)) {
    attr(r, "error") <- attr(x, "error")
    attr(r, "message") <- attr(x, "message")
    attr(r, "warning") <- attr(x, "warning")
    class(r) <- unique(c("dataquieR_result", class(r)))
  }
  r
}
