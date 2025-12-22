#' Get duplicates including first instance
#'
#' @param x object to pass do [base::duplicated()] as `x`
#' @param ... passed to [base::duplicated()] as `...`
#'
#' @returns see [base::duplicated()]
#' @noRd
#' @seealso [`StackOverflow`](https://stackoverflow.com/a/7854620)
#' @author [`Joshua Ulrich`](https://stackoverflow.com/users/271616/joshua-ulrich)
util_duplicated_inclding_first <- function(x, ...) {
  if (is.vector(x) || is.data.frame(x)) {
    return(duplicated(x, ...) | duplicated(x, fromLast=TRUE, ...))
  } else {
    util_error("Object of unsupported class %s passed",
               sQuote(paste(class(x), collapse = ", ")))
  }
}
