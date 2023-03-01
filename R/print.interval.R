#' print implementation for the class `interval`
#'
#' such objects, for now, only occur in `RECCap` rules, so this function
#' is meant for internal use, mostly -- for now.
#'
#' @param x `interval` objects to print
#' @param ... not used yet
#'
#' @seealso base::print
#'
#' @return the printed object
#' @export
print.interval <- function(x, ...) {
  util_stop_if_not(inherits(x, "interval"))
  if (x$inc_l) { cat("[") } else { cat("(") }
  if (inherits(x$low, "POSIXct")) {
    cat(as.character(x$low, usetz = FALSE))
  } else {
    cat(as.character(x$low))
  }
  cat(";")
  if (inherits(x$upp, "POSIXct")) {
    cat(as.character(x$upp, usetz = FALSE))
  } else {
    cat(as.character(x$upp))
  }
  if (x$inc_u) { cat("]") } else { cat(")") }
  x
}
