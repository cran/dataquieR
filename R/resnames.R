#' Return names of result slots (e.g., 3rd dimension of dataquieR results)
#'
#' @param x the objects
#'
#' @return character vector with names
#' @export
resnames <- function(x) {
  UseMethod("resnames")
}
