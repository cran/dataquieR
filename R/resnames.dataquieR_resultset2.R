#' @inherit resnames
#'
#' @export
# importFrom dataquieR resnames
resnames.dataquieR_resultset2 <- function(x) {
  dimnames(x)[[3]]
}
