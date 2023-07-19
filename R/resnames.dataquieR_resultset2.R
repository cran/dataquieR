#' @inherit resnames
#'
#' @export
# importFrom dataquieR resnames
resnames.dataquieR_resultset2 <- function(x) {
  sort(unique(unname(unlist(lapply(x, names)))))
}
