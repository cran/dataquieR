#' Names of a `dataquieR` report object (v2.0)
#'
#' @param x the result object
#'
#' @return the names
#'
#' @method dimnames dataquieR_resultset2
#' @export
dimnames.dataquieR_resultset2 <- function(x) {
  matrix_list <- attr(x, "matrix_list")
  row_indices <- attr(matrix_list, "row_indices")
  col_indices <- attr(matrix_list, "col_indices")
  list(names(sort(row_indices)), # TODO: sort also in Square2's access function
       names(sort(col_indices)), # TODO: sort also in Square2's access function
       resnames(x)
       )
}
