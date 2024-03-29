#' removes empty rows from `x`
#'
#' @param x [data.frame] a data frame to be cleaned
#' @param id_vars [character] column names, that will be treated as empty
#'
#' @return [data.frame] reduced `x`
#'
#' @family missing_functions
#' @concept data_management
#' @keywords internal
util_remove_empty_rows <- function(x, id_vars = character(0)) {
  util_expect_scalar(id_vars,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)
  util_expect_data_frame(x, id_vars)
  x[rowSums(!util_empty(as.matrix(x[, setdiff(colnames(x), id_vars)]))) != 0, ,
    FALSE]
}
