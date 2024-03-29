#' Tabulate a vector
#'
#' does the same as `as.data.frame(table(x))` but
#' guarantees a data frame with two columns is returned
#'
#' @param Var1 vector to ta tabulate
#'
#' @return a data frame with columns `Var1` and `Freq`
#'
#' @family data_management
#' @concept robustness
#' @keywords internal
util_table_of_vct <- function(Var1) {
  util_expect_scalar(Var1,
                     allow_more_than_one = TRUE,
                     allow_na = TRUE,
                     allow_null = TRUE)
  r <- as.data.frame(table(Var1))
  if (!identical(colnames(r), c("Var1", "Freq"))) {
    r <- data.frame(Var1 = integer(0), Freq = integer(0))
  }
  r
}
