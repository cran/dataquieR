#' Test, if values of x are empty, i.e. NA or whitespace characters
#' @param x the vector to test
#' @return a logical vector, same length as x; TRUE, if resp. element in x is
#'         "empty"
#'
#' @family robustness_functions
#' @concept process
#' @keywords internal
util_empty <- function(x) {
  xx <- "not_empty"
  if (util_is_try_error(try(xx <- trimws(x), silent = TRUE))) {
    try(xx <- vapply(x,
                     function(y) {
                       z <- try(r <- trimws(y), silent = TRUE)
                       if (util_is_try_error(z)) {
                         r <- y
                       }
                       r
                      },
                     FUN.VALUE = character(1)), silent = TRUE)
  }
  unname(is.na(x) | xx == "")
}
