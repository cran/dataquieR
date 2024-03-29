user_hints <- new.env(parent = emptyenv())
user_hints$l <- list()
#' Save a hint to the user during package load
#'
#' @param x [character] the hint
#'
#' @return `invisible(NULL)`
#'
#' @family system_functions
#' @concept process
#' @keywords internal
util_user_hint <- function(x) {
  util_expect_scalar(x, check_type = is.character)
  user_hints$l <-
    c(user_hints$l, x)
}
