#' Detect falsish values
#'
#' @param x a value/vector of values
#'
#' @return vector of logical values:
#'    `TRUE`, wherever x is somehow empty
util_is_na_0_empty_or_false <- function(x) { # TODO: generic?
  y <- x
  class(y) <- "logical"
  attributes(y) <- NULL
  y[] <- FALSE
  y[is.na(x)] <- TRUE
  y[!(trimws(x) %in% c("true", "TRUE", "T", "t", "+", ""))] <- TRUE
  idx <- !!suppressWarnings(as.numeric(x))
  idx[is.na(idx)] <- FALSE
  y[idx] <- FALSE
  y <- as.logical(y)
  return(y)
}

# xxx <- function(x) {
#   vapply(x, FUN.VALUE = logical(1), FUN = function(y) {
#     if (is.na(y)) {
#       return(TRUE)
#     }
#     if (is.character(y) && trimws(y) %in% c("true", "TRUE", "T", "t", "+")) {
#       return(TRUE)
#     }
#     ny <- suppressWarnings(as.numeric(y))
#     if (!is.na(ny)) {
#       return(!ny)
#     }
#     return(FALSE)
#   })
#
# }
