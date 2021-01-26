#' Check for one value only
#'
#' utility function to identify variables with one value only.
#'
#' @param x vector with values
#'
#' @return logical(1): TRUE, if -- except NA -- exactly only one value
#'                           is observed in `x`,
#'                     FALSE otherwise
#'
util_check_one_unique_value <- function(x) {
  howmany <- length(unique(x[!is.na(x)]))
  return(as.logical(ifelse(howmany == 1, 1, 0)))
}
