#' Filter collection based on its `names()` using regular expressions
#'
#' @param collection a named collection (list, vector, ...)
#' @param regexps [character] a vector of regular expressions
#'
#' @return `collection` reduced to entries, that's names match at least any
#'         expression from `regexps`
#' @examples
#' \dontrun{ # internal function
#' util_filter_names_by_regexps(iris, c("epa", "eta"))
#' }
util_filter_names_by_regexps <- function(collection, regexps) {

  if (is.null(names(collection))) {
    util_error("Need names in argument %s", sQuote("collection"))
  }

  util_expect_scalar(regexps,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  if (length(regexps) == 0 || length(collection) == 0) {
    return(collection)
  }

  matches_mat <-
    matrix(vapply(
      regexps,
      grepl,
      names(collection),
      perl = TRUE,
      FUN.VALUE =
        logical(length(collection))
    ), nrow = length(collection), ncol = length(regexps))

  if (is.null(dim(matches_mat))) {
    util_stop_if_not(length(regexps) == 1 && length(collection) == 1)
    r <- collection[matches_mat]
    nm <- names(collection[matches_mat])
  } else {
    r <- collection[rowSums(matches_mat) > 0]
    nm <- names(collection[rowSums(matches_mat) > 0])
  }

  mostattributes(r) <- attributes(collection)
  names(r) <- nm

  r
}
