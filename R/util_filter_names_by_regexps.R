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

  if (length(regexps) == 0) {
    return(collection)
  }

  matches_mat <-
    vapply(
      regexps,
      grepl,
      names(collection),
      perl = TRUE,
      FUN.VALUE =
        logical(length(collection))
    )

  collection[rowSums(matches_mat) > 0]

}
