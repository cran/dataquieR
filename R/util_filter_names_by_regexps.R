#' Filter collection based on its `names()` using regular expressions
#'
#' @param collection a named collection (list, vector, ...)
#' @param regexps [character] a vector of regular expressions
#' @param negate [logical] Invert the meaning of the regular expressions, so
#'                         keep only entries, that do not match any of the
#'                         `regexps`.
#'
#' @return `collection` reduced to entries, that's names match at least any
#'         expression from `regexps`
#' @examples
#' \dontrun{ # internal function
#' util_filter_names_by_regexps(iris, c("epa", "eta"))
#' }
#'
#' @family string_functions
#' @concept process
#' @noRd
util_filter_names_by_regexps <- function(collection,
                                         regexps,
                                         negate = FALSE) {

  if (is.null(names(collection))) {
    util_error("Need names in argument %s", sQuote("collection"))
  }

  util_expect_scalar(regexps,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  util_expect_scalar(negate, check_type = is.logical)

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

  if (is.null(dim(matches_mat))) { # TODO: is this ever executed?
    util_stop_if_not(length(regexps) == 1 && length(collection) == 1)
    if (length(regexps) > 1) { # TODO: ever executed?
      # only one long row of match?(T/F)-entries
      #             RE1 RE2 RE3 RE4 RE5
      # acc_margins T   F   F   T   T
      if (negate) {
        if (all(!as.vector(matches_mat), na.rm = TRUE)) {
          r <- collection
          nm <- names(collection)
        } else {
          r <- character(0)
          nm <- character(0)
        }
      } else {
        if (any(as.vector(matches_mat), na.rm = TRUE)) {
          r <- collection
          nm <- names(collection)
        } else {
          r <- character(0)
          nm <- character(0)
        }
      }
    } else {
      # only one long column of match?(T/F)-entries
      #               RE1
      # acc_margins   T
      # com_item_miss F
      # con_limit_dev F
      if (negate) {
        matches_mat <- ! matches_mat
      }
      r <- collection[matches_mat]
      nm <- names(collection[matches_mat])
    }
  } else {
    if (negate) {
      r <- collection[rowSums(matches_mat) == 0]
      nm <- names(collection[rowSums(matches_mat) == 0])
    } else {
      r <- collection[rowSums(matches_mat) > 0]
      nm <- names(collection[rowSums(matches_mat) > 0])
    }
  }

  mostattributes(r) <- attributes(collection)
  names(r) <- nm

  r
}
