#' return all matches of an expression
#'
#' @param data a character vector
#' @param pattern a character string containing a regular expression
#'
#' @return A list with matching elements or NULL (in case on non-matching elements)
#' @author Josh O'Brien
#' @seealso [Stack Overflow](https://stackoverflow.com/a/10215257)
#' @examples
#' \dontrun{ # not exported, so not tested
#' dat0 <- list("a sentence with citation (Ref. 12), (Ref. 13), and then (Ref. 14)",
#'   "another sentence without reference")
#' pat <- "Ref. (\\d+)"
#' util_extract_matches(dat0, pat)
#' }
#'
#' @family process_functions
#' @concept string
#' @keywords internal
util_extract_matches <- function(data, pattern) {
  util_expect_scalar(pattern)
  util_expect_scalar(data,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     allow_na = TRUE)
  .util_extract_matches <- function(dt) {
    start <-  gregexpr(pattern, dt)[[1]]
    stop  <-  start + attr(start, "match.length") - 1
    if(-1 %in% start) {
      NULL
    } else {
      mapply(substr, start, stop, MoreArgs = list(x = dt))
    }
  }
  lapply(data, .util_extract_matches)
}
