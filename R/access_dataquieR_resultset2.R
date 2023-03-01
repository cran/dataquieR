#' Get a subset of a `dataquieR` `dq_report2` report
#'
#' @param x the report
#'
#' @param row the variable names, must be unique
#' @param col the function-call-names, must be unique
#' @param res the result slot, must be unique
#' @param drop drop, if length is 1
#'
#' @return a list with results, depending on `drop` and the number of results,
#'         the list may contain all requested results in sub-lists. The order
#'         of the results follows the order of the row/column/result-names given
#'
#' @export
`[.dataquieR_resultset2` <- function(x, row, col, res, drop = FALSE) {
  util_stop_if_not(inherits(x, "dataquieR_resultset2"))

  cn <- attr(x, "cn")
  rn <- attr(x, "rn")

  if (missing(col)) {
    col_matches <- rep(TRUE, length(attr(x, "names")))
  } else {
    util_stop_if_not(!any(duplicated(col)))
    col_matches <- cn %in% col
  }

  if (missing(row)) {
    row_matches <- rep(TRUE, length(attr(x, "names")))
  } else {
    util_stop_if_not(!any(duplicated(row)))
    # row_matches <- rn %in% c(row, "[ALL]") # TODO: Ensure, that only one match is found (int_ should return [all] but nothing else, com_, eg, vv)
    row_matches <- rn %in% row
  }

  matches <- row_matches & col_matches

  r <- unclass(x)[matches]
  rcn <- cn[matches]
  rrn <- rn[matches]
  if (!missing(col)) {
    first_order <- ordered(rcn, col)
  } else {
    first_order <- seq_along(rcn)
  }
  if (!missing(row)) {
    second_order <- ordered(rrn, row)
  } else {
    second_order <- seq_along(rrn)
  }
  r <- r[order(first_order, second_order)]

  if (!missing(res) && length(res)) {
    util_stop_if_not(!any(duplicated(res)))
    errors <- lapply(r, attr, "error")
    errors <- vapply(errors, length, FUN.VALUE = integer(1)) > 0
    r[!errors] <- lapply(r[!errors], `[`, res, drop = drop)
    if (drop) r[!errors] <- lapply(r[!errors], `[[`, 1)
    if (drop && length(r) == 1) { r <- r[[1]] }
  }

  if (length(r) == 1 && drop) {
    r <- r[[1]]
  }

  return(r)

}
