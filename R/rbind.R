#' Combine `ReportSummaryTable` outputs
#'
#' @param ... `ReportSummaryTable` objects to combine.
#'
#' @description
#' Using this `rbind` implementation, you can combine different
#' heatmap-like results of the class `ReportSummaryTable`.
#'
#' @seealso [base::rbind.data.frame]
#'
#' @export
#'
rbind.ReportSummaryTable <- function(...) {
  a <- list(...)
  if (!all(vapply(a, is.data.frame, FUN.VALUE = logical(1)))) {
    util_error("Can only bind ReportSummaryTables")
  }
  if (!all(vapply(a, inherits, what = "ReportSummaryTable",
                  FUN.VALUE = logical(1)))) {
    util_error("Can only bind ReportSummaryTables")
  }
  a <- a[!!vapply(a, nrow, FUN.VALUE = integer(1))]
  a <- a[!!vapply(a, ncol, FUN.VALUE = integer(1))]
  if (length(a) <= 2) {
    if (length(a) == 0) {
      x <- rbind.data.frame(
        deparse.level = 1,
        make.row.names = TRUE,
        stringsAsFactors = FALSE,
        factor.exclude = TRUE
      )
      class(x) <- union("ReportSummaryTable", class(x))
      return(x)
    } else if (length(a) == 1) {
      x <- a[[1]]
      y <- data.frame(Variables = character(0), N = character(0))
      class(y) <- union("ReportSummaryTable", class(y))
    } else if (length(a) == 2) {
      x <- a[[1]]
      y <- a[[2]]
    }

    if (nrow(x) == 0) {
      return(y)
    }

    if (nrow(y) == 0) {
      return(x)
    }

    cols <- union(colnames(x), colnames(y))
    x[setdiff(cols, colnames(x))] <- numeric(nrow(x))
    y[setdiff(cols, colnames(y))] <- numeric(nrow(y))
    r <- rbind.data.frame(x[, cols, drop = FALSE], y[, cols, drop = FALSE],
                          deparse.level = 1,
                          make.row.names = TRUE,
                          stringsAsFactors = FALSE,
                          factor.exclude = TRUE
    )
    class(r) <- union("ReportSummaryTable", class(r))
    attr(r, "higher_means") <- attr(x, "higher_means")
    attr(r, "continuous") <- attr(x, "continuous")
    attr(r, "colcode") <- attr(x, "colcode")
    attr(r, "level_names") <- attr(x, "level_names")
    r
  } else {
    x <-
      do.call(Recall,
            c(list(
              a[[1]],
              a[[2]]
            ),
            list(
              # deparse.level = 1,
              # make.row.names = TRUE,
              # stringsAsFactors = FALSE,
              # factor.exclude = TRUE
            ))
    )
    y <- a[3:length(a)]
    do.call(Recall,
            c(list(
              x
            ),
            y,
            list(
                # deparse.level = 1,
                # make.row.names = TRUE,
                # stringsAsFactors = FALSE,
                # factor.exclude = TRUE
              ))
    )
  }
}
