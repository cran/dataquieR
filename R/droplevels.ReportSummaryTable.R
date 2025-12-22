#' Remove unused levels from `ReportSummaryTable`
#'
#' @param x `ReportSummaryTable` an object from which to drop unused factor
#'          levels.
#' @param ... no used.
#'
#' @return `ReportSummaryTable` with all (`NA` or `0`)-columns removed
#' @export
droplevels.ReportSummaryTable <- function(x, ...) {
  util_stop_if_not(inherits(x, "ReportSummaryTable"))
  cols_to_change <- setdiff(colnames(x), c("Variables", "N"))
  we_can_be_dropped <- vapply(x[, cols_to_change], function(cl) {
    all(is.na(cl) | 0 == cl)
  }, FUN.VALUE = logical(1))
  x[, names(which(we_can_be_dropped))] <- NULL
  x
}
