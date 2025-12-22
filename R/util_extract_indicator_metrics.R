#' Extract columns of a `SummaryTable` (or Segment, ...)
#'
#' @param Table [data.frame], a table
#'
#' @family summary_functions
#' @noRd
#'
#' @return [data.frame] columns with indicator metrics from `Table`
util_extract_indicator_metrics <- function(Table) {
  # Table <- data.frame(PCT_com_qum_nonresp = 1:10, xxxxx = letters[1:10])
  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics")
  dqi <- util_get_concept_info("dqi")
  cols_for_output <-
    vapply(colnames(Table), FUN.VALUE = character(1), FUN = function(x) {
      util_stop_if_not(length(x) == 1)
      nm <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(nm) >= 2) {
        m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
                         "Metrics", drop = TRUE), 1)
        d <- head(subset(dqi, get("abbreviation") == paste(tail(nm, -1),
                                                           collapse = "_"),
                         "Name", drop = TRUE), 1)
        if (length(m) == length(d) && length(d) == 1 &&
            !util_empty(m) && !util_empty(d)) {
          sprintf("%s (%s)", d, m)
        } else {
          NA_character_
        }
      } else {
        NA_character_
      }
    })
  Table[names(cols_for_output[!is.na(cols_for_output)])]
}
