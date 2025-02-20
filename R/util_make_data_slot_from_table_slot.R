#' Rename columns of a `SummaryTable` (or Segment, ...) to look nice
#'
#' @param Table [data.frame], a table
#'
#' @return renamed table
#'
#' @family reporting_functions
#' @concept summary
#' @keywords internal
util_make_data_slot_from_table_slot <- function(Table) { # TODO: Use also in both qualified missingness functions
  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics") # TODO: Use util_translate_indicator_metrics
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
  cols_for_output <- c(Variables = "Variables",
                       Segment = "Segment",
                       DF_NAME = "Dataframe",
                       CHECK_LABEL = "Check",
                       Dataframe = "Dataframe",
                       CONTRADICTION_TYPE = "Contradiction Type",
                       cols_for_output)
  cols_for_output <- cols_for_output[!is.na(cols_for_output)]
  cols_for_output <- cols_for_output[names(cols_for_output) %in%
                                       colnames(Table)]
  Data <- Table[, names(cols_for_output), FALSE]
  Data[, startsWith(names(cols_for_output), "PCT_")] <-
    Data[, startsWith(names(cols_for_output), "PCT_")]
  colnames(Data) <- cols_for_output
  Data[, startsWith(names(cols_for_output), "PCT_")] <-
    lapply(Data[, startsWith(names(cols_for_output), "PCT_"), FALSE],
           function(cl) {
             if (length(cl) == 0 ||
                 all(util_empty(cl))) {
               NA_character_
             } else {
               util_paste0_with_na(round(cl, 2), "%")
             }
           })
  if (inherits(Data, "TableSlot")) {
    class(Data) <- setdiff(class(Data), "TableSlot")
  }
  if (!inherits(Data, "DataSlot")) {
    class(Data) <- union("DataSlot", class(Data))
  }
  Data
}
