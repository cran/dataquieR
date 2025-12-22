#' Rename columns of a `SummaryTable` (or Segment, ...) to look nice
#'
#' @param Table [data.frame], a table
#'
#' @return renamed table
#'
#' @family reporting_functions
#' @concept summary
#' @noRd
util_make_data_slot_from_table_slot <- function(Table) {

  # 1) Select indicator metric columns via helper
  indicator_cols <- util_extract_indicator_metrics(Table)

  # 2) Choose base/technical columns (if present) and build final column order
  base_candidates <- c("Variables", "Segment", "DF_NAME", "CHECK_LABEL",
                       "Dataframe", "CONTRADICTION_TYPE")
  base_keep <- intersect(base_candidates, colnames(Table))

  select_names <- c(base_keep, colnames(indicator_cols))
  # Ensure unique, in case of overlaps
  select_names <- unique(select_names)

  # If nothing to select, return an empty DataSlot with preserved classes
  Data <- Table[, select_names, drop = FALSE]

  # 3) Prepare percent-column mask based on ORIGINAL names (before renaming)
  pct_mask <- startsWith(select_names, "PCT_")

  # 4) Translate headers with the helper (keep unknowns untouched)
  translated <- util_translate_indicator_metrics(
    select_names,
    short = FALSE,
    long  = TRUE,
    ignore_unknown = TRUE
  )

  # 5) Apply manual recodes for known technical columns
  manual_map <- c(
    Variables           = "Variables",
    Segment             = "Segment",
    DF_NAME             = "Dataframe",
    CHECK_LABEL         = "Check",
    Dataframe           = "Dataframe",
    CONTRADICTION_TYPE  = "Contradiction Type"
  )
  # Overwrite translated labels where we have explicit recodes
  idx_manual <- match(names(manual_map), select_names, nomatch = 0L)
  if (any(idx_manual > 0L)) {
    translated[idx_manual[idx_manual > 0L]] <-
      unname(manual_map[names(manual_map)[idx_manual > 0L]])
  }

  # 6) Rename columns
  colnames(Data) <- translated

  # 7) Format percentage columns by ORIGINAL-name mask (same positions)
  if (any(pct_mask)) {
    Data[, pct_mask] <- lapply(Data[, pct_mask, drop = FALSE], function(cl) {
      if (length(cl) == 0 || all(util_empty(cl))) {
        NA_character_
      } else {
        util_paste0_with_na(round(cl, 2), "%")
      }
    })
  }

  # 8) Adjust class tags
  if (inherits(Data, "TableSlot")) {
    class(Data) <- setdiff(class(Data), "TableSlot")
  }
  if (!inherits(Data, "DataSlot")) {
    class(Data) <- union("DataSlot", class(Data))
  }

  Data
}
