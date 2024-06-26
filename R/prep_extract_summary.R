#' Extract summary from data quality results
#'
#' Generic function, currently supports [dq_report2] and [dataquieR_result]
#'
#' @param r [dq_report2] or [dataquieR_result] object
#' @param ... further arguments, maybe needed for some implementations
#'
#' @export
#' @return [list] with two slots `Data` and `Table` with [data.frame]s
#'                featuring all metrics columns
#'                from the report or result in `x`,
#'                the [STUDY_SEGMENT] and the [VAR_NAMES].
#'                In case of `Data`, the columns are formatted nicely but still
#'                with the standardized column names -- use
#'                [util_translate_indicator_metrics()] to rename them nicely. In
#'                case of `Table`, just as they are.
#' @family summary_functions
prep_extract_summary <- function(r,
                                 ...) {
  UseMethod("prep_extract_summary")
}
