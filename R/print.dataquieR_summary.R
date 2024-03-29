#' Print a `dataquieR` summary
#'
#' @param x the `dataquieR` summary, see [summary()] and [dq_report2()]
#' @param ... not yet used
#' @param dont_print suppress the actual printing, just return a printable
#'                   object derived from `x`
#' @param grouped_by define the columns of the resulting matrix. It can be either
#' "call_names", one column per function,  or "indicator_metric", one column per indicator
#' or both c("call_names", "indicator_metric"). The last combination is the default
#' @return invisible html object
#' @export
#'
print.dataquieR_summary <- function(x, ..., grouped_by =
                                      c("call_names",
                                        "indicator_metric"),
                                    dont_print = FALSE) {
  util_ensure_suggested(pkg = c("htmltools",
                                "digest", "DT", "rmarkdown",
                                "markdown"),
                        goal = "generate plain HTML-summaries.")

  #check and normalize grouped_by
  util_expect_scalar(grouped_by, allow_more_than_one = TRUE,
                     min_length = 1, check_type = is.character,
                     error_message = "not allowed value for grouped_by")
  grouped_by <- util_match_arg(grouped_by, several_ok = TRUE)
  grouped_by <- unique(grouped_by)

  y <- util_render_table_dataquieR_summary(x, grouped_by = grouped_by)

  if (!dont_print) {
    print(y)
  } else {
    invisible(y)
  }
}
