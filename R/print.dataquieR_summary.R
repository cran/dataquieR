#' Print a `dataquieR` summary
#'
#' @param x the `dataquieR` summary, see [summary()] and [dq_report2()]
#' @param ... not yet used
#' @param dont_print suppress the actual printing, just return a printable
#'                   object derived from `x`
#' @param grouped_by define the columns of the resulting matrix. It can be either
#' "call_names", one column per function,  or "indicator_metric", one column per indicator
#' or both c("call_names", "indicator_metric"). The last combination is the default
#' @param folder_of_report a named vector with the location of variable and call_names
#' @param vars_to_include `"study"`, `"ssi"` or `c("study", "ssi")`. variables to include
#' @return invisible html object
#' @export
#'
print.dataquieR_summary <- function(x, ..., grouped_by =
                                      c("call_names",
                                        "indicator_metric"),
                                    dont_print = FALSE,
                                    folder_of_report = NULL,
                                    vars_to_include = c("study")) {
  util_ensure_suggested(pkg = c("htmltools",
                                "DT", "rmarkdown",
                                "markdown"),
                        goal = "generate plain HTML-summaries.")
  util_expect_scalar(vars_to_include,
                     allow_more_than_one = TRUE,
                     min_length = 1,
                     max_length = 2,
                     check_type = is.character)
  util_match_arg(vars_to_include, c("study", "ssi"), several_ok = TRUE)

  #check and normalize grouped_by
  util_expect_scalar(grouped_by, allow_more_than_one = TRUE,
                     min_length = 1, check_type = is.character,
                     error_message = "not allowed value for grouped_by")
  grouped_by <- util_match_arg(grouped_by, several_ok = TRUE)
  grouped_by <- unique(grouped_by)

  x <- util_reclassify_dataquieR_summary(x)

  y <- util_render_table_dataquieR_summary(x, grouped_by = grouped_by,
                                           folder_of_report = folder_of_report,
                                           vars_to_include = vars_to_include)

  if (!dont_print) {
    print(y)
  } else {
    invisible(y)
  }
}
