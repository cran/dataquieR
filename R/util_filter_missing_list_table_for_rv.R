#' Filter a `MISSING_LIST_TABLE` for rows matching the variable `rv`
#'
#' In `MISSING_LIST_TABLE`, a column `resp_vars` may be specified. If so,
#' and if, for a row, this column is not empty, then that row only affects the
#' one variable specified in that cell
#'
#' @param table [cause_label_df] a data frame with missing codes and
#'                               optionally `resp_vars`. It also comprises
#'                               labels and optionally an interpretation
#'                               column with `AAPOR` codes. Must already cover
#'                               the variable `rv`, i.e., item level metadata
#'                               is not checked to find the suitable missing
#'                               table for `rv`.
#'
#' @param rv [variable] the response variable to filter the missing list for
#'                      specified by a label.
#' @param rv2 [variable] the response variable to filter the missing list for
#'                       specified by a `VAR_NAMES`-name.
#'
#' @return [data.frame] the row-wise bound data frames as one data frame
#'
#' @family missing_functions
#' @concept metadata_management
#' @noRd
util_filter_missing_list_table_for_rv <- function(table, rv, rv2 = rv) {
  util_expect_scalar(rv, check_type = is.character)
  util_expect_scalar(rv2, check_type = is.character)
  util_expect_data_frame(table)
  if (!("resp_vars" %in% colnames(table))) { # no filter column, all rows match
    return(table)
  }
  unspecific <- table[util_empty(table[["resp_vars"]]), ,
                      FALSE] # rows w/o resp_vars restriction
  specific <- table[!util_empty(table[["resp_vars"]]) &
                      trimws(table[["resp_vars"]]) %in% c(rv, rv2), ,
                    FALSE]  # rows restricted by resp_vars on rv
  rbind(specific, unspecific) # combine both
}
