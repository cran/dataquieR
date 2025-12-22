#' Rotate 1-row data frames to key-value data frames
#'
#' if `nrow(tb) > 1`, `util_table_rotator` just returns `tb`.
#'
#' @param tb [data.frame] a data frame
#'
#' @return [data.frame] but transposed
#'
#' @noRd
util_table_rotator <- function(tb) {  #TODO: should take care of the description attribute
  util_expect_data_frame(tb)
  if (nrow(tb) == 1) {
    is_html_escaped <- attr(tb, "is_html_escaped")
    tb[] <- lapply(tb, FUN = as.character)
    util_attach_attr(data.frame(
      check.names = FALSE,
      fix.empty.names = FALSE,
      row.names = NULL,
      ` ` = colnames(tb),
      ` ` = unlist(tb[1, , drop = TRUE])
    ), is_html_escaped = is_html_escaped)
  } else {
    tb
  }
}
