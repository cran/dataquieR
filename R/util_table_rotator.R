#' Rotate 1-row data frames to key-value data frames
#'
#' if `nrow(tb) > 1`, `util_table_rotator` just returns `tb`.
#'
#' @param tb [data.frame] a data frame
#'
#' @return [data.frame] but transposed
util_table_rotator <- function(tb) {
  util_expect_data_frame(tb)
  if (nrow(tb) == 1) {
    data.frame(
      check.names = FALSE,
      fix.empty.names = FALSE,
      row.names = NULL,
      ` ` = colnames(tb),
      ` ` = unlist(tb[1, , drop = TRUE])
    )
  } else {
    tb
  }
}
