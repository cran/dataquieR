#' Bind data frames row-based
#'
#' if not all data frames share  all columns, missing columns will be filled with
#' `NA`s.
#'
#' @param ... [data.frame] none more more data frames
#' @param data_frames_list [list] optional, a list of data frames
#'
#' @return [data.frame] all data frames appended
#'
#' @examples
#' \dontrun{
#' util_rbind(head(cars), head(iris))
#' util_rbind(head(cars), tail(cars))
#' util_rbind(head(cars)[, "dist", FALSE], tail(cars)[, "speed", FALSE])
#' }
#'
#' @family data_management
#' @concept process
#' @noRd
util_rbind <- function(..., data_frames_list = list()) {
  data_frames_list <- c(list(...), data_frames_list)
  data_frames_list <- data_frames_list[!vapply(data_frames_list, is.null,
                                              FUN.VALUE = logical(1))]
  data_frames_list <-
    lapply(data_frames_list, util_expect_data_frame, dont_assign = TRUE)

  all_cols <- unique(unlist(lapply(data_frames_list, colnames),
                            recursive = TRUE))

  data_frames_list <- lapply(data_frames_list, function(dfr) {
    if (nrow(dfr) > 0) {
      for (cl in setdiff(all_cols, colnames(dfr))) {
        dfr[[cl]] <- NA
      }
      dfr
    } else {
      NULL
    }
  })

  do.call(rbind.data.frame, data_frames_list)
}
