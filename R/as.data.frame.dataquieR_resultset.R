#' Convert a full `dataquieR` report to a `data.frame`
#'
#' @description
#' converts a [dataquieR report][dq_report] to a [data.frame]. Intended for
#'  use in pipelines.
#'
#' @param x [dataquieR report][dq_report]
#' @param ... not used
#'
#' @return a [data.frame] with one row per indicator call, one column
#' `implementationform` naming the called indicator function, one column per
#' function argument and one additional column containing the results of each
#' call as a list.
#'
#' @export
as.data.frame.dataquieR_resultset <- function(x, ...) {
  .fill_up_dataframe <- function(fctn, cols, dfs) {
    df0 <- dfs[[fctn]]
    if (identical(df0, list())) {
      return(NULL)
    }
    df0$implementationform <- fctn
    missing_cols <- cols[!cols %in% colnames(df0)]
    cbind(df0, matrix(
      data = NA, nrow = nrow(df0), ncol = length(missing_cols),
      dimnames = list(NULL, missing_cols)))[,
                                            c("implementationform", cols),
                                            FALSE]
  }
  .c <- unique(unlist(lapply(x$long_format, colnames), recursive = TRUE))
  do.call(rbind.data.frame, lapply(names(x$long_format), .fill_up_dataframe,
                                   cols = .c, dfs = x$long_format))
}
