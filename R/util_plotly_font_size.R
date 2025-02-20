#' @seealso [print.ReportSummaryTableo]
#' @family figure_functions
#' @concept figure
#' @keywords internal

util_plotly_font_size <- function(n_labels, space = 200,
                                  max_font_size = 16, min_font_size = 12) {
  if (n_labels > 15 || min_font_size > max_font_size || n_labels == 0 ||
      n_labels >= space || space == 0) {
    return(10)
  } else if (n_labels > 0 && n_labels < 16){
    return(12)
  } else {
    res <- min_font_size + (log(space/n_labels) - log(space/15)) *
      (max_font_size - min_font_size) / (log(space) - log(space/15))
    return(round(res))
  }
}
