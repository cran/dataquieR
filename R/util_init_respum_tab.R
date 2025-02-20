#' Extract all properties of a `ReportSummaryTable`
#'
#' @param x `ReportSummaryTable` object
#'
#' @return [list] with all properties
#'
#' @keywords internal
util_init_respum_tab <- function(x) {
  my_cols <- c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
               "#fdbb84", "#fdd49e", "#fee8c8", "#2166AC")

  higher_means <- attr(x, "higher_means")
  if (is.null(higher_means)) higher_means <- "worse"
  continuous <- attr(x, "continuous")
  if (is.null(continuous)) continuous <- TRUE
  colcode <- attr(x, "colcode")
  if (is.null(colcode)) {
    continuous <- TRUE
  }
  level_names <- attr(x, "level_names")

  relative <- attr(x, "relative")
  if (is.null(relative)) relative <- continuous
  colscale <- attr(x, "colscale")
  if (is.null(colscale)) {
    colscale <- my_cols
  }

  if (!is.null(attr(x, "flip_mode"))) {
    flip_mode <- attr(x, "flip_mode")
  }

  if (higher_means != "worse")
    colscale <- rev(colscale)

  as.list(environment())
}
