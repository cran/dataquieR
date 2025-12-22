#' Escape characters for HTML in a data frame
#'
#' @param x [data.frame] to be escaped
#'
#' @return [data.frame] with html escaped content
#' @noRd
util_df_escape <- function(x) {
  if (identical(attr(x, "is_html_escaped"), TRUE)) {
    return(x)
  }
  util_ensure_suggested("htmltools", goal = "for HTML escaping data frames")
  util_expect_data_frame(x)
  x[] <- lapply(x, function(y) {
    r <- htmltools::htmlEscape(y)
    attr(r, "plain_label") <- attr(y, "plain_label")
    r
  })
  x
}
