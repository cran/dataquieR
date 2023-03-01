#' Generate a report summary table
#'
#' @param object a square result set
#' @param aspect an aspect/problem category of results
#' @param FUN function to apply to the cells of the result table
#' @param ... not used
#' @param collapse passed to `FUN`
#'
#' @return a summary of a `Square2` report
#' @export
#' @examples
#' \dontrun{
#'   util_html_table(summary(report, aspect = "error", FUN = util_get_html_cell_for_result),
#'        filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
#'        is_matrix_table = TRUE, rotate_headers = TRUE, output_format = "HTML"
#'   )
#' }
summary.dataquieR_resultset2 <- function(object, aspect = c("applicability", "error", "issue"), # , "anamat"),
                                   FUN = util_get_html_cell_for_result,
                                   collapse = "\n<br />\n",
                                   ...) {
  util_ensure_suggested("htmltools", "Generating nice tables")
  util_stop_if_not(inherits(object, "dataquieR_resultset2"))
  aspect <- util_match_arg(aspect, several_ok = FALSE)
  do.call(rbind, lapply(setNames(nm = rownames(object)),
         FUN = function(rn) {
          vapply(setNames(nm = colnames(object)),
               FUN.VALUE = character(1),
               FUN = function(cn, aspect, collapse) {
                 r <- FUN(object[rn, cn, drop = TRUE],
                     aspect = aspect,
                     collapse = collapse,
                     rn = rn,
                     cn = cn)
                 # if (!is.character(r) || length(r) != 1)
                 #   browser()
                 r
               },
               aspect = aspect,
               collapse = collapse)
        }
  )) # TOOD: "Any-Issue" Column
}
