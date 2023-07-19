#' Generate a report summary table
#'
#' @param object a square result set
#' @param aspect an aspect/problem category of results
#' @param FUN function to apply to the cells of the result table
#' @param ... not used
#' @param collapse passed to `FUN`
#'
#' @return a summary of a `dataquieR` report
#' @export
#' @examples
#' \dontrun{
#'   util_html_table(summary(report, aspect = "error", FUN = util_get_html_cell_for_result),
#'        filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
#'        is_matrix_table = TRUE, rotate_headers = TRUE, output_format = "HTML"
#'   )
#' }
summary.dataquieR_resultset2 <- function(object, aspect = c("applicability", "error", "issue", "anamat", "indicator_or_descriptor"),
                                   FUN = util_get_html_cell_for_result,
                                   collapse = "\n<br />\n",
                                   ...) {
  f <- substitute(FUN)
  FUN <- force(eval(f, enclos = parent.frame(), envir = environment()))
  util_ensure_suggested("htmltools", "Generating nice tables")
  util_stop_if_not(inherits(object, "dataquieR_resultset2"))
  aspect <- util_match_arg(aspect, several_ok = FALSE)

  rn_obj <- rownames(object)
  cn_obj <- colnames(object)

  rn_to_use <- vapply(rn_obj, function(rn) {
    any(vapply(object[rn, , drop = TRUE],
           inherits,
           "dataquieR_result",
           FUN.VALUE = logical(1)))
  }, FUN.VALUE = logical(1))

  cn_to_use <-
    vapply(cn_obj, function(cn) {
      # TODO: check entity attribute instead and omit outputs with entity != item
    x <- object[, cn, drop = FALSE]
      length(x) > 0 &&
      any(!endsWith(names(x), ".[ALL]")) &&
      all(vapply(names(x), function(listname) {
        any(endsWith(listname, paste0(".", rownames(object))))
      }, FUN.VALUE = logical(1)))
  }, FUN.VALUE = logical(1))

  rn_obj <- rn_obj[rn_to_use]
  cn_obj <- cn_obj[cn_to_use]

  do.call(rbind, lapply(setNames(nm = rn_obj),
         FUN = function(rn) {
          vapply(setNames(nm = cn_obj),
               FUN.VALUE = character(1),
               FUN = function(cn, aspect, collapse) {
                 r <- FUN(object[rn, cn, drop = TRUE],
                     aspect = aspect,
                     collapse = collapse,
                     rn = rn,
                     cn = cn)
                 # if (inherits(r, "try-error")) {
                 #   debug(FUN)
                 #   r <- FUN(object[rn, cn, drop = TRUE],
                 #            aspect = aspect,
                 #            collapse = collapse,
                 #            rn = rn,
                 #            cn = cn)
                 # }
                 # if (!is.character(r) || length(r) != 1)
                 #   browser()
                 r
               },
               aspect = aspect,
               collapse = collapse)
        }
  )) # TOOD: "Any-Issue" Column
}
