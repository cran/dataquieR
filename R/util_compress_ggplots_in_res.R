################################################################################
#' Remove specific classes from a ggplot `plot_env` environment
#'
#' Useful to remove large objects before writing to disk with `qs` or `rds`.
#' Also deletes parent environment of the plot environment.
#' Also deletes unneeded variables
#'
#' @param r the object
#'
#' @seealso [HERE](https://github.com/tidyverse/ggplot2/issues/3619#issuecomment-628021555)
#'
#' @keywords internal
util_compress_ggplots_in_res <- function(r) {
  if (isTRUE(attr(r, "from_ReportSummaryTable"))) {
    return(NULL) # never store plots of reportsummarytables, because the original objects are already in the report
  }
  if (ggplot2::is.ggplot(r)) {
    r$plot_env <- emptyenv()
    # https://stackoverflow.com/questions/75698707/how-to-extract-variable-names-from-aes-mapping-in-r/75699079#75699079
    #mv <- unique(unlist(lapply(r$mapping, all.vars))) # does not work for quosures .data[["variable_name"]] - gives '.data' instead of 'variable_name'
    mv <- unique(unlist(lapply(r$mapping, function(ll) {
      quo_ll_map <- rlang::quo_get_expr(ll)
      if (".data" %in% as.character(quo_ll_map)) {
        colnames(r$data)[which(colnames(r$data) %in% as.character(quo_ll_map))]
      } else {
        all.vars(ll)
      }
    })))
    mv <- unique(c(mv,
                   unlist(lapply(r$layers,
                                 function(lly)  {
                                   lapply(lly$mapping, function(ll) {
                                     quo_ll_map <- NULL
                                     try(quo_ll_map <- rlang::quo_get_expr(ll),
                                         silent = TRUE)
                                     if (".data" %in% as.character(quo_ll_map)) { #TODO: also handle the data in the geoms layers
                                       colnames(r$data)[which(colnames(r$data) %in%
                                                                as.character(quo_ll_map))]
                                     } else {
                                       all.vars(ll)
                                     }
                                   })
                                 }))))
    mv <- c(mv, "facet") # keep column facet as used in util_margins_nom

    if ("facet" %in% names(r)) {
      facet_v <- NULL
      try({
        facet_v <- r$facet$vars()
      }, silent = TRUE)
      if (is.character(facet_v) && length(facet_v) > 0) {
        mv <- c(mv, facet_v)
      }
    }


    r$data <- r$data[, intersect(colnames(r$data), mv), drop = FALSE]
  } else if (is.list(r)) {
    r[] <- lapply(r, util_compress_ggplots_in_res)
    r[vapply(r, is.null, logical(1))] <- NULL
  }
  return(r)
}
