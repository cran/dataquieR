#' Add labels to `ggplot`
#'
#' EXPERIMENTAL
#'
#' @param ... EXPERIMENTAL
#' @param meta_data the metadata
#' @param label_col the label columns
#'
#' @return a modified `ggplot`
#'
#' @examples
#' \dontrun{
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' ggplot(study_data, aes(x = v00013, y = v00004)) + geom_point() +
#'   util_gg_var_label()
#' m <- acc_margins(study_data = study_data, meta_data = meta_data,
#'   resp_vars = "v00004",
#'   group_vars = "v00012", label_col = VAR_NAMES)
#' m$SummaryPlot + util_gg_var_label()
#' l <- acc_loess(study_data = study_data, meta_data = meta_data,
#'   time_vars = "v00013",
#'   resp_vars = "v00004",
#'   group_vars = "v00012", label_col = VAR_NAMES)
#' l$SummaryPlotList$v00004 + util_gg_var_label()
#'
#' d <- acc_distributions("v00004", study_data = study_data,
#'   meta_data = meta_data,
#'   label_col = VAR_NAMES)
#'
#' d$SummaryPlotList$v00004 + util_gg_var_label()
#' }
util_gg_var_label <- function(...,
                              meta_data = get("meta_data", parent.frame()),
                              label_col = get("label_col", parent.frame())) { # nocov start

  lp <- ggplot2::last_plot()

  p <- ggplot2::ggplot_build(lp)

  l <- list(...)

  if ("labels" %in% names(lp)) {
    yy <- unlist(lp$labels[trimws(lp$labels) != ""],
                 recursive = FALSE)
    xx <- prep_map_labels(yy,
                          meta_data = meta_data, to = label_col,
                          ifnotfound = NA_character_)
    xx <- xx[!is.na(xx)]
    zz <- setNames(xx[yy], nm = names(yy))
    l[names(zz)] <- zz
  }


  for (n in names(p$plot$mapping)) {
    r <- try({
      rlang::quo_name(p$plot$mapping[[n]])
      }, silent = TRUE
    )
    if (!inherits(r, "try-error") &&
        length(r) == 1 && is.character(r) && r != "<unknown>") {
      l[n] <- r
    }
  }

  l <- l[!is.na(l)]

  l[] <- prep_map_labels(unlist(l, recursive = FALSE),
                  meta_data = meta_data, to = label_col,
                  ifnotfound = setNames(nm = unlist(l, recursive = FALSE)))

  do.call(ggplot2::labs, l)
} # nocov end
