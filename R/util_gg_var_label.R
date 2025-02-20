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
#' @concept process
#' @keywords internal
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
