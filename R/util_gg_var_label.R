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
#' @noRd
util_gg_var_label <- function(...,
                              meta_data = get("meta_data", parent.frame()),
                              label_col = get("label_col", parent.frame())) { # nocov start

  lp <- ggplot2::last_plot()

  # ggplot_build kann (je nach Version) S7-Objekte liefern, daher später util_gg_get
  p <- ggplot2::ggplot_build(lp)

  l <- list(...)

  # ----- Labels aus dem Plot holen -------------------------------------------
  labels_obj <- util_gg_get(lp, "labels")

  if (!is.null(labels_obj)) {
    yy <- unlist(labels_obj[trimws(labels_obj) != ""],
                 recursive = FALSE)
    xx <- prep_map_labels(
      yy,
      meta_data   = meta_data,
      to          = label_col,
      ifnotfound  = NA_character_
    )
    xx <- xx[!is.na(xx)]
    zz <- setNames(xx[yy], nm = names(yy))
    l[names(zz)] <- zz
  }

  # ----- Mappings aus dem ggplot_build-Objekt holen --------------------------
  plot_obj <- util_gg_get(p, "plot")
  mapping  <- if (!is.null(plot_obj)) util_gg_get(plot_obj, "mapping") else NULL

  if (!is.null(mapping)) {
    for (n in names(mapping)) {
      r <- try({
        rlang::quo_name(mapping[[n]])
      }, silent = TRUE)

      if (!inherits(r, "try-error") &&
          length(r) == 1 &&
          is.character(r) &&
          r != "<unknown>") {
        l[n] <- r
      }
    }
  }

  # ----- Labels mappen und labs() anwenden -----------------------------------
  l <- l[!is.na(l)]

  l[] <- prep_map_labels(
    unlist(l, recursive = FALSE),
    meta_data   = meta_data,
    to          = label_col,
    ifnotfound  = setNames(nm = unlist(l, recursive = FALSE))
  )

  do.call(ggplot2::labs, l)
} # nocov end
