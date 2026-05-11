# nocov start
#' Robust pointrange geom that turns non-finite range endpoints into NA
#'
#' This preserves normal ggplot2 aes inheritance because the cleaning happens
#' after mappings have been evaluated.
#'
#' @inheritParams ggplot2::geom_pointrange
#' @noRd
util_geom_pointrange_robust <- function(mapping = NULL, data = NULL,
                                        stat = "identity",
                                        position = "identity",
                                        ...,
                                        na.rm = TRUE,
                                        show.legend = NA,
                                        inherit.aes = TRUE) {
  if (!identical(na.rm, TRUE)) {
    util_error("Cannot call util_geom_pointrange_robust with na.rm = FALSE")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrangeRobust,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @noRd
util_geom_pointrange_robust_setup_data <- function(data, params) {
  data <- ggplot2::GeomPointrange$setup_data(data, params)

  range_aes <- base::intersect(
    c("xmin", "xmax", "ymin", "ymax"),
    base::names(data)
  )

  for (nm in range_aes) {
    x <- data[[nm]]
    if (base::is.numeric(x)) {
      x[!base::is.finite(x)] <- NA_real_
      data[[nm]] <- x
    }
  }

  data
}

environment(util_geom_pointrange_robust_setup_data) <- baseenv()

GeomPointrangeRobust <- local({
  ee <- new.env(parent = baseenv())
  ee$Parent <- ggplot2::GeomPointrange
  ee$setup_fun <- util_geom_pointrange_robust_setup_data

  evalq(
    ggplot2::ggproto(
      "GeomPointrangeRobust",
      Parent,
      extra_params = base::c(Parent$extra_params, "na.rm"),
      setup_data = setup_fun
    ),
    envir = ee
  )
})
#' Internally used point-range
#'
#' @param data the data returned by `ggplot2::ggplot_build()`
#' @param prestats_data the data before statistics are computed.
#' @param layout the panel layout.
#' @param params parameters for the geom, statistic, and 'constant' aesthetics
#' @param p a ggplot2 object (the conversion may depend on scales, for instance).
#' @param ... currently ignored
#'
#' @export
to_basic.GeomPointrangeRobust <- function(data, prestats_data, layout, params,
                                          p, ...) {
  for (nm in c("xmin", "xmax", "ymin", "ymax")) {
    if (nm %in% names(data) && is.numeric(data[[nm]])) {
      x <- data[[nm]]
      x[!is.finite(x)] <- NA_real_
      data[[nm]] <- x
    }
  }

  data$width <- 0

  list(
    util_prefix_class(data, "GeomErrorbar"),
    util_prefix_class(data, "GeomPoint")
  )
}

#' @noRd
util_prefix_class <- function(x, cls) {
  class(x) <- c(cls, setdiff(class(x), cls))
  x
}
# nocov end
