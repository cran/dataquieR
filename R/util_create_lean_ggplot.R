#' Evaluate an expression in a clean environment
#'
#' This helper is meant to create tidy \code{ggplot2} objects with fewer
#' unneeded objects included. If \code{.lazy = TRUE}, a lazy ggplot
#' object is returned which will only be materialized when actually used.
#'
#' @param expr the expression that creates a plot.
#' @param ... all objects needed by \code{expr}, must be named.
#' @param .lazy logical, if \code{TRUE}, return a \code{dq_lazy_ggplot}
#'   instead of directly evaluating the expression.
#'
#' @return A \code{ggplot} or \code{dq_lazy_ggplot} object.
#'
#' @noRd
util_create_lean_ggplot <- function(expr, ..., .lazy = FALSE) { # nolint
  expr <- substitute(expr, environment())
  ee <- new.env(parent = environment(util_create_lean_ggplot))
  list2env(list(...), ee)

  if (.lazy) {
    dq_lazy_ggplot(expr, ee)
  } else {
    eval(expr, envir = ee)
  }
}

#' Lean ggplot2 composition operator
#'
#' This operator uses \code{util_create_lean_ggplot} to combine
#' \code{ggplot2} components in a clean environment. If the option
#' \code{dataquieR.lazy_plots} is set to \code{TRUE}, a lazy
#' \code{dq_lazy_ggplot} object is returned.
#'
#' @noRd
`%lean+%` <- function(lhs, rhs) {
  lazy <- as.logical(getOption("dataquieR.lazy_plots",
                               dataquieR.lazy_plots_default))
  if (length(lazy) != 1 || is.na(lazy)) {
    util_warning(c(
      "Cannot use option dataquieR.lazy_plots %s as a logical value",
      "using %s"
    ),
    dQuote(paste(getOption("dataquieR.lazy_plots",
                           dataquieR.lazy_plots_default),
                 collapse = ",")),
    dQuote(dataquieR.lazy_plots_default))
    lazy <- as.logical(dataquieR.lazy_plots_default)
  }

  util_create_lean_ggplot(
    {
      util_realize_if_lazy(lhs) + util_realize_if_lazy(rhs)
    },
    lhs = lhs,
    rhs = rhs,
    .lazy = lazy
  )
}


#' Explicit lazy lean ggplot2 composition operator
#'
#' This variant always returns a lazy \code{dq_lazy_ggplot} object.
#'
#' @noRd
`%lean_lazy+%` <- function(lhs, rhs) {
  util_create_lean_ggplot(
    {
      util_realize_if_lazy(lhs) + util_realize_if_lazy(rhs)
    },
    lhs = lhs,
    rhs = rhs,
    .lazy = TRUE
  )
}


# global switch by option, existing  code using %lean+% remains unchanged.


#' @export
print.dq_lazy_ggplot <- function(x, ...) { # nolint
  p <- prep_realize_ggplot(x)
  print(p, ...)
  invisible(x)
}

#' @export
`$.dq_lazy_ggplot` <- function(x, name) { # nolint
  p <- prep_realize_ggplot(x)
  p[[name]]
}

#' @export
`[[.dq_lazy_ggplot` <- function(x, ...) { # nolint
  p <- prep_realize_ggplot(x)
  `[[`(p, ...)
}

# ggplot_build & ggplotGrob (important ggplot2-inteactions,
# cowplot/gridExtra/egg/ggpubr)
#' @export
ggplot_build.dq_lazy_ggplot <- function(plot, ...) { # nolint
  p <- prep_realize_ggplot(plot)
  ggplot2::ggplot_build(p, ...)
}

as_grob.dq_lazy_ggplot_s7 <- function(plot, ...) { # nolint
  ggplot2::ggplotGrob(prep_realize_ggplot(plot@payload), ...)
}

as_grob.dq_lazy_ggplot <- function(plot, ...) { # nolint
  ggplot2::ggplotGrob(prep_realize_ggplot(plot), ...)
}

#' @rdname dq_lazy_ggplot_methods
#' @export
ggplotGrob.dq_lazy_ggplot <- function(x, ...) { # nolint
  p <- prep_realize_ggplot(x)
  ggplot2::ggplotGrob(p, ...)
}

# as soon as someone build upon the plot, it's okay,
# to create a normal ggplot. Laziness will be gone
#' @export
`+.dq_lazy_ggplot` <- function(e1, e2) { # nolint
  p <- prep_realize_ggplot(e1)
  if (inherits(e2, "dq_lazy_ggplot")) {
    e2 <- prep_realize_ggplot(e2)
  }
  p + e2
}
