#' Cache for materialized lazy `ggplot`s
#'
#' This environment stores materialized ggplot objects referenced by
#' \code{dq_lazy_ggplot} IDs. It is intentionally not exported and not
#' serialized with result objects.
#'
#' @noRd
.dq_lazy_ggplot_cache <- new.env(parent = emptyenv())
# Counter in favor of RNG no side effects on .Rand.seed; cache only in process.

#' Generate a new ID for a lazy ggplot
#'
#' @noRd
util_lazy_ggplot_next_id <- local({ # nolint
  # Counter for assigning IDs to lazy ggplots
  #
  # We do not use the global RNG state here to avoid side effects on
  # reproducible workflows and tests.
  counter <- 0L
  e <- environment()

  function() {
    e$counter <- e$counter + 1L
    sprintf("dq_lazy_%08x", counter)
  }
})

#' Construct a lazy ggplot object
#'
#' @param expr expression which, when evaluated in \code{env}, returns
#'   a \code{ggplot} object.
#' @param env environment holding all required objects for \code{expr}.
#' @param id optional character ID; if \code{NULL}, a new ID is generated.
#'
#' @return An object of class \code{c("dq_lazy_ggplot", "gg", "ggplot")}.
#'
#' @noRd
dq_lazy_ggplot <- function(expr, env, id = NULL) {

  s3 <- dq_lazy_ggplot_s3(expr = expr, env = env, id = id)

  gg_compatible <-
    as.logical(getOption("dataquieR.lazy_plots_gg_compatibility",
                         dataquieR.lazy_plots_gg_compatibility_default))

  # TODO: make all this S7 magic optional
  if (gg_compatible && dq_lazy_register_s7()) {
    obj <- .dq_lazy_state$s7_class(payload = s3)

    # add an S3 class tag for dispatch (must be a valid S3 class name)
    class(obj) <- c("dq_lazy_ggplot_s7", class(obj))

    return(obj)
  }

  s3
}

dq_lazy_ggplot_s3 <- function(expr, env, id = NULL) {
  stopifnot(is.environment(env))

  if (is.null(id)) {
    id <- util_lazy_ggplot_next_id()
  }

  .dq_lazy_ggplot_cache[[id]] <- NULL

  structure(
    list(
      id   = id,
      expr = expr,
      env  = env
    ),
    class = c("dq_lazy_ggplot")
  )
}
#' Materialize a lazy `ggplot`
#'
#' Evaluate the stored expression in its lean environment and cache
#' the resulting \code{ggplot} object in the current R session, if enabled
#' using the option [dataquieR.lazy_plots_cache].
#'
#' @param x a \code{dq_lazy_ggplot} object.
#'
#' @return A \code{ggplot} object.
#'
#' @export
prep_realize_ggplot <- function(x) {

  x <- dq_lazy_unwrap(x)

  if (!inherits(x, "dq_lazy_ggplot")) {
    return(x)
  }

  x_un <- unclass(x)

  id <- x_un$id
  cached <- .dq_lazy_ggplot_cache[[id]]
  if (!is.null(cached)) {
    return(cached)
  }

  p <- eval(x_un$expr, envir = x_un$env)

  # if (!inherits(p, "ggplot")) {
  #   util_error("dq_lazy_ggplot: expr did not evaluate to a ggplot object, but %s",
  #              util_pretty_vector_string(class(p)))
  # }

  lazy_cache <- as.logical(getOption("dataquieR.lazy_plots_cache",
                               dataquieR.lazy_plots_cache_default))
  if (length(lazy_cache) != 1 || is.na(lazy_cache)) {
    util_warning(c(
      "Cannot use option dataquieR.lazy_plots_cache %s as a logical value",
      "using %s"
    ),
    dQuote(paste(getOption("dataquieR.lazy_plots_cache",
                           dataquieR.lazy_plots_cache_default),
                 collapse = ",")),
    dQuote(dataquieR.lazy_plots_cache_default))
    lazy_cache <- as.logical(dataquieR.lazy_plots_cache_default)
  }

  if (lazy_cache) .dq_lazy_ggplot_cache[[id]] <- p
  p
}

#' Forget all materialized lazy ggplots
#'
#' This clears the in-session cache of materialized plots. It does not
#' affect any stored lazy plot objects themselves.
#'
#' @noRd
util_forget_lazy_ggplots <- function() { # nolint
  rm(list = ls(.dq_lazy_ggplot_cache), envir = .dq_lazy_ggplot_cache)
  invisible(NULL)
}

#' @rdname dq_lazy_ggplot_methods
#' @export
ggplotly.dq_lazy_ggplot_s7 <- function(p, ...) { # nolint
  util_ggplotly(p@payload, ...)
}

#' @rdname dq_lazy_ggplot_methods
#' @export
plotly_build.dq_lazy_ggplot_s7 <- function(p, ...) { # nolint
  util_plotly_build(p@payload, ...)
}

#' @rdname dq_lazy_ggplot_methods
#' @export
ggplotly.dq_lazy_ggplot <- function(p, ...) { # nolint
  util_ggplotly(p, ...)
}

#' @rdname dq_lazy_ggplot_methods
#' @export
plotly_build.dq_lazy_ggplot <- function(p, ...) { # nolint
  util_plotly_build(p, ...)
}

# patchwork ist das einzige Downstream-Paket mit relevanten gg-/ggplot-Generics; cowplot/gridExtra/egg/ggpubr hängen an ggplotGrob()
#' @export
`-.dq_lazy_ggplot` <- function(e1, e2) { # nolint
  p1 <- prep_realize_ggplot(e1)
  if (inherits(e2, "dq_lazy_ggplot")) {
    e2 <- prep_realize_ggplot(e2)
  }
  p1 - e2
}

#' @export
`/.dq_lazy_ggplot` <- function(e1, e2) { # nolint
  p1 <- prep_realize_ggplot(e1)
  if (inherits(e2, "dq_lazy_ggplot")) {
    e2 <- prep_realize_ggplot(e2)
  }
  p1 / e2
}

#' @export
`|.dq_lazy_ggplot` <- function(e1, e2) { # nolint
  p1 <- prep_realize_ggplot(e1)
  if (inherits(e2, "dq_lazy_ggplot")) {
    e2 <- prep_realize_ggplot(e2)
  }
  p1 | e2
}

#' @export
`*.dq_lazy_ggplot` <- function(e1, e2) { # nolint
  p1 <- prep_realize_ggplot(e1)
  if (inherits(e2, "dq_lazy_ggplot")) {
    e2 <- prep_realize_ggplot(e2)
  }
  p1 * e2
}

#' @export
`&.dq_lazy_ggplot` <- function(e1, e2) { # nolint
  p1 <- prep_realize_ggplot(e1)
  if (inherits(e2, "dq_lazy_ggplot")) {
    e2 <- prep_realize_ggplot(e2)
  }
  p1 & e2
}

#' Realize lazy `ggplot`s only when needed
#'
#' @noRd
util_realize_if_lazy <- function(x) { # nolint
  if (inherits(x, "dq_lazy_ggplot")) {
    prep_realize_ggplot(x)
  } else {
    x
  }
}


#' S3/S7 methods for lazy ggplot objects
#'
#' These S3/S7 methods make \code{dq_lazy_ggplot}/\code{dq_lazy_ggplot_s7}
#' objects work smoothly with
#' functions from \pkg{ggplot2} and \pkg{plotly}. They simply materialize
#' the underlying \code{ggplot} object and then delegate to the respective
#' generic.
#'
#' @param x,p A \code{dq_lazy_ggplot} object.
#' @param ... Further arguments passed on to the underlying generic.
#'
#' @return
#' The return value is the same as for the corresponding generic:
#' \itemize{
#'   \item \code{ggplotGrob()} returns a \code{gtable} object.
#'   \item \code{ggplotly()} returns a \code{plotly} object.
#'   \item \code{plotly_build()} returns a \code{plotly_proxy} or similar.
#' }
#'
#' @seealso \code{\link[ggplot2]{ggplotGrob}},
#'   \code{plotly::ggplotly}
#'   \code{plotly::plotly_build}
#'
#' @name dq_lazy_ggplot_methods
NULL

#' @export
`$.dq_lazy_ggplot_s7` <- function(x, name) { # nolint
  `$.dq_lazy_ggplot`(x@payload, name)
}

#' @export
`[[.dq_lazy_ggplot_s7` <- function(x, ...) { # nolint
  `[[.dq_lazy_ggplot`(x@payload, ...)
}

#' @export
ggplot_build.dq_lazy_ggplot_s7 <- function(plot, ...) { # nolint
  ggplot2::ggplot_build(prep_realize_ggplot(plot@payload), ...)
}

#' @export
print.dq_lazy_ggplot_s7 <- function(x, ...) { # nolint
  print(prep_realize_ggplot(x@payload), ...)
}

#' @rdname dq_lazy_ggplot_methods
#' @export
ggplotGrob.dq_lazy_ggplot_s7 <- function(x, ...) { # nolint
  ggplot2::ggplotGrob(prep_realize_ggplot(x@payload), ...)
}

#' @export
util_undisclose.dq_lazy_ggplot_s7 <- function(x, ...) { # nolint
  util_undisclose(x@payload, ...)
}

