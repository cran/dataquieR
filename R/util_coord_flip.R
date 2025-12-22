#' return a flip term for `ggplot2` plots, if desired.
#'
#' @param w width of the plot to determine its aspect ratio
#' @param h height of the plot to determine its aspect ratio
#' @param p the `ggplot2` object, so far. If `w` or `h` are missing, `p` is used
#'        for an estimate on `w` and `h`, if both axes are discrete.
#' @param ref_env environment of the actual entry function, so that the correct
#'                formals can be detected.
#' @param ... additional arguments for `coord_flip` or `coord_cartesian`
#'
#' @return `coord_flip` or `coord_cartesian`
#' @importFrom ggplot2 coord_flip coord_cartesian
#'
#' @family process_functions
#' @concept process
#' @noRd
util_coord_flip <- function(w, h, p, ref_env, ...) {
  ## NEU: ggf. lazy-Plot einmal realisieren, bevor wir layer_scales() etc. nutzen
  if (!missing(p) && inherits(p, "dq_lazy_ggplot")) {
    p <- prep_realize_ggplot(p)
  }

  n <- 1
  if (!missing(ref_env)) {
    while (n < 4000 && !identical(rlang::caller_env(n = n), ref_env)) {
      n <- n + 1
    }
  }
  if (n >= 4000) {
    util_error(c("Internal error, sorry. As a dataquieR developer: There is",
                 "some call to util_coord_flip with a ref_env outside the",
                 "call trace, maybe inside a util_create_lean_ggplot()"))
  }
  rm(ref_env) # this reference should go to save resources
  calling_fn_name <- rlang::call_name(rlang::caller_call(n = n))
  if (!"flip_mode" %in% names(formals(calling_fn_name, envir =
                                      rlang::caller_env(n = n)))) {
    util_error("%s can only be called from a function with the %s argument",
               sQuote(rlang::caller_call()[[1]]),
               sQuote("flip_mode"))
  }
  .default <- formals(calling_fn_name, envir =
                        rlang::caller_env(n = n))$flip_mode
  .default <- eval(.default, envir = rlang::caller_env(n = n))
  if (eval(call("missing", as.symbol("flip_mode")), envir =
           rlang::caller_env(n = n))) {
    flip_mode <- getOption("dataquieR.flip_mode", dataquieR.flip_mode_default)
  } else {
    flip_mode <- get("flip_mode", parent.frame())
  }
  flip_mode <- util_match_arg(flip_mode, c("default", "flip", "noflip", "auto"))
  if (flip_mode == "default") {
    flip_mode <- .default
  }

  if ((flip_mode == "auto") && (missing(w) || missing(h))) {

    if (!missing(p)) {
      if (inherits(ggplot2::layer_scales(p)$x, "ScaleDiscrete") &&
          inherits(ggplot2::layer_scales(p)$y, "ScaleDiscrete")) {
        w <- length(ggplot2::layer_scales(p)$x$get_limits())
        h <- length(ggplot2::layer_scales(p)$y$get_limits())
      }
    }

    flip_mode <- .default
    if (flip_mode == "auto") {
      flip_mode <- dataquieR.flip_mode_default
    }
    if (flip_mode == "auto") {
      flip_mode <- "noflip"
    }
  }

  args <- list(...)

  if (flip_mode == "noflip") {
    return(util_create_lean_ggplot(
      do.call(coord_cartesian, args),
      args = args,
      .lazy = FALSE
    ))
  } else if (flip_mode == "flip") {
    return(util_create_lean_ggplot(
      do.call(coord_flip, args),
      args = args,
      .lazy = FALSE
    ))
  } else if (flip_mode == "auto") {
    if (w > h) {
      return(util_create_lean_ggplot(
        do.call(coord_flip, args),
        args = args,
        .lazy = FALSE
      ))
    } else {
      return(util_create_lean_ggplot(
        do.call(coord_cartesian, args),
        args = args,
        .lazy = FALSE
      ))
    }
  } else {
    util_error("Internal error 234243xx2423 -- Should never happen.")
  }
}



#' Lazily add a `coord` to a (possibly lazy) ggplot
#'
#' This helper attaches a coordinate system (e.g. \code{coord_flip()},
#' \code{coord_cartesian()}) to a plot. If \code{p} is a
#' \code{dq_lazy_ggplot}, a new lazy wrapper is created whose
#' expression materializes \code{p} and adds \code{coord}. For plain
#' \code{ggplot} objects, the function simply returns \code{p + coord}.
#'
#' @param p A \code{ggplot} or \code{dq_lazy_ggplot} object.
#' @param coord A ggplot2 coordinate object (e.g. from
#'   \code{ggplot2::coord_flip()}, \code{ggplot2::coord_cartesian()}).
#'
#' @return A \code{ggplot} or \code{dq_lazy_ggplot} object, depending
#'   on \code{p} and the global \code{dataquieR.lazy_plots} option.
#'
#' @noRd
util_lazy_add_coord <- function(p, coord) { # nolint
  ## "altes" Verhalten: für echte ggplots einfach p + coord
  if (!inherits(p, "dq_lazy_ggplot")) {
    return(p + coord)
  }

  ## globale Lazy-Option wie bei %lean+% auswerten
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

  ## neue Lazy-Hülle: beim Realisieren wird p materialisiert und coord angehängt
  util_create_lean_ggplot(
    {
      prep_realize_ggplot(p) + coord
    },
    p     = p,
    coord = coord,
    .lazy = lazy
  )
}
