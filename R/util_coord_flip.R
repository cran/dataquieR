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
util_coord_flip <- function(w, h, p, ref_env, ...) {
  n <- 1
  if (!missing(ref_env)) {
    while (n < 4000 && !identical(rlang::caller_env(n = n), ref_env)) {
      n <- n + 1
    }
  }
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

  if (flip_mode == "noflip") {
    return(coord_cartesian(...))
  } else if (flip_mode == "flip") {
    return(coord_flip(...))
  } else if (flip_mode == "auto") {
    if (w > h) {
      return(coord_flip(...))
    } else {
      return(coord_cartesian(...))
    }
  } else {
    util_error("Internal error 234243xx2423 -- Should never happen.")
  }
}
