util_gg_get <- function(x, name, default = NULL) {
  if (is.null(x) || is.null(name)) {
    return(default)
  }

  out <- default

  # --- ggproto objects: don't second-guess their internals -------------------
  if (inherits(x, "ggproto")) {
    # For ggproto, $ is the canonical accessor for fields
    val <- tryCatch(x[[name]], error = function(e) NULL)
    if (!is.null(val)) {
      out <- val
    } else if (exists(name, envir = x, inherits = FALSE)) {
      out <- get(name, envir = x, inherits = FALSE)
    } else {
      return(default)
    }

    # --- environments (non-ggproto) -------------------------------------------
  } else if (is.environment(x)) {
    if (exists(name, envir = x, inherits = FALSE)) {
      out <- get(name, envir = x, inherits = FALSE)
    } else {
      return(default)
    }

    # --- named lists -----------------------------------------------------------
  } else if (is.list(x) && !is.null(names(x)) && name %in% names(x)) {
    out <- x[[name]]

    # --- attributes fallback ---------------------------------------------------
  } else {
    att <- attr(x, name, exact = TRUE)
    if (!is.null(att)) {
      out <- att
    } else {
      return(default)
    }
  }

  # For some known list-y fields, normalize environments to lists
  if (is.environment(out) &&
      !inherits(x, "ggproto") &&
      name %in% c("data", "layers", "panel_scales_x")) {
    out <- as.list(out)
  }

  out
}
