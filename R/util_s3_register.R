#' Register an S3 method for an external generic
#'
#' This helper allows to register methods for generics defined in
#' suggested packages (e.g., plotly, patchwork) without creating
#' hard dependencies.
#'
#' @param generic character of the form "pkg::generic".
#' @param class character class name.
#' @param method function to register.
#'
#' @noRd
util_s3_register <- function(generic, class, method) { # nolint
  parts <- strsplit(generic, "::", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) {
    util_error("`generic` must be of the form \"pkg::generic\".")
  }
  pkg <- parts[[1L]]
  gen <- parts[[2L]]

  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(FALSE))
  }

  ## Wichtig: explizit utils::registerS3method() und positionsbasierte Argumente
  base::registerS3method(
    gen,
    class,
    method,
    envir = asNamespace(pkg)
  )

  invisible(TRUE)
}
