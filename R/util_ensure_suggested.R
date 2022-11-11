#' Support function to stop, if an optional package is not installed
#'
#' This function stops, if a package is not installed but needed for using an
#' optional feature of `dataquieR`.
#'
#' @param pkg needed package
#' @param goal feature description for error message.
util_ensure_suggested <- function(pkg, goal) {
  missingp <- !vapply(pkg,
                      FUN.VALUE = logical(1),
                      requireNamespace,
                      quietly = TRUE)
  if (any(missingp)) {
    util_error(c("Missing the package(s) %s to %s.",
                 "Install with install.packages(%s)"),
               paste0(dQuote(pkg[missingp]), collapse = ", "),
               goal,
               deparse(pkg[missingp]))
  }
}
