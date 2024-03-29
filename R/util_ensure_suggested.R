#' Support function to stop, if an optional package is not installed
#'
#' This function stops, if a package is not installed but needed for using an
#' optional feature of `dataquieR`.
#'
#' @param pkg needed package
#' @param err [logical] Should the function throw an error (default) or a
#'            warning?
#' @param goal feature description for error message.
#' @param and_import import the listed function to the caller's environment
#' @return `TRUE` if all packages in `pkg` are available, `FALSE` if at least
#'         one of the packages is missing.
#' @examples
#' \dontrun{ # internal use, only
#' f <- function() {
#'   util_ensure_suggested <- get("util_ensure_suggested",
#'     asNamespace("dataquieR"))
#'   util_ensure_suggested("ggplot2", "Test",
#'       and_import = "(ggplot|geom_.*|aes)")
#'   print(ggplot(cars, aes(x = speed)) + geom_histogram())
#' }
#' f()
#' }
#'
#' @family robustness_functions
#' @concept process
#' @keywords internal
util_ensure_suggested <- function(pkg, goal =
                                    ifelse(
                                      is.null(
                                        rlang::caller_call()),
                                      "work",
                                      paste("call", sQuote(rlang::call_name(
                                                      rlang::caller_call())))),
                                  err = TRUE, and_import = c()) {
  util_expect_scalar(err, check_type = is.logical)
  missingp <- !vapply(pkg,
                      FUN.VALUE = logical(1),
                      requireNamespace,
                      quietly = TRUE)
  if (err && any(missingp)) {
    rlang::check_installed(
      call = rlang::caller_env(),
      pkg = pkg,
      reason = paste("to", goal)
    )
    missingp <- !vapply(pkg,
                        FUN.VALUE = logical(1),
                        requireNamespace,
                        quietly = TRUE)
  }
  if (any(missingp)) {
    if (err) {
      lambda <- util_error
    } else {
      lambda <- util_warning
    }
    if (rlang::is_installed("cli")) {
      install_all <-
        cli::cli_text(
          sprintf("Call {.run [%s](%s)} to install all suggested packages",
                  "prep_check_for_dataquieR_updates()",
                  "dataquieR::prep_check_for_dataquieR_updates()")
        )
    } else {
      install_all <-
        sprintf("Call %s to install all suggested packages",
                dQuote("prep_check_for_dataquieR_updates()"))
    }
    lambda(c("Missing the package(s) %s to %s.",
             "Install with install.packages(%s).",
             install_all),
           paste0(dQuote(pkg[missingp]), collapse = ", "),
           goal,
           deparse(pkg[missingp]))
    return(FALSE)
  } else {
    if (length(and_import) > 0) {
      caller_env <- parent.frame()
      for (nm in ls(
        pattern = and_import, name = asNamespace(pkg))) {
        if (nm %in% getNamespaceExports(pkg)) {
          assign(nm, get(nm, asNamespace(pkg)), caller_env)
        }
      }
    }
    return(TRUE)
  }
}
