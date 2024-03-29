#' RStudio crashes on parallel calls in some versions on Darwin based operating
#' systems with R 4
#'
#' @return invisible null
#'
#' @family robustness_functions
#' @concept system
#' @keywords internal
util_fix_rstudio_bugs <- function() { # nocov start
  if (!util_really_rstudio()) return(invisible(NULL))
  # https://github.com/rstudio/rstudio/issues/6692
  if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      if (rstudioapi::isAvailable()) {
        rstudio_too_old <- rstudioapi::versionInfo()$version < "1.3.1056"
      } else {
        rstudio_too_old <- FALSE
      }
    } else {
      util_user_hint(sprintf(
        paste0("Without the package %s, I cannot decide, if your RStudio is",
               "at least at version 1.3.1056, so I'll activate a work-around",
               "for a known parallel-bug fixed in newer RStudios."),
        dQuote("rstudioapi")
      ))
      rstudio_too_old <- TRUE
    }
    if (rstudio_too_old) {
      util_user_hint(paste0("Enabling workaround for an RStudio bug:",
                             "https://github.com/rstudio/rstudio/issues/6692"))
      if (exists("setDefaultClusterOptions", asNamespace("parallel"),
                 mode = "function")) {
        get("setDefaultClusterOptions", asNamespace("parallel"),
            mode = "function")(setup_strategy = "sequential")
      }
    }
  }
  invisible(NULL)
} # nocov end
