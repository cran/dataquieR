#' Check for package updates
#'
#' @param beta [logical] check for beta version too
#' @param deps [logical] check for missing (optional) dependencies
#' @param ask [logical] ask for updates
#'
#' @return `invisible(NULL)`
#' @export
prep_check_for_dataquieR_updates <- function(beta = FALSE,
                                             deps = TRUE,
                                             ask = interactive()) { # nocov start
  util_expect_scalar(beta, check_type = is.logical)
  util_expect_scalar(deps, check_type = is.logical)
  util_expect_scalar(ask, check_type = is.logical)
  pn <- packageName()
  util_message("Looking for updates of %s. Currently installed: %s",
               sQuote(pn),
               sQuote(packageVersion(pn)))
  if (beta) {
    r <- list(
      QIHS = "https://packages.qihs.uni-greifswald.de//repository/ship-snapshot-r/"
    )
  } else {
    r <- list()
  }
  rlang::local_options(repos = c(
        r,
        getOption("repos")
    )
  )
  if (beta) {
    utils::update.packages(oldPkgs = pn, ask = ask, dependencies = deps,
                           type = "source")
  } else {
    utils::update.packages(oldPkgs = pn, ask = ask, dependencies = deps)
  }
  all_deps <- unique(trimws(unlist(strsplit(split = ",", fixed = TRUE, x =
                          unname(unlist(utils::packageDescription("dataquieR",
    fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))))))))
  all_deps <- all_deps[!util_empty(all_deps)]
  all_deps <- all_deps[!startsWith(all_deps, "R ")]
  if (ask) {
    rlang::check_installed(all_deps,
                           sprintf("as (soft) dependencies of %s",
                                   dQuote(packageName())))
  } else {
    to_install <- !vapply(all_deps, rlang::is_installed,
                          FUN.VALUE=logical(1))
    if (rlang::is_installed("pak")) {
      cat("Installing using pak: ", dQuote(names(to_install[to_install])), "\n")
      pkg_install <- rlang::env_get(rlang::ns_env("pak"), "pkg_install")
      pkg_install(names(to_install[to_install]), ask = FALSE)
    } else {
      cat("Installing ", dQuote(names(to_install[to_install])), "\n")
      utils::install.packages(names(to_install[to_install]))
    }
  }
  invisible(NULL)
} # nocov end
