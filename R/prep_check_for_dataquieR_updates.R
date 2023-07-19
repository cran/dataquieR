#' Check for package updates
#'
#' @param beta [logical] check for beta version too
#' @param deps [logical] check for missing (optional) dependencies
#'
#' @return `invisible(NULL)`
#' @export
prep_check_for_dataquieR_updates <- function(beta = FALSE, deps = TRUE) {
  util_expect_scalar(beta, check_type = is.logical)
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
  utils::update.packages(oldPkgs = pn, ask = TRUE, dependencies = TRUE)
  all_deps <- unique(trimws(unlist(strsplit(split = ",", fixed = TRUE, x =
                          unname(unlist(utils::packageDescription("dataquieR",
    fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))))))))
  all_deps <- all_deps[!util_empty(all_deps)]
  all_deps <- all_deps[!startsWith(all_deps, "R ")]
  rlang::check_installed(all_deps,
                         sprintf("as (soft) dependencies of %s",
                                 dQuote(packageName())))
  invisible(NULL)
}
