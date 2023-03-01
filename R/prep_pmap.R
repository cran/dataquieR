#' Support function for a parallel `pmap`
#'
#' parallel version of `purrr::pmap`
#'
#' @param .l [data.frame] with one call per line and one function argument
#'                        per column
#' @param .f [`function`] to call with the arguments from `.l`
#' @param ... additional, static arguments for calling `.f`
#' @param cores number of cpu cores to use or a (named) list with arguments for
#'              [parallelMap::parallelStart] or NULL, if parallel has already
#'              been started by the caller. Set to 0 to run without
#'              parallelization.
#'
#' @seealso `purrr::pmap`
#' @seealso [Stack Overflow post](https://stackoverflow.com/a/47575143)
#'
#' @author [Aurèle](https://stackoverflow.com/users/6197649)
#' @author S Struckmann
#'
#' @return [list] of results of the function calls
#' @export
prep_pmap <- function(.l, .f, ..., cores = 0) {
  if (!is.function(.f)) {
    util_error("Argument %s should be a function.", dQuote(".f"))
  }

  if (!is.null(cores)) {
    if (getOption("parallelMap.status", "stopped") != "stopped") {
      util_warning("prep_pmap called encapsulated",
                   applicability_problem = FALSE)
    }
    if (identical(cores, 0)) {
      suppressMessages(parallelMap::parallelStart(mode = "local"))
    } else if (inherits(cores, "list")) { # nocov start
      # Should not test in parallel
      suppressMessages(do.call(parallelMap::parallelStart, cores))
    } else {
      suppressMessages(parallelMap::parallelStart("socket", cpus = cores,
                                                  logging = FALSE,
                                                  load.balancing = TRUE))
      # Should not test in parallel
    } # nocov end
    on.exit(suppressMessages(parallelMap::parallelStop()))
  }
  more_args <- list(...)
  do.call(
    parallelMap::parallelMap,
    c(.l, list(
      fun = .f, more.args = more_args, simplify = FALSE, use.names = FALSE,
      show.info = FALSE, impute.error = identity
    ))
  )
}
