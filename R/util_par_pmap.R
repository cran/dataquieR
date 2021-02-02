#' Utility function parallel version of `purrr::pmap`
#'
#' Parallel version of `purrr::pmap`.
#'
#'
#' @param .l [data.frame] with one call per line and one function argument
#'                        per column
#' @param .f [`function`] to call with the arguments from `.l`
#' @param ... additional, static arguments for calling `.f`
#'
#' @param cores number of cpu cores to use or a (named) list with arguments for
#'              [parallelMap::parallelStart] or NULL, if parallel has already
#'              been started by the caller.
#' @param use_cache [logical] set to FALSE to omit re-using already distributed
#'                            study- and metadata on a parallel cluster
#'
#' @seealso `purrr::pmap`
#' @seealso [Stack Overflow post](https://stackoverflow.com/a/47575143)
#'
#' @author [Aurèle](https://stackoverflow.com/users/6197649)
#' @author S Struckmann
#'
#' @return [list] of results of the function calls
util_par_pmap <- function(.l, .f, ...,
                          cores = list(mode = "socket",
                                       logging = FALSE,
                                       load.balancing = TRUE),
                          use_cache = FALSE) {
  if (!is.null(cores)) {
    if (inherits(cores, "list")) {
      suppressMessages(do.call(parallelMap::parallelStart, cores))
    } else {
      suppressMessages(parallelMap::parallelStart("socket", cpus = cores,
                                                  logging = FALSE,
                                                  load.balancing = TRUE))
    }
    on.exit(suppressMessages(parallelMap::parallelStop()))
  }
  more_args <- list(...)
  if ("meta_data" %in% names(more_args)) {
    meta_data <- more_args[["meta_data"]]
    if (use_cache &&
        !all(unlist(parallelMap::parallelMap(exists, "meta_data")))) {
      parallelMap::parallelExport("meta_data")
      more_args[["meta_data"]] <- NULL
    }
  }
  if ("study_data" %in% names(more_args)) {
    study_data <- more_args[["study_data"]]
    if (use_cache &&
        !all(unlist(parallelMap::parallelMap(exists, "study_data")))) {
      parallelMap::parallelExport("study_data")
      more_args[["study_data"]] <- NULL
    }
  }
  do.call(
    parallelMap::parallelMap,
    c(.l, list(
      fun = .f, more.args = more_args, simplify = FALSE, use.names = FALSE,
      show.info = FALSE, impute.error = identity
    ))
  )
  #  do.call(
  #    parallel::mcmapply,
  #    c(.l, list(FUN = .f, MoreArgs = list(...), SIMPLIFY = FALSE,
  #         mc.cores = mc.cores))
  #  )
}
