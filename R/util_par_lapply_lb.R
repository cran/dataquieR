#' Parallel or fallback to serial `lapply`
#'
#' Wraps `parallel::parLapplyLB`, but if no cluster is available,
#' falls back to `lapply()`. The cluster is automatically detected using
#' `parallel::getDefaultCluster()`.
#'
#' @param cl cluster Optional. A cluster object as returned by `makeCluster()`.
#'                     If `NULL`, attempts to use `parallel::getDefaultCluster()`.
#'                     If that is also `NULL`, falls back to serial `lapply()`.
#' @param X [list] The list of elements to iterate over.
#' @param fun [function] The function to apply to each element.
#' @param ... Further arguments passed to `fun`.
#' @param chunk.size [integer] Optional chunk size passed to `parLapplyLB`.
#'
#' @return A list of results, like `lapply()` or `parLapplyLB()` would return.
#' @noRd
util_par_lapply_lb <- function(cl = parallel::getDefaultCluster(),
                               X, fun, ..., chunk.size = NULL) {
  if (is.null(cl)) {
    # No cluster: fallback to serial execution
    return(lapply(X, fun, ...))
  } else {
    # Cluster execution
    if (is.null(chunk.size)) {
      return(parallel::parLapplyLB(cl = cl, X = X, fun = fun, ...))
    } else {
      return(parallel::parLapplyLB(cl = cl, X = X, fun = fun,
                                   chunk.size = chunk.size, ...))
    }
  }
}
