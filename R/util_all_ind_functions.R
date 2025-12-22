#' All indicator functions of `dataquieR`
#' @return [character] names of all indicator functions
#'
#' @noRd
util_all_ind_functions <- function() {
  all_inds <-
    lapply(paste0(names(dims), '_'),
           startsWith,
           x = getNamespaceExports(utils::packageName()))
  all_inds <- Reduce(`|`, all_inds)
  getNamespaceExports(utils::packageName())[all_inds]
  # TOOD: Use also, e.g., in util_generate_calls instead of manual names
}
