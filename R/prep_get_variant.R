#' Get machine variant for snapshot tests
#'
#' @return [character] the variant
#'
#' @keywords internal
#' @export
prep_get_variant <- function() {
  v <- paste0("R", getRversion()[, 1:2])
#  if (v == "R4.4") { # current developer version
#    return(NULL)
#  } else {
    return(v)
#  }
}
