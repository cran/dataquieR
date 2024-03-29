#' Get description for a call
#'
#' @param cn the call name
#'
#' @return the description
#'
#' @keywords internal
util_col_description <- function(cn) {

  fname <- util_map_by_largest_prefix(
    cn,
    haystack = names(.manual$titles))

  if (is.na(fname)) {
    fname <- cn
  }

  return(util_function_description(fname))

}
