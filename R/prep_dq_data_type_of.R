#' Get the dataquieR `DATA_TYPE` of `x`
#'
#' @param x object to define the dataquieR data type of
#'
#' @return the dataquieR data type as listed in `DATA_TYPES`
#' @seealso [`DATA_TYPES_OF_R_TYPE`]
#' @export
prep_dq_data_type_of <- function(x) {
  r <- head(unique(intersect(names(DATA_TYPES_OF_R_TYPE), class(x))), 1)
# FIXME: for strings, try readr::guess_parser(as.character(1:10), guess_integer = TRUE)
  if (length(r) != 1) {
    NULL
  } else {
    if (r %in% c("numeric", "float", "double")) {
      if (all(util_is_integer(x))) {
        r <- "integer"
      }
    }

    tolower(DATA_TYPES_OF_R_TYPE[[r]])
  }
}
