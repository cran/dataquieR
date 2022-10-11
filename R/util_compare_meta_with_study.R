#' Compares study data data types with the ones expected according to the
#' metadata
#'
#' Utility function to compare data type of study data with those defined
#' in metadata
#'
#' @param sdf the [data.frame] of study data
#' @param mdf the [data.frame] of associated static meta data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param check_convertible [logical] also try, if a conversion to the
#'                                    declared data type would work.
#' @param threshold_value [numeric] from=0 to=100. percentage failing
#'                                  conversions allowed if `check_convertible`
#'                                  is `TRUE`.
#'
#' @return if `check_convertible` is `FALSE`,
#'         a binary vector `(0, 1)` if data type applies,
#'         if `check_convertible` is `TRUE``
#'         a vector with the states `0, 1, 2`: 0 = Mismatch, not convertible
#'                                             1 = Match
#'                                             2 = Mismatch, but convertible
#' @importFrom stats setNames
util_compare_meta_with_study <- function(sdf, mdf, label_col,
                                         check_convertible = FALSE,
                                         threshold_value = 0) {
  missing_in_study_data <- !(mdf[[label_col]] %in% colnames(sdf))
  sdf <- sdf[, as.character(mdf[[label_col]][!missing_in_study_data]),
             drop = FALSE]
  is_data_type <- vapply(setNames(nm = colnames(sdf)),
                         function(x, check_convertible, threshold_value) {
    as.integer(util_check_data_type(sdf[, x, drop = TRUE],
                                        mdf$DATA_TYPE[mdf[[label_col]] == x],
                                        check_convertible = check_convertible,
                                        threshold_value = threshold_value)
                   )
  }, FUN.VALUE = integer(1),
    check_convertible = check_convertible,
    threshold_value = threshold_value)
  return(is_data_type)
}
