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
#'
#' @return a binary vector `(0, 1)` if data type applies
#'
#' @importFrom stats setNames
util_compare_meta_with_study <- function(sdf, mdf, label_col) {
  missing_in_study_data <- !(mdf[[label_col]] %in% colnames(sdf))
  sdf <- sdf[, as.character(mdf[[label_col]][!missing_in_study_data])]
  is_data_type <- vapply(setNames(nm = colnames(sdf)), function(x) {
    as.numeric(all(util_check_data_type(sdf[, x, drop = TRUE],
                                        mdf$DATA_TYPE[mdf[[label_col]] == x]),
                   na.rm = TRUE))
  }, 1)
  return(is_data_type)
}
