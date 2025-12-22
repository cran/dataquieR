#' Find all columns in item-level-metadata, that refer to some other variable
#'
#' @param meta_data [data.frame] the metadata
#'
#' @return [character] all column names referring to variables from item-level
#'                     metadata
#'
#' @family lookup_functions
#' @concept metadata_management
#' @noRd

util_variable_references <- function(meta_data = "item_level") {
  util_expect_data_frame(meta_data)
  colnames(meta_data)[startsWith(colnames(meta_data), "KEY_") |
                      startsWith(colnames(meta_data), "GROUP_VAR_") |
                      colnames(meta_data) == "TIME_VAR" |
                      colnames(meta_data) == "PART_VAR" |
                      colnames(meta_data) == "CO_VARS"]
}
