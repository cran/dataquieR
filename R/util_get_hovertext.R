#' Import vector of hover text for tables in the report
#'
#' @param x name of the tables. They are `meta_data`,
#'            `meta_data_segment`, `meta_data_dataframe`, `meta_data_cross_item`,
#'            `meta_data_item_computation`, `com_item_missingness`,
#'            `int_datatype_matrix`, `con_inadmissible_categorical`,
#'            `rulesetformat`, `gradingruleset`
#' @return named vector containing the hover text from the file `metadata-hovertext.rds`
#'          in the inst folder. Names correspond to column names in the metadata
#'          tables
#'
#' @family html
#' @concept reporting
#' @noRd
util_get_hovertext <- function(x) {
  f <- system.file(paste0("hovertext", ".rds"), package = "dataquieR")
  if (exists(f, .concept_chache, mode = "list")) {
    list_of_hover_metadata <- get(f, .concept_chache, mode = "list")
  } else {
    if (0 != file.access(f, 4)) {
      util_error("Cannot read file %s. Internal error.",
                 dQuote("metadata-hovertext"))
    }
    list_of_hover_metadata <- readRDS(f)
    assign(f, list_of_hover_metadata, .concept_chache)
  }
  list_of_hover <- list_of_hover_metadata[[x]]
  list_of_hover
}
