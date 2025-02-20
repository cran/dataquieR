#' All valid `voc:` vocabularies
#'
#' @return [character()] all `voc:` suffixes allowed for
#'                       [prep_get_data_frame()].
#' @export
#'
#' @examples
#' \dontrun{
#' prep_list_dataframes()
#' prep_list_voc()
#' prep_get_data_frame("<ICD10>")
#' my_voc <-
#'   tibble::tribble(
#'     ~ voc, ~ url,
#'     "test", "data:datasets|iris|Species+Sepal.Length")
#' prep_add_data_frames(`<>` = my_voc)
#' prep_list_dataframes()
#' prep_list_voc()
#' prep_get_data_frame("<test>")
#' prep_get_data_frame("<ICD10>")
#' my_voc <-
#'   tibble::tribble(
#'     ~ voc, ~ url,
#'     "ICD10", "data:datasets|iris|Species+Sepal.Length")
#' prep_add_data_frames(`<>` = my_voc)
#' prep_list_dataframes()
#' prep_list_voc()
#' prep_get_data_frame("<ICD10>")
#' }
#'
prep_list_voc <- function() { # TODO: Also the custom tables
  voc_tab <- util_get_voc_tab()
  # table lookup to modify fn to package: or data:
  try(subset(voc_tab, select = "voc", drop = TRUE))
}
