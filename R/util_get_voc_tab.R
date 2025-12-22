#' Get the Table with Known Vocabularies
#'
#' @param .data_frame_list [environment] cache for loaded data frames
#'
#' @return [data.frame] the (combined) table with known vocabularies
#' @noRd
util_get_voc_tab <- function(.data_frame_list = .dataframe_environment()) {
  voc_tab <- util_get_concept_info("voc")
  if ("<>" %in% names(.data_frame_list)) { # also labels?
    my_voc <- .dataframe_environment()[["<>"]]
    if (!is.data.frame(my_voc) || !all(c("voc", "url") %in% colnames(my_voc))) {
      util_warning(
        c("Invalid <>-Bookmark-Table, need two columns %s in",
          "a data frame. Ignoring the user-provided table %s"),
        util_pretty_vector_string(c("voc", "url")),
        dQuote("<>"),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE
      )
    } else {
      voc_tab <- voc_tab[!(voc_tab$voc %in% my_voc$voc), , FALSE]
      voc_tab <- util_rbind(my_voc, voc_tab)
    }
  }
  voc_tab
}
