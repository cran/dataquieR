#' Get description for an indicator function
#'
#' @param fname the function name
#'
#' @return the description
#'
#' @noRd
util_function_description <- function(fname) {
  # def <-
  #   util_map_labels(fname,
  #                   util_get_concept_info("implementations"),
  #                   to = "Definition",
  #                   from = "function_R",
  #                   ifnotfound = "")
  # if (gsub("[^a-z]", "", tolower(trimws(def))) == "na") {
  #   def <- ""
  # }
  # expl <-
  #   util_map_labels(fname,
  #                   util_get_concept_info("implementations"),
  #                   to = "Explanation",
  #                   from = "function_R",
  #                   ifnotfound = "")
  # if (gsub("[^a-z]", "", tolower(trimws(expl))) == "na") {
  #   expl <- ""
  # }
  # rel <-
  #   util_map_labels(fname,
  #                   util_get_concept_info("implementations"),
  #                   to = "Relevance",
  #                   from = "function_R",
  #                   ifnotfound = "")
  # if (gsub("[^a-z]", "", tolower(trimws(rel))) == "na") {
  #   rel <- ""
  # }
  # guidance <-
  #   util_map_labels(fname,
  #                   util_get_concept_info("implementations"),
  #                   to = "Guidance",
  #                   from = "function_R",
  #                   ifnotfound = "")
  # if (gsub("[^a-z]", "", tolower(trimws(guidance))) == "na") {
  #   guidance <- ""
  # }
  int <-
    util_map_labels(fname,
                    util_get_concept_info("implementations"),
                    to = "Interpretation",
                    from = "function_R",
                    ifnotfound = "")
  if (is.na(int) || gsub("[^a-z]", "", tolower(trimws(int))) == "na") {
    int <- ""
  }
  desc <- paste(int, sep = "\n")
  if (util_empty(desc)) {
    desc <-
      .manual$descriptions;
    if (length(desc) != 1) desc <- sprintf(
      "<i>No description found for <tt>%s</tt></i>",
      dQuote(fname));
  } else {
    if (suppressWarnings(util_ensure_suggested("markdown", err = FALSE))) {
      int <-
        markdown::markdownToHTML(int, fragment.only = TRUE)
    }
  }
  desc
}
