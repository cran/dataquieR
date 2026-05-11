util_get_concept_links <- function(fkt) {

  # generates pages with navigation menu
  # load concept to get current indicator links in reports

  concept <- util_get_concept_info("dqi")
  concept <- concept[c("PublicID", "Name", "function_R")]

  fkt2concept <- subset(
    concept,
    get("function_R") == fkt
  )

  # create link tags
  get_links <- function(indicator_id, indicator_name) {
    htmltools::tags$a(
      target = "_blank",
      href = paste(
        "https://dataquality.qihs.uni-greifswald.de/id/#",
        indicator_id,
        sep = ""
      ),
      indicator_name
    )
  }

  # create un-ordered item list for each indicator
  links <- mapply(
    get_links,
    indicator_id   = fkt2concept$PublicID,
    indicator_name = fkt2concept$Name,
    SIMPLIFY = FALSE
  )
  names(links) <- NULL

  items <- lapply(links, htmltools::tags$li)

  htmltools::tags$ul(items)
}
