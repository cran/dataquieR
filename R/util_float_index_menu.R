#' return a single page navigation menu floating on the right
#'
#' if displayed in a `dq_report2`
#'
#' @param index_menu_table [data.frame] columns: links, hovers, texts
#' @param object `htmltools` tag list, used, instead of `index_menu_table`,
#'               if passed
#'
#' @examples
#' \dontrun{
#' util_float_index_menu(tibble::tribble(
#'    ~ links, ~ hovers, ~ texts,
#'    "http://www.google.de/#xxx", "This is Google", "to Google",
#'    "http://www.uni-giessen.de/#xxx", "This is Gießen", "cruising on the A45"
#' ))
#' }
util_float_index_menu <- function(index_menu_table, object) {
  if (missing(object)) {
    object <- do.call(htmltools::tagList,
                      mapply(SIMPLIFY = FALSE,
                             link = index_menu_table$links,
                             hover = index_menu_table$hovers,
                             text = index_menu_table$texts,
                             FUN = function(link, hover, text) {
                               htmltools::tags$li(
                                 htmltools::a(href = link, title = hover, text)
                               )
                             })
    )
  } else {
    util_stop_if_not(missing(index_menu_table))
  }
  if (!!length(object)) {
    htmltools::div(
      class = "floatbar",
      htmltools::tags$i(class = "fas fa-bars"),
      htmltools::tags$ul(
        class = "floatmenu",
        object
      )
    )
  } else {
    NULL
  }
}
