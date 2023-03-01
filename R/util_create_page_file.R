#' Create an HTML file for the [dq_report2]
#'
#' @param page the name of the page-file being created
#' @param pages list with all page-contents named by their desired file names
#' @param rendered_pages list with all rendered (`htmltools::renderTags`) page-contents named by their desired file names
#' @param template_file the report template file to use
#' @param report the output of [dq_report2]
#' @param packageName the name of the current package
#' @param dir target directory
#' @param logo logo `PNG` file
#' @param loading loading animation div
#' @param deps dependencies, as pre-processed by
#'             `htmltools::copyDependencyToDir` and
#'             `htmltools::renderDependencies`
#'
#' @return `invisible(file_name)`
util_create_page_file <- function(page,
                                  pages,
                                  rendered_pages,
                                  dir,
                                  template_file,
                                  report,
                                  logo,
                                  loading,
                                  packageName,
                                  deps) {

  util_message("Writing %s...", dQuote(page))

  util_stop_if_not(endsWith(page, ".html"))
  util_stop_if_not(!is.null(pages[[page]]))

  pg <- rendered_pages[[page]]

  pg[["dependencies"]] <- NULL

  html_report <- htmltools::htmlTemplate(template_file,
                                  document_ = TRUE,
                                  spage = pg,
                                  logo = logo,
                                  menu = .menu_env$menu(pages),
                                  loading = loading,
                                  deps = deps
                                  )

  #fix: sort reportsummarytable by first (sysmiss) and varaible column

  # https://atomiks.github.io/tippyjs/v6/all-props/

  file_name <- file.path(dir, page)

  # htmltools::save_html(html_report,
  #                      libdir = file.path(dir, "lib"),
  #                      file = file_name)

  cat(as.character(html_report), file = file_name)

  invisible(file_name)
}
