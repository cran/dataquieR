#' Create an HTML file for the [dq_report2]
#'
#' @param page_nr the number of the page being created
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
#' @param progress_msg [closure] to call with progress information
#' @param progress [closure] to call with progress information
#' @param title [character] the web browser's window name
#' @param by_report [logical] this report html is part of a set of reports,
#'                            add a back-link
#'
#' @return `invisible(file_name)`
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_create_page_file <- function(page_nr,
                                  pages,
                                  rendered_pages,
                                  dir,
                                  template_file,
                                  report,
                                  logo,
                                  loading,
                                  packageName,
                                  deps,
                                  progress_msg,
                                  progress,
                                  title,
                                  by_report) {

  page <- names(pages)[page_nr] # the name of the page-file being created

  file_name <- file.path(dir, page)

  if (getOption("dataquieR.resume_print", dataquieR.resume_print_default) &&
       util_is_html_file_complete(file_name)) {
    progress(page_nr / length(pages) * 100)
    return(invisible(file_name))
  }

  # util_message("Writing %s...", dQuote(page))
  progress_msg("", sprintf("Writing %s...", dQuote(page)))

  util_stop_if_not(endsWith(page, ".html"))
  util_stop_if_not(!is.null(pages[[page]]))

  pg <- rendered_pages[[page]]

  pg[["dependencies"]] <- NULL

  if (by_report) {
    backlink <- htmltools::div(
      style = htmltools::css(
        font.size = "18px",
        font.weight = "bold",
        position = "fixed",
        right = "0",
        top = "125px"#,
        #border =  paste("6px", "solid", "#6495ED")
      ),
      htmltools::a(
        href = "#",
        onclick =
          'window.location.href = "../../index.html"',
        "Back to reports' overview"
      )
    )
  } else {
    backlink <- NULL
  }

  if (!is.null(attr(report, "title")) &&
      !isTRUE(attr(attr(report, "title"), "default"))) {
    header_text <- attr(report, "title")
    if (!is.null(attr(report, "subtitle")) &&
        !isTRUE(attr(attr(report, "subtitle"), "default"))) {
      header_text <- paste0(header_text, ": ", attr(report, "subtitle"))
    }
  } else {
    header_text <- NULL
  }

  if (!is.null(header_text)) {
    header <- htmltools::tagList(
      htmltools::p(class = "dq-title",
                   htmltools::tags$a(href = "report.html", header_text))
    )
  } else {
    header <- NULL
  }

  html_report <- htmltools::htmlTemplate(template_file,
                                  document_ = TRUE,
                                  spage = pg,
                                  logo = logo,
                                  menu = .menu_env$menu(pages),
                                  loading = loading,
                                  deps = deps,
                                  title = title,
                                  backlink = backlink,
                                  header = header)

  #fix: sort reportsummarytable by first (sysmiss) and varaible column

  # https://atomiks.github.io/tippyjs/v6/all-props/

  # htmltools::save_html(html_report,
  #                      libdir = file.path(dir, "lib"),
  #                      file = file_name)

  f <- file(description = file_name , open = "w", encoding = "utf-8")
  on.exit(close(f))

  withCallingHandlers({
    cat(as.character(html_report), file = f)
    # TODO: [html_files_should_also_work_alone] ack and remove RDS -- then check, if html exists, really in util_html_for_*

  },
  warning = function(cond) { # suppress a waning caused by ggplotly for barplots
    if (startsWith(conditionMessage(cond),
                   "'bar' objects don't have these attributes: 'mode'") ||
        startsWith(conditionMessage(cond),
                   "'box' objects don't have these attributes: 'mode'")) {
      invokeRestart("muffleWarning")
    }
  })

  progress(page_nr / length(pages) * 100)

  invisible(file_name)
}
