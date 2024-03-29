#' Make `it` scalable, if it is a figure
#'
#' this function writes figures to helper files and embeds these in a returned
#' object which is a scalable `iframe`. it does not change other objects in `it`.
#'
#' @param it [htmltools::tag()] compatible object
#' @param dir [character] output directory for potential `iframes`.
#' @param nm [character] name for the `iframe`d file, if one is created
#' @param fkt [character] function name of the indicator function that created
#'                        `ìt`.
#'
#' @return [htmltools::tag()] compatible object, maybe now in an `iframe`
#' @keywords internal
util_iframe_it_if_needed <- function(it, dir, nm, fkt) {
  functionName <- fkt
  if (inherits(it, "plotly") ||
      (inherits(it, "shiny.tag") && it$name == "img")) {
    util_expect_scalar(dir, check_type = is.character)
    util_expect_scalar(nm, check_type = is.character)
    util_stop_if_not(dir.exists(dir))

    if (!dir.exists(file.path(dir, "lib"))) {
      dir.create(file.path(dir, "lib"), showWarnings = FALSE, recursive = TRUE)
    }

#    if (inherits(it, "plotly")) {
#      it$sizingPolicy$defaultHeight <- "100%" # seems sometimes not to work?! now done by an !important style
#    }


    fig_framed_file <- paste0("FIG_", prep_link_escape(nm, html = TRUE),
                              ".html")
    if (!file.exists(fig_framed_file)) { # FIXME: right click, ... fix JS; https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage to make buttons work
      it <- htmltools::tagList(
        rmarkdown::html_dependency_jquery(),
        htmltools::tags$span(`data-nm` = nm, id = "nm"),
        htmltools::tags$span(`data-functionName` = functionName,
                             id = "functionName"),
        html_dependency_dataquieR(iframe = TRUE),
        it
      )
      suppressWarnings(htmltools::save_html(html = it,
                           file = fig_framed_file,
                           libdir = file.path(dir, "lib")))
    }

    it <- htmltools::div(
      style = htmltools::css(
        width = "480px",
        height = "270px",
        min.width = "320px",
        min.height = "180px",
        resize = "both",
        overflow = "auto", # hidden does not work with Safar, which does not show the resize handle, then.
        border = "1px"
      ),
      htmltools::tags$iframe(
        src = fig_framed_file,
        style = htmltools::css(
          border = "0",
          width = "100%",
          height = "calc(100% - 5px)"
        )
      )
    )
    return(it)
  } else {
    return(it)
  }
}
