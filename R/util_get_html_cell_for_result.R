#' Return the html summary table cell for a result
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#' @param rn row name for links inside a `dq_report2` report
#' @param cn column name for links inside a `dq_report2` report
#' @param ... not used
#'
#' @return [character]`(1)` `html` result for a `DT` cell
util_get_html_cell_for_result <- function(result,
                                     aspect = c("applicability",
                                                "error",
                                                "issue"),
                                     rn,
                                     cn,
                                     ...) {
  util_ensure_suggested("htmltools", "Generating nice tables")
  icon <- list(
    grey   = htmltools::HTML("&nbsp;"),
    reddishgrey = htmltools::HTML("&nbsp;"),
    green  = htmltools::HTML("&nbsp;"),
    yellow = htmltools::HTML("&nbsp;"),
    red    = htmltools::HTML("&nbsp;"),
    white  = htmltools::HTML("&nbsp;")
  )

  color <- c(
    grey   = "#B0B0B0",
    reddishgrey = "#F4A582",
    green  = "#009E73",
    yellow = "#F0E442",
    red    = "#B2182B",
    white  = "#FFFFFF"
  )

  if (aspect == "anamat") {
    color[["green"]] <- "#2166AC"; # blue
  }

  order_of <- c (
    grey  = 100,
    reddishgrey = -1,
    green = 30,
    yellow = 20,
    red = 10,
    white = 200
  )

  filter_of = c(
    grey   = "grey",
    reddishgrey = "red",
    green  = "green",
    yellow = "yellow",
    red    = "red",
    white  = "grey"
  )
  aspect <- util_match_arg(aspect, several_ok = FALSE)
  if (!inherits(result, "dataquieR_result")) {
    cl <- "white"
    ln <- util_generate_anchor_link(rn, cn, title = icon[[cl]])
    ln$attribs$style <- c(ln$attribs$style, "text-decoration:none;display:block;")
    r <- as.character(htmltools::p(style = sprintf("height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer;", color[cl]),
                              sort = order_of[cl],
                              filter = filter_of[cl],
                              title = "N/A",
                              ln))
    return(r)
  }
  ms <- util_get_message_for_result(result = result,
                              aspect = aspect,
                              collapse = function(msgs) {
                                if (length(msgs) > 0) {
                                  htmltools::tags$ul(
                                    Map(htmltools::tags$li, msgs)
                                  )
                                } else {
                                  ""
                                }
                              })
  cl <- util_get_color_for_result(result = result,
                             aspect = aspect)

  ln <- util_generate_anchor_link(rn, cn, title = icon[[cl]])
  ln$attribs$style <- c(ln$attribs$style, "text-decoration:none;display:block;")

  # text-decoration: none;
  as.character(htmltools::p(style = sprintf("height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer;", color[cl]),
                  sort = order_of[cl],
                  filter = filter_of[cl],
                  title = htmltools::tagList(htmltools::h5(paste(cn, "of", rn)),
                                             ms),
                  ln))
}
