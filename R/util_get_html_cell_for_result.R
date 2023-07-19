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
                                                "issue", "anamat", "indicator_or_descriptor"),
                                     rn,
                                     cn,
                                     ...) {
  util_ensure_suggested("htmltools", "Generating nice tables")

  aspect <- util_match_arg(aspect, several_ok = FALSE)

  icon <- list(
    grey   = htmltools::HTML("&nbsp;"),
    reddishgrey = htmltools::HTML("&nbsp;"),
    green  = htmltools::HTML("&nbsp;"),
    yellow = htmltools::HTML("&nbsp;"),
    red    = htmltools::HTML("&nbsp;"),
    white  = htmltools::HTML("&nbsp;")
  )

  color <- c(
    grey   = "#cccccc",
    reddishgrey = "#F4A582",
    green  = "#92c5de",
    washedoutgreen = "#92c5de",
    yellow = "#cccccc",
    red    = "#f4a582",
    white  = "#FFFFFF"
  )

  # if (aspect == "anamat") {
  #   color[["green"]] <- "#2166AC"; # blue
  # }

  order_of <- c (
    grey  = 100,
    reddishgrey = 100,
    green = 30,
    washedoutgreen = 25,
    yellow = 100,
    red = 10,
    white = 200
  )

  filter_of = c( # TODO: Use speaking names for the left side of the assignments, i.e., do not return grey from util_get_color_for_result but NA, ok, warn, error or so
    grey   = "grey",
    reddishgrey = "red",
    green  = "blue",
    washedoutgreen = "blue",
    yellow = "grey",
    red    = "red",
    white  = "grey"
  )

  if (aspect == "issue") {
    color <- c(
      grey   = "#cccccc",
      reddishgrey = "#cccccc",
      green  = "#92c5de",
      washedoutgreen = "#92c5de",
      yellow = "#cccccc",
      red    = "#f4a582",
      white  = "#FFFFFF"
    )
  } else if (aspect == "applicability") {
    color <- c(
      grey   = "#cccccc",
      reddishgrey = "#F4A582",
      green  = "#92c5de",
      washedoutgreen = "#92c5de",
      yellow = "#cccccc",
      red    = "#f4a582",
      white  = "#FFFFFF"
    )
  }

  if (!inherits(result, "dataquieR_result")) {
    cl <- "white"
    ln <- util_generate_anchor_link(rn, cn, title = icon[[cl]])
    ln$attribs$style <- c(ln$attribs$style, "text-decoration:none;display:block;")
    r <- as.character(htmltools::pre(style = sprintf("height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer;", color[cl]),
                              sort = order_of[cl],
                              filter = filter_of[cl],
                              title = "No result available",
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
  as.character(htmltools::pre(style = sprintf("height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer;", color[cl]),
                  sort = order_of[cl],
                  filter = filter_of[cl],
                  title = htmltools::tagList(htmltools::h5(paste(util_alias2caption(cn, long = TRUE), "of", rn)),
                                             ms),
                  ln))
}
