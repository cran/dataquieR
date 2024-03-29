#' HTML Dependency for `dataquieR`
#'
#' generate all dependencies used in static `dataquieR` reports
#'
#' @param iframe [logical]`(1)` if `TRUE`, create the dependency used in figure
#' `iframes`.
#'
#' @return the dependency
html_dependency_dataquieR <- function(iframe = FALSE) {
  util_expect_scalar(iframe, check_type = is.logical)
  if (iframe) {
    htmltools:: htmlDependency("menu", "1.0.1",
                               src = system.file("menu", package =
                                                   "dataquieR"),
                               stylesheet = c("style.css", "style_iframe.css"),
                               script = c("script.js", "script_iframe.js"))
  } else {
    htmltools:: htmlDependency("menu", "1.0.1",
                               src = system.file("menu", package =
                                                   "dataquieR"),
                               stylesheet = c("style.css", "style_toplevel.css"),
                               script = c("script.js", "script_toplevel.js"))
  }
}
