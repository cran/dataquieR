#' HTML Dependency for report headers in `clipboard`
#'
#' @return the dependency
html_dependency_clipboard <- function() {
  htmltools::htmlDependency(
    name = "clipboard",
    version = "2.0.11",
    src = system.file("clipboard", package = "dataquieR"),
    script = c("clipboard.min.js")
  )
}
