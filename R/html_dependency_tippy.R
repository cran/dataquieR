#' HTML Dependency for `tippy`
#'
#' @return the dependency
html_dependency_tippy <- function() {
  htmltools::htmlDependency(
    name = "tippy",
    version = "6.7.3",
    src = system.file("tippy", package = "dataquieR"),
    script = c("core.js", "tippy.js")
  )
}
