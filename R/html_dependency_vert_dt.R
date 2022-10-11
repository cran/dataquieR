#' HTML Dependency for vertical headers in `DT::datatable`
#'
#' @return the dependency
html_dependency_vert_dt <- function() {
  htmltools::htmlDependency(
    name = "vertical-dt-style"
    ,version = "0.0.1"
    ,src = c(file = system.file("vertical-dt-style", package = "dataquieR"))
    ,stylesheet = "vertical-dt-style.css"
    ,script = "sort_heatmap_dt.js"
  )
}
