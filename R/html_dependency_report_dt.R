#' HTML Dependency for report headers in `DT::datatable`
#'
#' @return the dependency
html_dependency_report_dt <- function() {
  htmltools::htmlDependency(
    name = "report-dt-style"
    ,version = "0.0.1"
    ,src = c(file = system.file("report-dt-style", package = "dataquieR"))
    ,stylesheet = "report-dt-style.css"
    ,script = "report_dt.js"
  )
}
