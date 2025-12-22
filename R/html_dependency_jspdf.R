#' HTML dependency for `jsPDF`
#'
#' Provides `jsPDF` for use in `Shiny` or `RMarkdown` via `htmltools`.
#'
#' @return An [htmltools::htmlDependency()] object
#' @export
html_dependency_jspdf <- function() {
  if (suppressWarnings(!util_ensure_suggested("visNetwork","PDF Downloads from Plotly Plots",
                        err = FALSE))) {
    return(htmltools::htmlDependency(
      name = "dummy",
      version = "0.0.1",
      src = ""
    ))
  }
  htmltools::htmlDependency(
    name = "jspdf",
    version = "1.3.2",
    src = c(file=system.file("htmlwidgets/lib/export/jsPDF", package="visNetwork")),
    script = "jspdf.debug.js"
  )
  # htmltools::htmlDependency(
  #   name = "jspdf",
  #   version = "2.5.1",
  #   src = system.file("jsPDF", package = "dataquieR"),
  #   script = "jspdf.umd.min.js"
  # )
}
