#' View a file in most suitable viewer
#'
#' @param file the file to view
#'
#' @return
#' `invisible(file)`
util_view_file <- function(file) {
  viewer <- getOption("viewer")
  if (is.null(viewer)) { # nocov start
    # Viewer stuff cannot be tested w/o a GUI
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        (exists("viewer", asNamespace("rstudioapi"), mode = "function")) &&
        (exists("isAvailable", asNamespace("rstudioapi"), mode = "function")) &&
        rstudioapi::isAvailable()) {
      rstudioapi::viewer(file)
    } else {
      browseURL(paste0("file://", file))
    }
  } else { # nocov end
    viewer(file)
  }
  invisible(file)
}
