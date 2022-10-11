#' Generate a RMarkdown-based report from a [dataquieR] report
#'
#' @param x [dataquieR report][dq_report].
#' @param ... additional arguments:
#'  * template: Report template to use, not yet supported.
#'  * chunk_error: display error messages in report
#'  * chunk_warning: display warnings in report
#'  * output_format: output format to use, see [rmarkdown::render] -- currently,
#'                   html based formats are supported by the default template.
#'  * chunk_echo: display R code in report
#'  * chunk_message: display [message] outputs in report
#'
#' @return file name of the generated report
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset <- function(x, ...) {

  util_ensure_suggested(pkg = c("flexdashboard", "htmltools", "digest",
                                "DT", "rmarkdown", "knitr"),
                        goal = "generate flexdashboard-HTML-reports.")

  df_report <- as.data.frame(x)
  if (nrow(df_report) * ncol(df_report) == 0) {
    util_error("Report is empty, no results at all.")
  }

  dir <- tempfile()
  dir.create(dir)

  output_format <- NULL # fetch from document

  template <- "default"

  chunk_echo <- FALSE
  chunk_error <- FALSE
  chunk_warning <- FALSE
  chunk_message <- FALSE

  opts <- list(...)

  if ("template" %in% names(opts)) {
    template <- opts[["template"]]
    if (!is.character(template) || length(template) != 1)
      util_error("template must be a character(1)")
  }
  if ("chunk_echo" %in% names(opts)) {
    chunk_echo <- opts[["chunk_echo"]]
    if (!is.logical(chunk_echo) || length(chunk_echo) != 1)
      util_error("chunk_echo must be a logical(1)")
  }
  if ("chunk_error" %in% names(opts)) {
    chunk_error <- opts[["chunk_error"]]
    if (!is.logical(chunk_error) || length(chunk_error) != 1)
      util_error("chunk_error must be a logical(1)")
  }
  if ("chunk_message" %in% names(opts)) {
    chunk_message <- opts[["chunk_message"]]
    if (!is.logical(chunk_message) || length(chunk_message) != 1)
      util_error("chunk_message must be a logical(1)")
  }
  if ("chunk_warning" %in% names(opts)) {
    chunk_warning <- opts[["chunk_warning"]]
    if (!is.logical(chunk_warning) || length(chunk_warning) != 1)
      util_error("chunk_warning must be a logical(1)")
  }
  if ("output_format" %in% names(opts)) {
    output_format <- opts[["output_format"]]
  }

  report <- x # the report object is defined here to be available in markdown
  rm(x)

  outputFile <- file.path(dir, "report")

  packageName <-
    utils::packageName(
    ) # the packageName is defined here to be available in markdown

  message("Compiling rmarkdown report, please wait...")

  template_file <- system.file(sprintf("%s_report.Rmd", template), package =
                              utils::packageName())

  if (template_file == "") {
    if (file.exists(template)) {
    template_file <- template
    } else {
      util_error("Template %s not found", dQuote(template))
    }
  }

  rstudiojob <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    try({
      rstudiojob = rstudioapi::jobAdd("Viewing dq_report",
                                      progressUnits = 100L)
    }, silent = TRUE)
  }

  on.exit({
    if (!is.null(rstudiojob)) { # nocov start
      try({
        rstudioapi::jobRemove(rstudiojob)
      }, silent = TRUE)
    } # nocov end
  })

  progress <- function(percent) {
    if (!is.null(rstudiojob)) { # nocov start
      try({
        rstudioapi::jobSetProgress(rstudiojob, percent)
      }, silent = TRUE)
    } # nocov end
  }

  file <- rmarkdown::render(
    quiet = TRUE,
    template_file,
    output_format = output_format,
    output_dir = dir,
    output_file = outputFile,
    envir = environment()
  )


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



# rstudioapi::jobRunScript()
