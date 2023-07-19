#' Generate a RMarkdown-based report from a [dataquieR] report
#'
#' @param x [dataquieR report][dq_report].
#' @param dir [character] directory to store the rendered report's files,
#'   a temporary one,
#'   if omitted. Directory will be created, if missing, files may be
#'   overwritten inside that directory
#' @param view [logical] display the report
#' @param self_contained [logical] create a single page application HTML file.
#'   This may be quite big and hard to render for your web-browser.
#' @param ... additional arguments:
#'  * template: Report template to use, not yet supported.
#'  * chunk_error: display error messages in report
#'  * chunk_warning: display warnings in report
#'  * output_format: output format to use, see [rmarkdown::render] -- currently,
#'                   html based formats are supported by the default template.
#'                   If set, the argument self_contained will be ignored.
#'  * chunk_echo: display R code in report
#'  * chunk_message: display [message] outputs in report
#'
#' @return file name of the generated report
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset <- function( # TODO: use parallel-mapping
    x,
    dir,
    view = TRUE,
    self_contained = FALSE,
    ...) {
  opts <- c(as.list(environment()), list(...))
  opts$x <- NULL
  for (arg_name in setdiff(names(formals()), "...")) {
    missing_in_parent <- eval(call("missing", as.symbol(arg_name)))
    if (missing_in_parent) {
      opts[[arg_name]] <- NULL
    }
  }
  util_ensure_suggested(pkg = c("flexsiteboard", "flexdashboard", "htmltools",
                                "digest", "DT", "rmarkdown", "knitr"),
                        goal = "generate flexdashboard-HTML-reports.")

  df_report <- as.data.frame(x)
  if (nrow(df_report) * ncol(df_report) == 0) {
    util_error("Report is empty, no results at all.")
  }

  dir <- NULL

  output_format <- NULL # fetch from document

  template <- "default"

  chunk_echo <- FALSE
  chunk_error <- FALSE
  chunk_warning <- FALSE
  chunk_message <- FALSE
  view <- TRUE

  if ("dir" %in% names(opts)) {
    dir <- opts[["dir"]]
    if (!is.character(dir) || length(dir) != 1) {
      util_error("dir must be a character(1)")
      dir <- NULL
    }
  }
  if ("template" %in% names(opts)) {
    template <- opts[["template"]]
    if (!is.character(template) || length(template) != 1)
      util_error("template must be a character(1)")
  }
  if ("view" %in% names(opts)) {
    view <- opts[["view"]]
    if (!is.logical(view) || length(view) != 1 || is.na(view))
      util_error("view must be a logical(1)")
  }
  if ("chunk_echo" %in% names(opts)) {
    chunk_echo <- opts[["chunk_echo"]]
    if (!is.logical(chunk_echo) || length(chunk_echo) != 1 || is.na(chunk_echo))
      util_error("chunk_echo must be a logical(1)")
  }
  if ("chunk_error" %in% names(opts)) {
    chunk_error <- opts[["chunk_error"]]
    if (!is.logical(chunk_error) || length(chunk_error) != 1 ||
        is.na(chunk_error))
      util_error("chunk_error must be a logical(1)")
  }
  if ("chunk_message" %in% names(opts)) {
    chunk_message <- opts[["chunk_message"]]
    if (!is.logical(chunk_message) || length(chunk_message) != 1 ||
        is.na(chunk_message))
      util_error("chunk_message must be a logical(1)")
  }
  if ("chunk_warning" %in% names(opts)) {
    chunk_warning <- opts[["chunk_warning"]]
    if (!is.logical(chunk_warning) || length(chunk_warning) != 1 ||
        is.na(chunk_warning))
      util_error("chunk_warning must be a logical(1)")
  }

  if (length(dir) == 0) {
    dir <- tempfile()
    util_message("No output directory given (with dir=), setting it to %s",
                 dQuote(dir))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  old_dir <- setwd(dir)
  on.exit({
    setwd(old_dir)
  })

  report <- x # the report object is defined here to be available in markdown
  rm(x)

  outputFile <- file.path(dir, "report")

  packageName <-
    utils::packageName(
    ) # the packageName is defined here to be available in markdown

  util_message("Compiling rmarkdown report, please wait...")

  template_file <- system.file(sprintf("%s_report.Rmd", template), package =
                              utils::packageName())

  if (template_file == "") {
    if (file.exists(template)) {
    template_file <- template
    } else {
      util_error("Template %s not found", dQuote(template))
    }
  }

  file.copy(template_file, dir)
  template_file <- file.path(dir, basename(template_file))

  if ("output_format" %in% names(opts)) { # TODO: Make all arguments explicit
    output_format <- opts[["output_format"]]
  } else if ("self_contained" %in% names(opts)) {
    self_contained <- opts["self_contained"]
    if (length(self_contained) != 1 ||
        is.na(as.logical(self_contained))) {
      util_error("The argument %s must be %s or %s, if given",
                 dQuote("self_contained"),
                 sQuote(TRUE),
                 sQuote(FALSE))
    }
    self_contained <-
      as.logical(self_contained)
    output_format <-
      rmarkdown::default_output_format(template_file)
    output_format$options$self_contained <-
      self_contained
    output_format <-
      rmarkdown::resolve_output_format(
        input = template_file,
        output_format = output_format$name,
        output_options = output_format$options)
  }

  util_setup_rstudio_job("Viewing dq_report")

  file <- rmarkdown::render(
    quiet = TRUE,
    template_file,
    output_format = output_format,
    output_dir = dir,
    output_file = outputFile,
    envir = environment()
  )

  if (endsWith(file, ".html")) {
    loading_chunk_file <-
      system.file("loading.html", package = utils::packageName())
    lines_chunk <- paste(readLines(loading_chunk_file), collapse = "\n")
    lines <- readLines(file)
    body_line <- which(lines == "<body>")
    if (length(body_line) == 1) {
      cat(
        c(lines[1:body_line],
          lines_chunk,
          lines[(body_line+1):length(lines)]),
        file = file,
        sep = "\n"
      )
    }
  }

  if (view) {
    util_view_file(file)
    invisible(file)
  } else {
    file
  }
}



# rstudioapi::jobRunScript()
