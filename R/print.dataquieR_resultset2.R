#' Generate a RMarkdown-based report from a [dataquieR] report
#'
#' @param x [dataquieR report v2][dq_report2].
#' @param dir [character] directory to store the rendered report's files,
#'   a temporary one,
#'   if omitted. Directory will be created, if missing, files may be
#'   overwritten inside that directory
#' @param view [logical] display the report
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param ... additional arguments:
#'
#' @return file names of the generated report's HTML files
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset2 <- function( # TODO: use parallel-mapping
  x,# TODO: progress bar
  dir,
  view = TRUE,
  disable_plotly = FALSE,
  ...) {
  opts <- c(as.list(environment()), list(...))
  opts$x <- NULL
  for (arg_name in setdiff(names(formals()), "...")) {
    missing_in_parent <- eval(call("missing", as.symbol(arg_name)))
    if (missing_in_parent) {
      opts[[arg_name]] <- NULL
    }
  }
  util_ensure_suggested(pkg = c("htmltools",
                                "digest", "DT", "rmarkdown",
                                "markdown"),
                        goal = "generate plain HTML-reports.")

  if (nrow(x) * ncol(x) * nres(x) == 0) {
    util_error("Report is empty, no results at all.")
  }

  dir <- NULL

  template <- "default"

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

  util_setup_rstudio_job("Rendering dq_report2 to HTML...")
  start_time <- Sys.time()

  if (length(dir) == 0) {
    dir <- tempfile()
    util_message("No output directory given (with dir=), setting it to %s",
                 dQuote(dir))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir)
  old_dir <- setwd(dir)
  on.exit({
    setwd(old_dir)
  }, add = TRUE)

  report <- x # the report object is defined here to be available
  rm(x)

  packageName <- utils::packageName()

  if (!inherits(attr(report, "min_render_version"), "numeric_version") ||
      attr(report, "min_render_version") > as.numeric_version("1.0.0")
  ) {
    util_warning(
      c("This report may be not rendered correctly by",
        "this version of %s (i.e., %s). I'll try, but consider",
        "upgradig %s, please:\n install.packages(\"%s\")"),
      packageName,
      as.character(packageVersion(packageName)),
      packageName,
      packageName
      )
  }

  util_message("Compiling HTML report, please wait...")


  template_file <- system.file("templates", template, "report.html", package =
                                 packageName)

  if (template_file == "") {
    if (file.exists(template)) {
      template_file <- template
    } else {
      util_error("Template %s not found", dQuote(template))
    }
  }

  file.copy(system.file("logos",
                        "dataquieR_48x48.png",
                        package = packageName),
            file.path(dir, "logo.png"))

  pages <- util_generate_pages_from_report(report, template,
                                           disable_plotly = disable_plotly,
                                           progress = progress,
                                           progress_msg = progress_msg)

  logo <- "logo.png"
  loading <-
    htmltools::includeHTML(
      system.file("loading.html",
                  package =
                    packageName))

  deps_prepro <- util_copy_all_deps(dir = dir,
                                    pages,
                                    rmarkdown::html_dependency_jquery(),
                                    htmltools::htmlDependency(
                                      name = "clipboard",
                                      version = "2.0.11",
                                      src = system.file("clipboard", package = packageName),
                                      script = c("clipboard.min.js")
                                    ),
                                    htmltools::htmlDependency(
                                        name = "tippy",
                                        version = "6.7.3",
                                        src = system.file("tippy", package = packageName),
                                        script = c("core.js", "tippy.js")
                                      ),
                                      rmarkdown::html_dependency_font_awesome(),
                                      htmltools:: htmlDependency("menu", "1.0.0",
                                                                 src = system.file("menu", package =
                                                                                   packageName),
                                                                 stylesheet = "style.css",
                                                                 script = "script.js")
  )

  fnms <- lapply(
    setNames(nm = names(pages)),
    util_create_page_file,
    pages = pages,
    rendered_pages = deps_prepro$rendered_pages,
    dir = dir,
    template_file = template_file,
    report = report,
    logo = logo,
    loading = loading,
    packageName = packageName,
    deps = deps_prepro$deps
  )

  end_time <- Sys.time()

  cat(sep = "",
    "{\"renderingTime\": \"",
    paste(as.character(round(difftime(end_time, start_time, units = "mins"), 2)),
          "min"),
    "\"}",
    file = file.path(dir, "renderinfo.json")
    )

  util_message("Wrote %s", paste0(dQuote(fnms), collapse = ", "))

  if (view) {
    util_view_file(fnms[[1]])
    invisible(fnms)
  } else {
    fnms
  }
}
