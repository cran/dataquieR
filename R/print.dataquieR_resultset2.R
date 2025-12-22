#' Generate a HTML-based report from a [dataquieR] report
#'
#' @param x [dataquieR report v2][dq_report2].
#' @param dir [character] directory to store the rendered report's files,
#'   a temporary one,
#'   if omitted. Directory will be created, if missing, files may be
#'   overwritten inside that directory
#' @param view [logical] display the report
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param block_load_factor [numeric] see [dataquieR.print_block_load_factor]
#' @param advanced_options [list] options to set during report computation,
#'                                see [options()]
#' @param ... additional arguments:
#' @param dashboard [logical] dashboard mode: `TRUE`: create a dashboard only,
#'                            `FALSE`: don't create a dashboard at all,
#'                            `NA` or missing: create a "normal" report with
#'                            a dashboard included.
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for [parallelMap::parallelStart] or NULL,
#'                        if parallel has already been started by the caller.
#'                        Can also be a cluster.
#'
#' @return file names of the generated report's HTML files
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset2 <- function(
    x,
    dir,
    view = TRUE,
    disable_plotly = FALSE,
    block_load_factor = getOption("dataquieR.print_block_load_factor",
                                  dataquieR.print_block_load_factor_default),
    advanced_options =  list(),
    dashboard = NA,
    ...,
    cores = list(mode = "socket",
                 logging = FALSE,
                 cpus = util_detect_cores(),
                 load.balancing = TRUE)) {

  util_ensure_suggested("parallel")


  util_expect_scalar(block_load_factor,
                     check_type =
                       util_is_numeric_in(min = 1, whole_num = FALSE),
                     convert_if_possible = as.numeric,
                     error_message =
                       sprintf(
                         "%s should be a number above 0",
                         sQuote("block_load_factor")))

  if (suppressWarnings(util_ensure_suggested("testthat", err = FALSE))) {
    if (testthat::is_testing()) {
      if (missing(cores)) {
        cores <- 2
      }
      if (!rlang::is_scalar_integerish(cores) ||
          cores > 2L) {
        if (!is.null(cores))
          util_warning(
            "Internal problem: %s should be an integer below %s in the context of %s",
            sQuote("cores"), dQuote("3"), sQuote("testthat"))
          cores <- 2
      }
    }
  }

  if (!is.null(cores)) {
    if (inherits(cores, "cluster")) {
      old_def_cl <- parallel::getDefaultCluster()
      parallel::setDefaultCluster(cores)
      on.exit(parallel::setDefaultCluster(old_def_cl), add = TRUE)
      old_o_def_cl <- options(
        parallelMap.cpus = length(parallel::getDefaultCluster()),
        parallelMap.load.balancing = TRUE,
        parallelMap.mode = "socket"
      )
      on.exit(options(old_o_def_cl), add = TRUE)
    } else if (inherits(cores, "list")) {
      suppressMessages(do.call(parallelMap::parallelStart, cores))
      on.exit(suppressMessages(parallelMap::parallelStop()), add = TRUE)
      on.exit(suppressMessages(gc(full = TRUE)), add = TRUE)
    } else {
      suppressMessages(parallelMap::parallelStart("socket", cpus = cores,
                                                  logging = FALSE,
                                                  load.balancing = TRUE))
      on.exit(suppressMessages(parallelMap::parallelStop()), add = TRUE)
      on.exit(suppressMessages(gc(full = TRUE)), add = TRUE)
    }
    cores <- NULL
  }
  util_stop_if_not(is.list(advanced_options))
  old_O <- options(
    c(
      list(
        dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
        dataquieR.ERRORS_WITH_CALLER = FALSE,
        dataquieR.MESSAGES_WITH_CALLER = FALSE,
        dataquieR.WARNINGS_WITH_CALLER = FALSE,
        DT.warn.size = FALSE
      ),
      advanced_options
    )
  )
  .old_.dq2_globs_.called_in_pipeline <- .dq2_globs$.called_in_pipeline
  .dq2_globs$.called_in_pipeline <- TRUE
  on.exit({
    .dq2_globs$.called_in_pipeline <- .old_.dq2_globs_.called_in_pipeline
    options(old_O)
  }, add = TRUE)

  opts <- c(as.list(environment()), list(...))
  opts$x <- NULL
  for (arg_name in setdiff(names(formals()), "...")) {
    missing_in_parent <- eval(call("missing", as.symbol(arg_name)))
    if (missing_in_parent) {
      opts[[arg_name]] <- NULL
    }
  }
  util_ensure_suggested(pkg = c("htmltools",
                                "DT", "rmarkdown",
                                "markdown"),
                        goal = "generate plain HTML-reports.")

  if (nrow(x) * ncol(x) * nres(x) == 0) {
    all_errors <- lapply(x, function(res) {
      e <- attr(res, "error")
      if (length(e)) {
        stopifnot(length(e) == 1)
        e <- e[[1]]
        if (!is.null(attr(e, "applicability_problem")) &&
            is.logical(attr(e, "applicability_problem")) &&
            identical(attr(e, "applicability_problem"), TRUE)) {
          "Applicability problem(s)"
        } else {
          paste(conditionMessage(e), collapse = "\n") # nocov
        }
      } else {
        NULL
      }
    })
    all_errors <- all_errors[!vapply(all_errors, is.null, FUN.VALUE =
                                       logical(1))]
    all_errors <- unlist(all_errors, recursive = TRUE)
    all_errors <- gsub("\nwhen calling .*$", "", all_errors)
    all_errors <- unique(all_errors)
    if (length(all_errors) > 0)
      reason <- paste0(", possible reasons are: \n", paste("-", all_errors, collapse = "\n"))
    else
      reason <- "."
    util_error("Report is empty, no results at all%s",
               reason)
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
  if ("by_report" %in% names(opts)) {
    by_report <- opts[["by_report"]]
    if (!is.logical(by_report) || length(by_report) != 1 || is.na(by_report))
      util_error("by_report must be a logical(1)")
  } else {
    by_report <- FALSE
  }


  util_setup_rstudio_job("Rendering dq_report2 to HTML...",
                         n = 2 * nrow(x) * ncol(x))
  start_time <- Sys.time()

  if (length(dir) == 0) {
    dir <- tempfile()
    util_message("No output directory given (with dir=), setting it to %s",
                 dQuote(dir))
  }

  content_dir <- dir
  content_file <- file.path(content_dir, "index.html")
  dir <- file.path(dir, ".report")

  if (file.exists(dir) && !getOption("dataquieR.resume_print",
                                     dataquieR.resume_print_default)) {
    util_error("%s already exists, cannot use this as an output folder.",
               dQuote(dir))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir)
  old_dir <- setwd(dir)
  on.exit({
    setwd(old_dir)
  }, add = TRUE)


  old_opt <- options(DT.warn.size=FALSE)
  on.exit({
    options(old_opt)
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

  if (is.na(dashboard) || dashboard) { # DASHBOARD
    progress_msg("Computing dashboard", "Computing dashboard...start")
    my_dashboard <- util_setup_dashboard(report, make_links = is.na(dashboard))
    progress_msg("Computing dashboard...", "Computing dashboard...done")
  } else {
    my_dashboard <- NULL
  }

  if (!is.na(dashboard) && dashboard) {
    progress_msg("Writing dashboard...", "Writing dashboard...start")
    htmltools::save_html(my_dashboard,
                         libdir = file.path(content_dir, "lib"),
                         file = content_file)
    progress_msg("Writing dashboard...", "Writing dashboard...done")
    fnms <- setNames(list(content_file), nm = paste0("dash-", title))

  } else {
    progress_msg("Creating HTML files in memory...",
                 "Creating HTML files in memory...start")
    pages <- util_generate_pages_from_report(report, template,
                                             disable_plotly = disable_plotly,
                                             progress = progress,
                                             progress_msg = progress_msg,
                                             block_load_factor =
                                               block_load_factor,
                                             dir = dir,
                                             my_dashboard = my_dashboard)
    progress_msg("Creating HTML files in memory...",
                 "Creating HTML files in memory...finalize")

    all_ids <- util_extract_all_ids(pages)
    cat(sep = "",
        "window.all_ids = {\"all_ids\": ",
        paste0("[",
               paste0('"', all_ids, '"', collapse = ", "),
               "]"),
        "}",
        file = file.path(dir, "anchor_list.js")
    )
    saveRDS(object = all_ids, file = file.path(dir, "anchor_list.RDS"))

    logo <- "logo.png"
    loading <-
      htmltools::includeHTML(
        system.file("loading.html",
                    package =
                      packageName))

    jqui <- rmarkdown::html_dependency_jqueryui()
    jqui$stylesheet <- "jquery-ui.min.css"

    deps_prepro <- util_copy_all_deps(dir = dir,
                                      pages,
                                      rmarkdown::html_dependency_jquery(),
                                      jqui,
                                      html_dependency_clipboard(),
                                      html_dependency_tippy(),
                                      rmarkdown::html_dependency_font_awesome(),
                                      html_dependency_dataquieR(),
                                      html_dependency_jspdf()
    )

    title <- attr(report, "title")
    if (is.null(title)) {
      title <- "Data Quality Report"
    }

    progress_msg("Creating HTML files in memory...",
                 "Creating HTML files in memory...done")

    progress_msg("Writing HTML files from memory...",
                 "Writing HTML files from memory...start")

    fnms <- lapply(
      setNames(nm = seq_along(pages)),
      util_create_page_file,
      progress_msg = progress_msg,
      progress = progress,
      pages = pages,
      rendered_pages = deps_prepro$rendered_pages,
      dir = dir,
      template_file = template_file,
      report = report,
      logo = logo,
      loading = loading,
      packageName = packageName,
      deps = deps_prepro$deps,
      title = title,
      by_report = by_report
    )

    util_setup_rstudio_job("Page generation: Finalization",
                           length(pages))

    progress_msg("Writing HTML files from memory...",
                 "Writing HTML files from memory...thumbnails/iframes")

    util_write_iframe_results(pages,
                              progress_msg = progress_msg,
                              progress = progress,
                              block_load_factor = block_load_factor,
                              template_file = system.file("templates",
                                                          template,
                                                          "iframe.html",
                                                          package =
                                                            packageName),
                              dir = dir)

    progress_msg("Creating HTML files in memory...",
                 "Writing HTML files from memory...done")

    end_time <- Sys.time()

    cat(sep = "",
        "{\"renderingTime\": \"",
        paste(as.character(round(difftime(end_time, start_time, units = "mins"), 2)),
              "min"),
        "\"}",
        file = file.path(dir, "renderinfo.json")
    )

    cat(sep = "",
        "window.renderingData = {\"renderingTime\": \"",
        util_des_functions_env$util_compute_difftime_auto(
          difftime(end_time, start_time, units = "mins")),
        "\"}",
        file = file.path(dir, "renderinfo.js")
    )

    util_hide_file_windows(dir)

    progress_msg(sprintf("Wrote %s", paste0(dQuote(fnms), collapse = ", ")),
                 sprintf("Wrote %s", paste0(dQuote(fnms), collapse = ", ")))

    f <- file(description = content_file,
              open = "w", encoding = "utf-8")
    on.exit(close(f), add = TRUE)

    cat("<html>",
        "<head>",
        "<title>",
        title,
        "</title>",
        '<meta http-equiv="refresh" content="0; URL=.report/',
        basename(fnms[[1]]),
        '">',
        "</head>",
        "<body><a href=\".report/",
        basename(fnms[[1]]),
        "\">Report</a></body></html>",
        sep = "",
        file = f)

    if (util_really_rstudio()) {
      rstudioapi::executeCommand("activateConsole")
    }
  }

  if (view) {
    util_view_file(content_file)
    invisible(fnms)
  } else {
    fnms
  }
}

#' print a list of `dataquieR_result` objects
#'
#' @param x [list()] only, if all elements inherit from [dataquieR_result],
#'          this implementation runs
#' @param ... passed to other implementations
#'
#' @return undefined
#' @export
print.list <- function(x, ...) {
  util_stop_if_not(is.list(x))
  if (length(x) == 0) {
    return(NextMethod())
  }
  if (isTRUE(getOption('knitr.in.progress'))) {
    return(NextMethod())
  }
  entities <- try(unique(
    vapply(x,
           function(dqr) attr(attr(dqr, "call"), "entity"),
           FUN.VALUE = character(1))), silent = TRUE)
  if (length(entities) == 1 &&
      !util_is_try_error(entities) &&
      length(x) > 1) {
    util_ensure_suggested("htmltools")
    f <- withr::local_tempdir(.local_envir = rlang::global_env())
    withr::local_dir(f)
    jqui <- rmarkdown::html_dependency_jqueryui()
    jqui$stylesheet <- "jquery-ui.min.css"
    entity <- attr(attr(x[[1]], "call"), "entity")
    outputs <- lapply(x, function(dqr) {
      if (is.null(dqr) || inherits(dqr, "dataquieR_NULL")) {
        return(htmltools::tagList())
      }
      cn <- attr(dqr, "cn")
      nm <- paste0(cn, ".", entity)
      if (is.null(cn)) {
        title <- ""
        cn <- "Data Quality"
        nm <- ""
      } else {
        title <-
          util_alias2caption(cn,
                             long = TRUE)
      }
      section <- htmltools::tagList(html_dependency_dataquieR(iframe = FALSE),
                                    html_dependency_jspdf(),
                                    jqui,
                                    htmltools::h2(title),
                                    util_pretty_print(dqr = dqr,
                                                      nm = nm,
                                                      is_single_var = TRUE,
                                                      use_plot_ly = (util_ensure_suggested("plotly",
                                                                                           goal = "plot interactive figures",
                                                                                           err = FALSE)),
                                                      dir = f))
    })
    outputs <- c(list(htmltools::h1(entity)),
                 outputs)
    doc <- do.call(htmltools::tagList, outputs)
    htmltools::save_html(doc, "index.html")
    # FIXME: knitinprogress
    if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                       FALSE))) {
      viewer <- getOption("viewer", utils::browseURL)
      viewer("index.html")
    }
    invisible(NULL)
  } else if (identical(
    try(all(vapply(x, function(y) inherits(y, "dataquieR_result") &&
                   inherits(y, "master_result"), FUN.VALUE = logical(1)),
            na.rm = TRUE), silent = TRUE), TRUE)) {
    ERRs <- vapply(x, FUN.VALUE = logical(1), FUN = function(y) {
      length(attr(y, "error")) > 0
    })
    if (any(ERRs)) {
      for (e in x[ERRs]) {
        for (cnd in attr(e, "error"))
          warning(cnd)
      }
    }
    x <- x[!ERRs]
    all_slots <- unique(unlist(lapply(x, names)))
    all_slots <- all_slots[vapply(all_slots,
                                  function(slot)
                                    all(vapply(x, function(dqr) slot %in%
                                                 names(dqr),
                                               FUN.VALUE = logical(1)),
                                        na.rm = TRUE),
                                  FUN.VALUE = logical(1))]
    names(x) <- paste0(rep("r.", length(x)), seq_along(x))
    nm1 <- unlist(lapply(x, attr, "function_name"))
    nm2 <- unlist(lapply(x, function(.dqr) {
      attr(attr(.dqr, "call"), "entity_name")
    }))
    if (!!length(nm1) &&
        length(nm1) == length(nm2)) {
      names(x) <- paste0(nm1, ".", nm2)
    }
    if (length(all_slots) > 0) {
      x <- lapply(setNames(nm = all_slots), function(slot) {
        cr <- util_combine_res(lapply(x, `[`, slot))
        if (length(cr) == 1) {
          r <- cr[[1]][[slot]]
          rownames(r) <- NULL
        } else {
          old_r <- cr
          r <- lapply(cr, `[[`, slot)
          if (slot == "SummaryPlot") {
            if (util_ensure_suggested("plotly",
                                      goal = "plot interactive figures",
                                      err = FALSE)) {
              as_plotlys <- unique(lapply(old_r, attr, "as_plotly"))
              as_plotly <- NULL
              if (length(as_plotlys) == 1) {
                if (!is.null(as_plotlys[[1]]) &&
                    exists(as_plotlys[[1]], mode = "function"))
                  as_plotly <- get(as_plotlys[[1]], mode = "function")
              }
              if (is.function(as_plotly)) {
                my_plots <- lapply(old_r, as_plotly)
              } else {
                my_plots <- lapply(r, util_plot_figure_plotly) # IDEA: Do we need sizing_hints, here?
              }
              my_plots <- mapply(SIMPLIFY = FALSE,
                                 p = my_plots,
                                 nm = names(my_plots), function(p, nm) {
                                   title <- paste0("", nm)
                                   title <- util_sub_string_right_from_.(nm)
                                   if (inherits(p, "plotly")) {
                                     # p <- plotly::layout(p, width = "30%", height = "30%", margin = list(b = 120))
                                     p <- plotly::config(p, responsive = TRUE, displaylogo = FALSE)
                                   }
                                   react_size <- "
                  window.dq_rs2_script_init = false;
                  window.dq_rs2_script_init_fkt = function() {
                    if (window.dq_rs2_script_init) {
                      return;
                    }
                    window.dq_rs2_script_init = true;
                    var pys = $(document).find('.js-plotly-plot')
                    for (var i = 0; i < pys.length; i++) {
                      var py = pys[i]
                      Plotly.relayout(py, {
                                    width: py.getBoundingClientRect().width,
                                    height: py.getBoundingClientRect().height
                                  })
                    }
                  }
                  $(window.dq_rs2_script_init_fkt)
                "
                                   htmltools::div(style = "float:left;width:30%;height:33%;overflow-y:hidden;",
                                                  htmltools::h2(style = "position:relative;top:0;height:0;margin:0;padding:0;z-index:9;", title),
                                                  p,
                                                  htmltools::tags$script(type = "text/javascript",
                                                                         htmltools::HTML(react_size)))
                                 })

              my_plots <- htmltools::tagList(c(my_plots,
                                               list(htmltools::br(style =
                                                                    "clear: both;"))))
              r <- my_plots
            } else {
              r <- patchwork::wrap_plots(r, ncol = 2)
            }
          }
          class(r) <- union("dataquieR_result", class(r))
        }
        r
      })
    } else {
      return(NextMethod())
    }
    class(x) <- c("master_result")
    print.master_result(x)
  } else {
    NextMethod()
  }
}

#' Find all tags with `iframe`-attributes
#'
#' @param x `html` object to search
#' @param key key to find
#'
#' @returns character vector with all found keys
#' @noRd
util_collect_tag_attrs <- function(x, key = "html_file") {
  found <- list()
  found_env <- environment()

  recurse <- function(tag) {
    if (inherits(tag, "shiny.tag.list") || inherits(tag, "list")) {
      lapply(tag, recurse)
    } else if (inherits(tag, "shiny.tag")) {
      if (!is.null(attr(tag, key))) {
        found_env$found[[length(found_env$found) + 1]] <- tag
      }
      if (!is.null(tag$children)) {
        recurse(tag$children)
      }
    }
  }

  recurse(x)
  found
}

#' Save ggplot with guaranteed inner panel size (no down-scaling)
#'
#' Saves a plot ensuring that the inner panel area is at least the requested
#' size in inches. Axes and text are scaled accordingly. If necessary, the
#' overall image is enlarged to preserve the original aspect ratio and avoid
#' shrinking. Supports complex plots with faceting or long tick labels.
#'
#' @param plot A ggplot object
#' @param filename Path to the output file
#' @param inner_width Target width (in inches) of the panel area
#' @param inner_height Target height (in inches) of the panel area
#' @param dpi Output resolution (default: 300)
#' @param base_text_factor Font scaling factor (default: 11)
#'
#' @return Nothing. Saves the file and returns invisibly.
#' @noRd
util_save_with_inner_size <- function(plot, filename,
                                      inner_width, inner_height,
                                      dpi = 300,
                                      base_text_factor = 11) {
  # Text size scaling
  min_text_pt <- 12
  max_text_pt <- 16
  scale_factor <- mean(inner_width / 6, inner_height / 4)
  base_size <- base_text_factor * scale_factor
  base_size <- max(min_text_pt, min(base_size, max_text_pt))

  # Optionally apply theme with scaled base_size
  # plot <- plot + util_theme_with_scaling_limits(base_size = base_size)

  # Convert to gtable
  g <- NULL
  try({
    g <- ggplot2::ggplotGrob(plot)
  }, silent = TRUE)

  # Grob measurement helpers (robust against nesting and gtable/zeroGrob)
  measure_grob_height_in <- function(grob) {
    if (is.null(grob) || inherits(grob, "zeroGrob")) return(0)
    if (inherits(grob, "gtable")) {
      return(sum(grid::convertHeight(grob$heights, "in", TRUE)))
    }
    if (inherits(grob, "gTree") && length(grob$children) > 0) {
      vals <- unlist(lapply(grob$children, function(child) {
        tryCatch(measure_grob_height_in(child), error = function(e) 0)
      }))
      return(max(vals, 0))
    }
    as.numeric(grid::convertHeight(grid::grobHeight(grob), "in", TRUE))
  }

  measure_grob_width_in <- function(grob) {
    if (is.null(grob) || inherits(grob, "zeroGrob")) return(0)
    if (inherits(grob, "gtable")) {
      return(sum(grid::convertWidth(grob$widths, "in", TRUE)))
    }
    if (inherits(grob, "gTree") && length(grob$children) > 0) {
      vals <- unlist(lapply(grob$children, function(child) {
        tryCatch(measure_grob_width_in(child), error = function(e) 0)
      }))
      return(max(vals, 0))
    }
    as.numeric(grid::convertWidth(grid::grobWidth(grob), "in", TRUE))
  }

  get_grob_by_name <- function(gt, name) {
    idx <- which(gt$layout$name == name)
    if (length(idx)) gt$grobs[[idx]] else NULL
  }

  # Measure axes/margins in inches
  if (is.null(g)) {
    left_margin   <- 0
    right_margin  <- 0
    top_margin    <- 0
    bottom_margin <- 0
  } else {
    left_margin   <- measure_grob_width_in(get_grob_by_name(g, "axis-l"))
    right_margin  <- measure_grob_width_in(get_grob_by_name(g, "axis-r"))
    top_margin    <- measure_grob_height_in(get_grob_by_name(g, "axis-t"))
    bottom_margin <- measure_grob_height_in(get_grob_by_name(g, "axis-b"))
  }

  # Total size including margins
  total_width  <- inner_width  + left_margin + right_margin
  total_height <- inner_height + top_margin + bottom_margin

  # Save to file
  e <- try(ggplot2::ggsave(
    filename  = filename,
    plot      = plot,
    width     = total_width,
    height    = total_height,
    dpi       = dpi,
    units     = "in",
    limitsize = FALSE
  ), silent = TRUE)

  if (util_is_try_error(e)) {
    msg <- conditionMessage(attr(e, "condition"))
    msg_stripped <- msg
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      msg_stripped <- cli::ansi_strip(msg_stripped)
    }

    e <- try(ggplot2::ggsave(
      filename  = filename,
      plot      = util_ggplot_text(msg_stripped),
      width     = total_width,
      height    = total_height,
      dpi       = dpi,
      units     = "in",
      limitsize = FALSE
    ), silent = TRUE)

    util_warning("Could not write %s: %s",
               dQuote(filename),
               sQuote(msg))
  }
  suppressMessages(gc(verbose = FALSE))
  invisible(NULL)
}

#' Internal theme with scalable font size (not exported)
#'
#' Creates a `ggplot2::theme()` object where text elements are scaled
#' proportionally to a given base font size. Font sizes for axis text, axis
#' titles, legend text, and plot titles are explicitly set, not using
#' `rel()`. The legend key size is scaled in relation to the axis text size.
#'
#' @param base_size Base font size in pt (e.g., 12)
#' @param rel_axis Relative size for axis and legend text (default: 0.65)
#' @param rel_title Relative size for axis titles and plot titles (default: 0.75)
#'
#' @return A ggplot2 theme object with consistently scaled text
#' @noRd
util_theme_with_scaling_limits <- function(base_size,
                                           rel_axis = 0.9,
                                           rel_title = 1.0) {
  ggplot2::theme(
    text            = ggplot2::element_text(size = base_size),
    axis.text       = ggplot2::element_text(size = base_size * rel_axis),
    axis.text.x     = ggplot2::element_text(size = base_size * rel_axis),
    axis.text.y     = ggplot2::element_text(size = base_size * rel_axis),
    axis.title      = ggplot2::element_text(size = base_size * rel_title),
    axis.title.x    = ggplot2::element_text(size = base_size * rel_title),
    axis.title.y    = ggplot2::element_text(size = base_size * rel_title),
    legend.text     = ggplot2::element_text(size = base_size * rel_axis),
    legend.title    = ggplot2::element_text(size = base_size * rel_title),
    legend.key.size = ggplot2::unit(rel_axis, "lines"),
    title           = ggplot2::element_text(size = base_size * rel_title)
  )
}

#' Write `iframe` HTML and thumbnails (parallel-aware)
#'
#' works serially, if no cluster runs. in parallel mode, it works
#' block-wise to show a progress bar.
#'
#' @param pages           list of shiny/HTML tags
#' @param progress_msg    `function(msg_short, msg_long)`
#' @param progress        `function(percent)`
#' @param block_load_factor [numeric] see
#'   [dataquieR.print_block_load_factor]
#' @param template_file file to use as a template for the HTML output
#' @param dir [character] directory to store the rendered report's files
#'
#' @return invisible(NULL)
#' @noRd
util_write_iframe_results <- function(
    pages,
    progress_msg,
    progress,
    block_load_factor = getOption(
      "dataquieR.print_block_load_factor",
      dataquieR.print_block_load_factor_default),
    template_file,
    dir
) {

  util_expect_scalar(
    block_load_factor,
    check_type         = util_is_numeric_in(min = 1, whole_num = FALSE),
    convert_if_possible = as.numeric,
    error_message       = sprintf("%s should be a number above 0",
                                  sQuote("block_load_factor"))
  )

  ## --- preparation ----------------------------------------------------------
  all_tags <- unlist(lapply(pages, util_collect_tag_attrs), recursive = FALSE)
  # process each output file only once
  tasks <- Filter(function(tag) !is.null(attr(tag, "html_file")), all_tags)
  outfiles <- vapply(tasks, attr, FUN.VALUE = character(1), "html_file")
  html_inners <- lapply(tasks, attr, "html_inner")

  rp <- util_copy_all_deps(dir = dir,
                           pages = html_inners)

  tasks <- Map(function(task, html_inner) {
    html_inner[["dependencies"]] <- NULL
    attr(task, "html_inner") <- html_inner
    task
  }, tasks, rp$rendered_pages)

  tasks <- tasks[!duplicated(outfiles)]

  total <- length(tasks)
  if (total == 0L) return(invisible(NULL))

  ## --- block-wise processing -------------------------------------------------
  cl         <- parallel::getDefaultCluster()
  if (!util_check_shared_filesystem(cl = cl)$shared) {
    util_message(
      c("File system not shared amoungst workers (e.g., through NFS),",
        "falling back to serial writing"))
    cl <- NULL
  }
  chunk_size <- max(1, ceiling(block_load_factor * length(cl)))
  blocks     <- split(tasks, ceiling(seq_along(tasks) / chunk_size))
  done       <- 0L

  for (chunk in blocks) {
    if (!is.null(cl)) {
      parallel::parLapplyLB(cl, chunk, util_write_one,
                            template_file = template_file, deps = rp$deps)
    } else {
      lapply(chunk, util_write_one, template_file = template_file,
             deps = rp$deps)
    }
    done <- done + length(chunk)
    progress(100 * done / total)
    progress_msg("Writing HTML/thumbnails",
                 sprintf("Progress: %d of %d", done, total))
  }

  invisible(NULL)
}

#' For a shiny tag, write all attached "FIG"-files
#'
#' @param tag the shiny tag
#' @param template_file the report template file to use
#' @param deps html-dependencies as html-tags
#'
#' @returns `invisible(NULL)`
#' @noRd
util_write_one <- function(tag, template_file, deps) {
  outfile        <- attr(tag, "html_file")
  thumb          <- attr(tag, "thumbnail_path")
  ggthumb        <- util_decompress(attr(tag, "ggthumb"))
  html_inner     <- attr(tag, "html_inner")
  thumbnail_args <- attr(tag, "thumbnail_args")

  # HTML
  logo <- "logo.png"
  withCallingHandlers({
    html_report <- htmltools::htmlTemplate(template_file,
                                           document_ = TRUE,
                                           logo = logo,
                                           deps = deps,
                                           content = html_inner,
                                           title = basename(outfile))
  },
  warning = function(cond) { # suppress a waning caused by ggplotly for barplots
    if (startsWith(conditionMessage(cond),
                   "'bar' objects don't have these attributes: 'mode'") ||
        startsWith(conditionMessage(cond),
                   "'box' objects don't have these attributes: 'mode'")) {
      invokeRestart("muffleWarning")
    }
  })


  f <- file(outfile, open = "w", encoding = "utf-8")
  on.exit(close(f))
  withCallingHandlers(
    cat(as.character(html_report), file = f),
    warning = function(w) {
      if (startsWith(conditionMessage(w),
                     "'bar' objects don't have these attributes: 'mode'") ||
          startsWith(conditionMessage(w),
                     "'box' objects don't have these attributes: 'mode'"))
        invokeRestart("muffleWarning")
    })

  # thumbnail (optional)
  figure_type_id <- thumbnail_args$figure_type_id

  if (length(figure_type_id) != 1 ||
      !is.character(figure_type_id)) {
    # FIXME: How can that be?!
    figure_type_id <- "dot_mat"
  }

  if (isTRUE(thumbnail_args$rotated)) {
    figure_type_id <-
      paste0(figure_type_id, "_rotated")
  }

  if (!is.null(ggthumb) && !file.exists(thumb) &&
      !is.null(thumbnail_args)) {
    suppressWarnings(suppressMessages(
      util_save_with_inner_size(
        plot           = ggthumb,
        filename       = thumb,
        inner_width    = thumbnail_args$width,
        inner_height   = thumbnail_args$height,
        dpi            = thumbnail_args$dpi,
        base_text_factor = switch(
          figure_type_id,
          dot_mat = 11,
          40)
      )))
  }
  invisible(NULL)
}

#' Check whether all workers see the same file-system
#'
#' @param dir [character] directory assumed to be shared **and writable**
#'                        by master and workers. Defaults to the current
#'                        working directory (`"."`).
#' @param cl `cluster` an existing `parallel` cluster **or** `NULL`. If
#'                     `NULL`, try `parallel::getDefaultCluster()` and
#'                     assume sequential mode if that fails.
#' @param cleanup [logical] remove the test-file once the check is finished
#'                          (default `TRUE`).
#' @param tmp_prefix [character] prefix for the temporary file name
#'                               (default `"fs_test_"`).
#'
#' @return *list* with three components
#'   * `shared`  `TRUE` if, and only if **all** workers confirmed the file,
#'   * `exists`  logical vector per worker,
#'   * `tmpFile` absolute path of the test-file (removed when `cleanup`).
#'
#' @family util_functions
#' @concept process
#' @noRd
util_check_shared_filesystem <- function(dir = ".",
                                         cl = NULL,
                                         cleanup = TRUE,
                                         tmp_prefix = "fs_test_") {

  ## ensure that package 'parallel' is available -----------------------------
  util_ensure_suggested("parallel")

  ## create a temporary test file on the master node -------------------------
  dir <- normalizePath(dir, mustWork = TRUE)
  testfile <- tempfile(tmp_prefix, tmpdir = dir)

  if (!file.create(testfile)) {
    util_error("Cannot create file %s", dQuote(testfile))
  }

  if (cleanup) {
    on.exit(unlink(testfile, force = TRUE), add = TRUE)
  }

  ## retrieve a running cluster, if not passed in ----------------------------
  if (is.null(cl)) {
    cl <- tryCatch(parallel::getDefaultCluster(),
                   error = function(e) NULL)
  }

  ## if no cluster is available, assume sequential execution -----------------
  if (is.null(cl)) {
    return(list(
      shared  = TRUE,
      exists  = TRUE,
      tmpFile = testfile
    ))
  }
  ## query each worker for file existence ------------------------------------
  exists_vec <- tryCatch({
    unlist(parallel::clusterCall(cl, file.exists, testfile),
           use.names = FALSE)
  }, error = function(e) {
    util_warning("clusterCall failed, sequential fallback: %s",
                 dQuote(conditionMessage(e)))
    TRUE
  })

  names(exists_vec) <- paste0("worker", seq_along(exists_vec))

  list(
    shared  = all(exists_vec),
    exists  = exists_vec,
    tmpFile = testfile
  )
}

# IDEA: For PDF output, support https://cran.r-project.org/web/packages/xmpdf/index.html
