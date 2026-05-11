#' Generate a HTML-based report from a [dataquieR] report
#'
#' @param x [dataquieR report v2][dq_report2].
#' @param dir [character] directory to store the rendered report's files,
#'   a temporary one,
#'   if omitted. Directory will be created, if missing
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
#' @param force_overwrite [logical] force to overwrite `dir`, even if it exists
#'
#' @return file names of the generated report's HTML files
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset2 <- function(
    x,
    dir, # TODO: consolidate dir or output_dir see dq_report2 and dq_report_by
    view = TRUE,
    disable_plotly = FALSE,
    block_load_factor = getOption("dataquieR.print_block_load_factor",
                                  dataquieR.print_block_load_factor_default),
    advanced_options =  list(),
    dashboard = NA,
    force_overwrite = FALSE,
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
      on.exit({Sys.sleep(2);suppressMessages(parallelMap::parallelStop());}, add = TRUE) # whyever, rstudio needs these two seconds, it hangs, otherwise.
      on.exit({suppressMessages(gc(full = TRUE));}, add = TRUE)
    } else {
      suppressMessages(parallelMap::parallelStart("socket", cpus = cores,
                                                  logging = FALSE,
                                                  load.balancing = TRUE))
      on.exit({Sys.sleep(2);suppressMessages(parallelMap::parallelStop());}, add = TRUE) # whyever, rstudio needs these two seconds, it hangs, otherwise.
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
        util_stop_if_not(length(e) == 1)
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

  content_dir <- util_normalize_path(dir)
  content_file <- file.path(content_dir, "index.html")

  progress_msg("Page generation", sprintf("Writing to %s", dQuote(content_file)))

  dir <- file.path(dir, ".report")

  util_expect_scalar(force_overwrite, check_type = is.logical,
                     error_message = sprintf("%s must be a logical value",
                                             sQuote("force_overwrite")))

  if (!getOption("dataquieR.resume_print", dataquieR.resume_print_default)) {
    util_overwrite_if_requested(dir, force_overwrite)
  } else {
    if (!dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
      util_error("Could not create %s", dQuote(dir))
    }
  }

  dir <- util_normalize_path(dir)
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

  # rep_id <- attr(report, "properties")$rep_id
  rep_id <- util_make_report_id() # use a different ID for each rendering

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

  if (!is.null(attr(report, "translation_version"))) {
    if (translation_version != attr(report, "translation_version")) {
      util_warning(
        c("This report may be not rendered correctly by",
          "this version of %s. The translations may have changed.",
          "Version in report: %s, version in installed %s: %s"),
        sQuote(packageName),
        sQuote(attr(report, "translation_version")),
        sQuote(paste(packageName, as.character(packageVersion(packageName)))),
        sQuote(translation_version)
      )
    }
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

  title <- attr(report, "title")
  if (is.null(title)) {
    title <- "Data Quality Report"
  }

  .hi <- .hp <- .hm <- NULL
  content_file <- NULL
  if (!missing(dir)) {
    content_file <- file.path(dirname(dir), "index.html")
    list2env(util_init_html_progress(output_dir = content_dir,
                            content_file = content_file,
                            title = title,
                            view = view,
                            rep_id = rep_id), envir = environment())
  }

  if (is.na(dashboard) || dashboard) { # DASHBOARD
    progress_msg("Computing dashboard", "Computing dashboard...start")
    my_dashboard <- util_setup_dashboard(report, make_links = is.na(dashboard))
    progress_msg("Computing dashboard...", "Computing dashboard...done")
  } else {
    my_dashboard <- NULL
  }

  logo <- "logo.png"
  # <link rel="icon" type="image/png" href="{{ logo }}">
  if (!is.na(dashboard) && dashboard) {
    progress_msg("Writing dashboard...", "Start")
    my_dashboard <- htmltools::tagList(
      htmltools::tags$head(
        htmltools::tags$title(title),
        htmltools::htmlTemplate(
          text_ = '<link rel="icon" type="image/png" href="{{ logo }}">',
          logo = logo,
          document_ = FALSE)
      ),
      htmltools::HTML("<!-- done -->"),
      my_dashboard
    )
    # important: no progress update after this line should replace the index.html any more, so deregister
    prep_deregister_progress_hook(.hi, verbose = FALSE)
    prep_deregister_progress_hook(.hp, verbose = FALSE)
    prep_deregister_progress_hook(.hm, verbose = FALSE)
    progress_msg("Writing dashboard...", "Done")
    htmltools::save_html(my_dashboard,
                         libdir = file.path(content_dir, "lib"),
                         file = content_file)
    util_write_renderinfo_js_json(
      content_dir,
      rep_id,
      start_time = start_time,
      end_time = Sys.time()
    )
    fnms <- setNames(list(content_file), nm = paste0("dash-", title))
    if (util_really_rstudio()) {
      rstudioapi::executeCommand("activateConsole")
    }

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
    js_escape_string <- function(x) {
      x <- gsub("\\\\", "\\\\\\\\", x)  # Backslash first
      x <- gsub("\"", "\\\\\"", x)      # "
      x <- gsub("\n", "\\\\n", x)
      x <- gsub("\r", "\\\\r", x)
      x <- gsub("\t", "\\\\t", x)
      x
    }
    cat(sep = "",
        "window.all_ids = {\"all_ids\": ",
        paste0("[",
               paste0('"', js_escape_string(all_ids), '"', collapse = ", "),
               "]"),
        "}",
        file = file.path(dir, "anchor_list.js")
    )
    saveRDS(object = all_ids, file = file.path(dir, "anchor_list.RDS"))

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

    util_hide_file_windows(dir)

    progress_msg(sprintf("Wrote %d files", length(fnms)),
                 sprintf("Wrote %s", util_pretty_vector_string(fnms, n_max = 20)))

    # important: no progress update after this line should replace the index.html any more, so deregister
    prep_deregister_progress_hook(.hi, verbose = FALSE)
    prep_deregister_progress_hook(.hp, verbose = FALSE)
    prep_deregister_progress_hook(.hm, verbose = FALSE)

    util_write_index_html(
      file.path(content_dir, "index.html"),
      util_index_redirect_lines(
        title = title,
        target_rel = paste0(".report/", basename(fnms[[1]])),
        delay_ms = 300L
      )
    )

    util_write_renderinfo_js_json(
      content_dir,
      rep_id,
      start_time = start_time,
      end_time = end_time
    )

    if (util_really_rstudio()) {
      rstudioapi::executeCommand("activateConsole")
    }
  }

  if (view) {
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
#' shrinking. Supports complex plots with faceting or patchwork layouts.
#'
#' @param plot A ggplot object or patchwork object
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


  is_pairs_plot <-
    identical(attr(plot, "sizing_hints")$figure_type_id, "pairs_plot")

  min_text_pt <- 6.5
  max_text_pt <- 12
  scale_factor <- mean(c(inner_width / 6, inner_height / 4))
  base_size <- base_text_factor * scale_factor
  base_size <- max(min_text_pt, min(base_size, max_text_pt))

  if (!is_pairs_plot)
    plot <- plot + util_theme_with_scaling_limits(base_size = base_size)

  pw_widths <- NULL
  pw_heights <- NULL
  if (inherits(plot, "patchwork")) {
    pw_widths <- try(plot$patches$widths, silent = TRUE)
    if (util_is_try_error(pw_widths)) {
      pw_widths <- NULL
    }
    pw_heights <- try(plot$patches$heights, silent = TRUE)
    if (util_is_try_error(pw_heights)) {
      pw_heights <- NULL
    }
  }

  if (!is_pairs_plot) plot <- util_gg_stagger_x_axis_labels(
                                                plot,
                                                inner_width = inner_width,
                                                inner_height = inner_height,
                                                patchwork_widths = pw_widths,
                                                patchwork_heights = pw_heights,
                                                theme_base_size = base_size
                                              )

  as_plot_gtable <- function(plot) {
    if (inherits(plot, "patchwork")) {
      util_ensure_suggested("patchwork", err = TRUE)
      return(patchwork::patchworkGrob(plot))
    }
    ggplot2::ggplotGrob(plot)
  }

  sum_width_in <- function(x) {
    if (!length(x)) return(0)
    sum(grid::convertWidth(x, "in", valueOnly = TRUE))
  }

  sum_height_in <- function(x) {
    if (!length(x)) return(0)
    sum(grid::convertHeight(x, "in", valueOnly = TRUE))
  }

  find_panel_tracks <- function(gt) {
    nm <- gt$layout$name

    idx_panel <- grep("^panel($|[-;])", nm)
    idx_panel_area <- which(nm == "panel-area")

    idx <- sort(unique(c(idx_panel, idx_panel_area)))

    if (!length(idx)) {
      idx <- grep("panel", nm, fixed = TRUE)
    }

    if (!length(idx)) {
      return(list(rows = integer(0), cols = integer(0)))
    }

    rows <- unique(unlist(Map(seq.int, gt$layout$t[idx], gt$layout$b[idx])))
    cols <- unique(unlist(Map(seq.int, gt$layout$l[idx], gt$layout$r[idx])))

    list(rows = sort(rows), cols = sort(cols))
  }

  measure_non_panel_overhead_in <- function(gt) {
    tracks <- find_panel_tracks(gt)

    n_rows <- length(gt$heights)
    n_cols <- length(gt$widths)

    panel_rows <- tracks$rows
    panel_cols <- tracks$cols

    non_panel_rows <- setdiff(seq_len(n_rows), panel_rows)
    non_panel_cols <- setdiff(seq_len(n_cols), panel_cols)

    list(
      overhead_width = sum_width_in(gt$widths[non_panel_cols]),
      overhead_height = sum_height_in(gt$heights[non_panel_rows]),
      panel_rows = panel_rows,
      panel_cols = panel_cols
    )
  }

  g <- try(as_plot_gtable(plot), silent = TRUE)
  if (util_is_try_error(g)) {
    g <- NULL
  }

  if (is.null(g)) {
    overhead_width <- 0
    overhead_height <- 0
  } else {
    dims <- measure_non_panel_overhead_in(g)
    overhead_width <- dims$overhead_width
    overhead_height <- dims$overhead_height
  }

  pad_left <- 0.03
  pad_right <- 0.03
  pad_bottom <- 0.04
  pad_top <- 0.08

  total_width <- inner_width + overhead_width + pad_left + pad_right
  total_height <- inner_height + overhead_height + pad_top + pad_bottom

  e <- try(ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = total_width,
    height = total_height,
    dpi = dpi,
    units = "in",
    limitsize = FALSE
  ), silent = TRUE)

  if (util_is_try_error(e)) {
    msg <- conditionMessage(attr(e, "condition"))
    msg_stripped <- msg
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      msg_stripped <- cli::ansi_strip(msg_stripped)
    }

    e <- try(ggplot2::ggsave(
      filename = filename,
      plot = util_ggplot_text(msg_stripped),
      width = total_width,
      height = total_height,
      dpi = dpi,
      units = "in",
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
                                           rel_axis = 0.72,
                                           rel_title = 0.9,
                                           rel_subtitle = 0.8) {
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
    plot.title      = ggplot2::element_text(size = base_size * rel_title),
    plot.subtitle   = ggplot2::element_text(size = base_size * rel_subtitle)
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

# writes in subfolder .report and also renderinfo.json
util_write_renderinfo_js_json <- function(output_dir,
                                          rep_id = NULL,
                                          start_time = NULL,
                                          end_time = NULL) {
  output_dir <- file.path(output_dir, ".report")

  if (!dir.exists(output_dir)) {
    if (!dir.create(output_dir)) {
      util_error("Could not create %s -- exists as file?", output_dir)
    }
  }

  if (is.null(rep_id))
    rep_id <- util_make_report_id()

  if (is.null(start_time) ||
      is.null(end_time) ||
      any(is.na(start_time)) ||
      any(is.na(end_time)) ||
      length(start_time) != 1 ||
      length(end_time) != 1) {
    t <- "null"
  } else {
    t <- paste0("\"",
               as.character(round(
                 difftime(end_time, start_time, units = "mins"), 2)),
          " min", "\"")
  }

  fn <- file.path(output_dir, "renderinfo.json")
  tmp <- paste0(fn, ".tmp")

  txt <- paste0(
    "{\"renderingTime\": ",
    t,
    ", ",
    "\"reportId\": \"",
    rep_id,
    "\"}\n"
  )

  # write to temp file first (same directory => atomic rename on POSIX)
  cat(txt, file = tmp, sep = "")

  ok <- file.rename(tmp, fn)
  if (!isTRUE(ok)) {
    # Windows can fail if target exists / is locked -> best-effort replace
    unlink(fn, force = TRUE)
    ok2 <- file.rename(tmp, fn)
    if (!isTRUE(ok2)) {
      # last resort: copy then remove temp
      file.copy(tmp, fn, overwrite = TRUE)
      unlink(tmp, force = TRUE)
    }
  }

  fn <- file.path(output_dir, "renderinfo.js")
  tmp <- paste0(fn, ".tmp")

  js_txt <- paste0("window.renderingData = ", txt, ";")

  # write to temp file first (same directory => atomic rename on POSIX)
  cat(js_txt, file = tmp, sep = "")

  ok <- file.rename(tmp, fn)
  if (!isTRUE(ok)) {
    # Windows can fail if target exists / is locked -> best-effort replace
    unlink(fn, force = TRUE)
    ok2 <- file.rename(tmp, fn)
    if (!isTRUE(ok2)) {
      # last resort: copy then remove temp
      file.copy(tmp, fn, overwrite = TRUE)
      unlink(tmp, force = TRUE)
    }
  }

  invisible(rep_id)
}

util_write_index_html <- function(path, lines, tmp_name = ".index.html") {
  util_stop_if_not(length(path) == 1L)
  util_stop_if_not(is.character(lines))

  dir <- dirname(path)
  tmp <- file.path(dir, tmp_name)

  con <- file(tmp, open = "wb")
  on.exit({
    if (exists("con", inherits = FALSE) && inherits(try(close(con), silent = TRUE), "try-error")) {
      NULL
    }
  }, add = TRUE)

  writeLines(lines, con = con, sep = "\n", useBytes = TRUE)
  flush(con)
  close(con)
  rm(con)

  if (.Platform$OS.type != "windows") {
    ok <- file.rename(tmp, path)
    if (!ok) {
      if (file.exists(tmp)) {
        unlink(tmp, force = TRUE)
      }
      util_error("Failed to atomically replace: %s -> %s",
                 dQuote(tmp), dQuote(path))
    }
    return(invisible(path))
  }

  # Windows fallback: keep 'path' always present (not atomic, but no gap)
  ok <- file.copy(tmp, path, overwrite = TRUE)
  if (!ok) {
    if (file.exists(tmp)) {
      unlink(tmp, force = TRUE)
    }
    util_error("Failed to copy temp index into place: %s -> %s",
               dQuote(tmp), dQuote(path))
  }

  if (file.exists(tmp)) {
    for (i in seq_len(10L)) {
      if (unlink(tmp, force = TRUE) == 0L || !file.exists(tmp)) {
        break
      }
      Sys.sleep(0.05)
    }
  }

  invisible(path)
}

.outer_by_env <- new.env(parent = emptyenv())

util_index_loading_lines <- function(title,
                                     message = "Preparing report...",
                                     detail = NULL,
                                     reload_ms = 1200L,
                                     n = Inf,
                                     percent = 0,
                                     logo_rel = "logo.png") {

  title_escaped <- htmltools::htmlEscape(title)

  detail_html <- ""
  if (!!length(detail) && nzchar(detail)) {
    detail_html <- paste0(
      "<p class=\"detail\">",
      htmltools::htmlEscape(detail),
      "</p>"
    )
  }

  reload_s <- max(1, as.integer(round(as.integer(reload_ms) / 1000)))

  show_progress <- is.numeric(percent) && length(percent) == 1 &&
    is.finite(percent) && percent >= 0 && percent <= 100

  indicator_html <- if (show_progress) {
    paste0(
      "<div class=\"progress\" aria-hidden=\"true\">",
      "<div class=\"progress-bar\" style=\"width:", round(percent), "%\"></div>",
      "</div>",
      "<div class=\"progress-label\">", round(percent), "%</div>"
    )
  } else {
    "<div class=\"spinner\" aria-hidden=\"true\"></div>"
  }

  outer_html <- ""
  if (is.list(.outer_by_env$outer_by) &&
      length(.outer_by_env$outer_by) &&
      !is.null(.outer_by_env$outer_by$i) &&
      !is.null(.outer_by_env$outer_by$n)) {

    outer_i <- suppressWarnings(as.integer(.outer_by_env$outer_by$i))
    outer_n <- suppressWarnings(as.integer(.outer_by_env$outer_by$n))
    outer_msg <- .outer_by_env$outer_by$msg

    valid_outer <- length(outer_i) == 1 &&
      length(outer_n) == 1 &&
      !is.na(outer_i) &&
      !is.na(outer_n) &&
      outer_n > 0 &&
      outer_i >= 1 &&
      outer_i <= outer_n

    if (valid_outer) {
      outer_percent <- ((outer_i - 1) / outer_n) * 100
      outer_percent <- max(0, min(100, outer_percent))

      outer_msg_html <- ""
      if (!!length(outer_msg) && nzchar(outer_msg)) {
        outer_msg_html <- paste0(
          "<div class=\"outer-msg\">",
          htmltools::htmlEscape(outer_msg),
          "</div>"
        )
      }

      outer_html <- paste0(
        "<div class=\"outer-block\">",
        outer_msg_html,
        "<div class=\"progress outer-progress\" aria-hidden=\"true\">",
        "<div class=\"progress-bar\" style=\"width:", round(outer_percent), "%\"></div>",
        "</div>",
        "<div class=\"progress-label\">",
        "Overall: ", round(outer_percent), "% &mdash; step ",
        outer_i, " / ", outer_n,
        "</div>",
        "</div>"
      )
    } else {
      outer_parts <- character(0)

      if (!!length(outer_msg) && nzchar(outer_msg)) {
        outer_parts <- c(
          outer_parts,
          paste0(
            "<div class=\"outer-msg\">",
            htmltools::htmlEscape(outer_msg),
            "</div>"
          )
        )
      }

      if (length(outer_i) == 1 && !is.na(outer_i)) {
        if (length(outer_n) == 1 && !is.na(outer_n) && outer_n > 0) {
          outer_parts <- c(
            outer_parts,
            paste0(
              "<div class=\"progress-label\">",
              "Overall: step ", outer_i, " / ", outer_n,
              "</div>"
            )
          )
        } else {
          outer_parts <- c(
            outer_parts,
            paste0(
              "<div class=\"progress-label\">",
              "Overall: step ", outer_i,
              "</div>"
            )
          )
        }
      } else if (length(outer_n) == 1 && !is.na(outer_n) && outer_n > 0) {
        outer_parts <- c(
          outer_parts,
          paste0(
            "<div class=\"progress-label\">",
            "Overall: ", outer_n, " steps",
            "</div>"
          )
        )
      }

      outer_parts <- c(
        outer_parts,
        "<div class=\"spinner outer-spinner\" aria-hidden=\"true\"></div>"
      )

      outer_html <- paste0(
        "<div class=\"outer-block\">",
        paste(outer_parts, collapse = ""),
        "</div>"
      )
    }
  }

  # --- favicon (logo + small badge) ---
  icon_lines <- character(0)
  if (!is.null(logo_rel) && nzchar(logo_rel)) {
    badge_text <- if (show_progress) paste0(round(percent), "%") else "..."
    # Minimal SVG favicon with embedded logo + badge (bottom-right)
    svg <- paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'>",
      "<image href='", htmltools::htmlEscape(logo_rel), "' x='0' y='0' width='64' height='64'/>",
      "<circle cx='50' cy='50' r='13' fill='white' opacity='0.92'/>",
      "<circle cx='50' cy='50' r='12' fill='rgba(0,0,0,0.55)'/>",
      "<text x='50' y='54' text-anchor='middle' font-family='system-ui,-apple-system,sans-serif' ",
      "font-size='14' fill='white'>", htmltools::htmlEscape(badge_text), "</text>",
      "</svg>"
    )
    svg_enc <- utils::URLencode(svg, reserved = TRUE)
    icon_lines <- c(
      paste0("<link rel=\"icon\" type=\"image/svg+xml\" href=\"data:image/svg+xml,", svg_enc, "\">"),
      paste0("<link rel=\"apple-touch-icon\" href=\"", htmltools::htmlEscape(logo_rel), "\">")
    )
  }

  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<meta http-equiv=\"Cache-Control\" content=\"no-store, no-cache, must-revalidate\">",
    "<meta http-equiv=\"Pragma\" content=\"no-cache\">",
    "<meta http-equiv=\"Expires\" content=\"0\">",
    icon_lines,
    # paste0("<meta http-equiv=\"refresh\" content=\"", reload_s, "\">"),
    "<script>",
    "(function(){",
    "  'use strict';",
    "  var ms = ", as.integer(reload_ms), ";",
    "  setTimeout(function(){",
    "    try { location.reload(); } catch(e) {}",
    "  }, Math.max(250, ms));",
    "})();",
    "</script>",
    "<title>...", title_escaped, "... Progressing</title>",
    "<style>",
    "body{font-family:system-ui,-apple-system,sans-serif;display:flex;justify-content:center;align-items:center;height:100vh;margin:0;background:#f8f9fa;color:#333;}",
    ".box{text-align:center;max-width:42rem;width:100%;padding:1.25rem;}",
    ".spinner{width:36px;height:36px;border:4px solid rgba(0,0,0,.15);border-top-color:rgba(0,0,0,.55);border-radius:50%;animation:spin 1s linear infinite;margin:0 auto .9rem;}",
    "@keyframes spin{to{transform:rotate(360deg);}}",
    ".progress{width:220px;height:10px;background:rgba(0,0,0,.15);border-radius:6px;margin:0 auto .5rem;overflow:hidden;}",
    ".progress-bar{height:100%;background:rgba(0,0,0,.55);transition:width .4s ease;}",
    ".progress-label{font-size:.85rem;opacity:.75;margin-bottom:.4rem;}",
    ".detail{opacity:.75;font-size:.95rem;}",
    ".outer-block{position:fixed;top:0;left:0;width:100%;padding-top:1rem;display:flex;flex-direction:column;align-items:center;background:#f8f9fa;z-index:1000;}",
    ".outer-progress{width:80%;max-width:42rem;}",
    ".outer-msg{font-size:.95rem;font-weight:600;margin-bottom:.35rem;}",
    ".outer-spinner{width:28px;height:28px;margin:0 auto .5rem;}",
    "</style>",
    "</head>",
    "<body>",
    "<div class=\"box\">",
    outer_html,
    indicator_html,
    "<h2>", htmltools::htmlEscape(message), "</h2>",
    detail_html,
    "</div>",
    "</body>",
    "</html>"
  )
}

util_index_redirect_lines <- function(title,
                                      target_rel,
                                      delay_ms = 0L,
                                      logo_rel = "logo.png") {

  title_escaped <- htmltools::htmlEscape(title)
  delay_ms <- max(0L, as.integer(delay_ms))
  target_js <- paste0('"', (gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", target_rel))), '"')

  icon_lines <- character(0)
  if (!is.null(logo_rel) && nzchar(logo_rel)) {
    icon_lines <- c(
      paste0("<link rel=\"icon\" href=\"", htmltools::htmlEscape(logo_rel), "\">"),
      paste0("<link rel=\"apple-touch-icon\" href=\"", htmltools::htmlEscape(logo_rel), "\">")
    )
  }

  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<!-- done -->",
    "<head>",
    "<meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<meta http-equiv=\"Cache-Control\" content=\"no-store, no-cache, must-revalidate\">",
    "<meta http-equiv=\"Pragma\" content=\"no-cache\">",
    "<meta http-equiv=\"Expires\" content=\"0\">",
    icon_lines,
    "<script>",
    "(function(){",
    "  'use strict';",
    paste0("  var target = ", target_js, ";"),
    "  var ms = ", delay_ms, ";",
    "  function go(){",
    "    try { location.replace(target); } catch(e) { try { location.href = target; } catch(e2) {} }",
    "  }",
    "  if (ms <= 0) go(); else setTimeout(go, ms);",
    "})();",
    "</script>",
    "<title>", title_escaped, "</title>",
    "</head>",
    "<body></body>",
    "</html>"
  )
}

util_index_error_lines <- function(title,
                                   message = "An error occurred",
                                   detail = NULL,
                                   logo_rel = "logo.png",
                                   reload_ms = 5000L) {

  title_escaped <- htmltools::htmlEscape(title)

  detail_html <- ""
  if (!is.null(detail) && nzchar(detail)) {
    detail_html <- paste0(
      "<pre class=\"detail\">",
      htmltools::htmlEscape(detail),
      "</pre>"
    )
  }

  # --- favicon (logo + red badge) ---
  icon_lines <- character(0)
  if (!is.null(logo_rel) && nzchar(logo_rel)) {
    svg <- paste(
      "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'>",
      "<image href='", htmltools::htmlEscape(logo_rel),
      "' x='0' y='0' width='64' height='64'/>",
      "<circle cx='50' cy='50' r='13' fill='white' opacity='0.95'/>",
      "<circle cx='50' cy='50' r='12' fill='#b00020'/>",
      "<text x='50' y='55' text-anchor='middle' font-family='system-ui,-apple-system,sans-serif' ",
      "font-size='20' fill='white'>!</text>",
      "</svg>",
      sep = "\n",
      collapse = "\n"
    )
    svg_enc <- utils::URLencode(svg, reserved = TRUE)
    icon_lines <- c(
      paste0("<link rel=\"icon\" type=\"image/svg+xml\" href=\"data:image/svg+xml,", svg_enc, "\">"),
      paste0("<link rel=\"apple-touch-icon\" href=\"", htmltools::htmlEscape(logo_rel), "\">")
    )
  }

  reload_ms <- as.integer(reload_ms)
  if (!is.finite(reload_ms) || reload_ms < 0L) reload_ms <- 5000L

  # --- JS: poll renderinfo.js; if it appears (or changes), soft-reload the page ---
  refresh_js <- paste(
    "(function(){",
    "  var delay=", reload_ms, ";",
    "  var baseUrl = (function(){",
    "    var u = window.location.href;",
    "    var i = u.indexOf('#'); if (i>=0) u = u.slice(0,i);",
    "    return u;",
    "  })();",
    "  function addBuster(u){",
    "    u = u.replace(/([?&])__dq_refresh=\\d+(&)?/g, function(m, sep, tail){",
    "      return sep === '?' ? (tail ? '?' : '') : (tail ? sep : '');",
    "    });",
    "    u = u.replace(/[?&]$/, '');",
    "    return u + (u.indexOf('?')>=0?'&':'?') + '__dq_refresh=' + Date.now();",
    "  }",
    "  function softReload(){",
    "    try { window.location.replace(addBuster(baseUrl)); } catch(e) {}",
    "  }",
    "",
    "  // Compute sibling URL: same dir as current document, file name '.report/renderinfo.js'",
    "  function renderinfoUrl(){",
    "    try {",
    "      var u = new URL(baseUrl);",
    "      // Replace last path segment with .report/renderinfo.js",
    "      u.pathname = u.pathname.replace(/\\/[^\\/]*$/, '/.report/renderinfo.js');",
    "      return u.toString();",
    "    } catch(e) {",
    "      // Fallback for older URL handling: manual",
    "      return baseUrl.replace(/\\/[^\\/]*$/, '/.report/renderinfo.js');",
    "    }",
    "  }",
    "",
    "  var lastToken = null;",
    "  var inFlight = false;",
    "  function check(){",
    "    if (inFlight) return;",
    "    inFlight = true;",
    "",
    "    // IMPORTANT: renderinfo.js should define window.renderingData = {...}",
    "    // When it does not exist -> that indicates an error; do NOT reload blindly.",
    "    var s = document.createElement('script');",
    "    s.async = true;",
    "    s.src = addBuster(renderinfoUrl());",
    "",
    "    s.onload = function(){",
    "      try {",
    "        var d = window.renderingData || null;",
    "        // If renderinfo.js loaded but didn't set data, treat as 'not ready' and keep polling.",
    "        if (!d) return;",
    "        // Build a token that changes when the report changes.",
    "        var token = String(d.reportId || '') + '|' + String(d.renderingTime || '');",
    "        if (lastToken === null) {",
    "          lastToken = token;",
    "          // renderinfo.js now exists => reload once to leave error page",
    "          softReload();",
    "          return;",
    "        }",
    "        if (token !== lastToken) {",
    "          lastToken = token;",
    "          softReload();",
    "          return;",
    "        }",
    "      } finally {",
    "        // cleanup",
    "        try { s.remove(); } catch(e) {}",
    "      }",
    "    };",
    "",
    "    s.onerror = function(){",
    "      // renderinfo.js missing -> error state; keep polling but do not reload",
    "      try { s.remove(); } catch(e) {}",
    "    };",
    "",
    "    // Append and always release the inFlight flag at end of this tick",
    "    (document.head || document.documentElement).appendChild(s);",
    "    window.setTimeout(function(){ inFlight = false; }, 0);",
    "  }",
    "",
    "  check();",
    "  window.setInterval(check, Math.max(1000, delay));",
    "})();",
    sep = "\n",
    collapse = "\n"
  )

  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<meta http-equiv=\"Cache-Control\" content=\"no-store, no-cache, must-revalidate\">",
    "<meta http-equiv=\"Pragma\" content=\"no-cache\">",
    "<meta http-equiv=\"Expires\" content=\"0\">",
    icon_lines,
    "<title>\u2716 ", title_escaped, "</title>",
    "<style>",
    "body{font-family:system-ui,-apple-system,sans-serif;display:flex;justify-content:center;",
    "align-items:center;height:100vh;margin:0;background:#fff5f6;color:#2a0006;}",
    ".box{text-align:center;max-width:48rem;padding:1.5rem;}",
    ".icon{font-size:3rem;color:#b00020;margin-bottom:.6rem;}",
    "h2{color:#b00020;margin:.3rem 0 .6rem 0;}",
    ".detail{margin-top:.8rem;padding:.8rem;background:#fff;",
    "border:1px solid #f1b0b7;border-radius:6px;",
    "text-align:left;white-space:pre-wrap;max-height:40vh;overflow:auto;}",
    "</style>",
    "<script>", refresh_js, "</script>",
    "</head>",
    "<body>",
    "<div class=\"box\">",
    "<div class=\"icon\">\u26A0</div>",
    "<h2>", htmltools::htmlEscape(message), "</h2>",
    detail_html,
    "</div>",
    "</body>",
    "</html>"
  )
}

util_gg_stagger_x_axis_labels <- function(
    p,
    inner_width = NULL,
    inner_height = NULL,
    angle = NULL,
    auto_dodge = TRUE,
    max_dodge = 3L,
    min_labels_for_change = 28L,
    min_size = 4.2,
    title_scale = 0.90,
    subtitle_scale = 0.82,
    y_text_scale = 0.92,
    patchwork_widths = NULL,
    patchwork_heights = NULL,
    theme_base_size = NULL,
    verbose = FALSE) {

  ok <-
    (is.null(inner_width) || (length(inner_width) == 1L && is.numeric(inner_width) &&
                                is.finite(inner_width) && inner_width > 0)) &&
    (is.null(inner_height) || (length(inner_height) == 1L && is.numeric(inner_height) &&
                                 is.finite(inner_height) && inner_height > 0)) &&
    length(max_dodge) == 1L &&
    is.numeric(max_dodge) &&
    max_dodge >= 1 &&
    length(min_labels_for_change) == 1L &&
    is.numeric(min_labels_for_change) &&
    min_labels_for_change >= 1 &&
    length(min_size) == 1L &&
    is.numeric(min_size) &&
    min_size > 0 &&
    length(auto_dodge) == 1L &&
    is.logical(auto_dodge) &&
    length(verbose) == 1L &&
    is.logical(verbose) &&
    length(title_scale) == 1L &&
    is.numeric(title_scale) &&
    is.finite(title_scale) &&
    title_scale > 0 &&
    length(subtitle_scale) == 1L &&
    is.numeric(subtitle_scale) &&
    is.finite(subtitle_scale) &&
    subtitle_scale > 0 &&
    length(y_text_scale) == 1L &&
    is.numeric(y_text_scale) &&
    is.finite(y_text_scale) &&
    y_text_scale > 0 &&
    (is.null(angle) || (length(angle) == 1L && is.numeric(angle))) &&
    (is.null(patchwork_widths) ||
       (is.numeric(patchwork_widths) && length(patchwork_widths) >= 1L &&
          all(is.finite(patchwork_widths)) && all(patchwork_widths > 0))) &&
    (is.null(patchwork_heights) ||
       (is.numeric(patchwork_heights) && length(patchwork_heights) >= 1L &&
          all(is.finite(patchwork_heights)) && all(patchwork_heights > 0))) &&
    (is.null(theme_base_size) ||
       (length(theme_base_size) == 1L && is.numeric(theme_base_size) &&
          is.finite(theme_base_size) && theme_base_size > 0))

  if (!ok) {
    return(p)
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  is_patchwork_obj <- function(x) {
    inherits(x, "patchwork") ||
      (!is.null(x$patches) &&
         is.list(x$patches) &&
         !is.null(x$patches$plots) &&
         is.list(x$patches$plots))
  }

  normalize_weights <- function(w, n, default = 1) {
    if (is.null(w) || !length(w)) {
      w <- rep(default, n)
    } else {
      if (length(w) < n) {
        w <- c(w, rep(tail(w, 1L), n - length(w)))
      } else if (length(w) > n) {
        w <- w[seq_len(n)]
      }
    }
    s <- sum(w)
    if (!is.finite(s) || s <= 0) {
      return(rep(1 / n, n))
    }
    w / s
  }

  get_theme_obj <- function(g) {
    th <- try(ggplot2::theme_get() + g$theme, silent = TRUE)
    if (util_is_try_error(th) || is.null(th)) {
      return(NULL)
    }
    th
  }

  get_local_theme_size <- function(g) {
    th_local <- g$theme
    if (is.null(th_local)) {
      return(NULL)
    }

    sz <- th_local$axis.text.x$size %||%
      th_local$axis.text$size %||%
      th_local$text$size

    if (is.null(sz) || !is.numeric(sz) || length(sz) != 1L || !is.finite(sz)) {
      return(NULL)
    }

    sz
  }

  get_theme_text_size <- function(g) {
    th <- get_theme_obj(g)
    if (is.null(th)) {
      return(11)
    }

    sz <- th$axis.text.x$size %||%
      th$axis.text$size %||%
      th$text$size %||%
      11

    if (!is.numeric(sz) || length(sz) != 1L || !is.finite(sz)) {
      return(11)
    }

    sz
  }

  get_theme_angle <- function(g) {
    th <- get_theme_obj(g)
    if (is.null(th)) {
      return(NULL)
    }

    ang <- th$axis.text.x$angle
    if (is.null(ang) || !is.numeric(ang) || length(ang) != 1L || !is.finite(ang)) {
      return(NULL)
    }

    ang
  }

  get_x_labels_info <- function(g) {
    b <- try(ggplot2::ggplot_build(g), silent = TRUE)
    if (util_is_try_error(b)) {
      return(NULL)
    }

    sc <- try(b$plot$scales$get_scales("x"), silent = TRUE)
    if (util_is_try_error(sc) || is.null(sc)) {
      return(NULL)
    }

    is_discrete_x <- inherits(sc, "ScaleDiscrete") ||
      inherits(sc, "ScaleDiscretePosition")

    if (!isTRUE(is_discrete_x)) {
      return(NULL)
    }

    pp <- try(b$layout$panel_params[[1L]], silent = TRUE)
    if (util_is_try_error(pp) || is.null(pp)) {
      return(NULL)
    }

    labels <- try(pp$x$get_labels(), silent = TRUE)
    if (util_is_try_error(labels) || is.null(labels)) {
      return(NULL)
    }

    labels <- as.character(labels)
    labels <- labels[nzchar(labels)]

    if (!length(labels)) {
      return(NULL)
    }

    list(
      labels = labels,
      n = length(labels),
      mean_nchar = mean(nchar(labels), na.rm = TRUE),
      max_nchar = max(nchar(labels), na.rm = TRUE)
    )
  }

  choose_target_size <- function(base_size, info, inner_width = NULL) {
    n_lab <- info$n
    mean_nchar <- info$mean_nchar
    max_nchar <- info$max_nchar

    target <- base_size

    if (n_lab <= 12L) {
      return(target)
    }

    if (n_lab <= 20L) {
      if (mean_nchar >= 10) {
        target <- min(target, base_size * 0.96)
      }
      return(max(min_size, target))
    }

    if (n_lab < min_labels_for_change) {
      if (!is.null(inner_width) && is.finite(inner_width) && inner_width > 0) {
        labels_per_in <- n_lab / inner_width
        if (labels_per_in >= 7) {
          target <- min(target, base_size * 0.92)
        }
      }
      return(max(min_size, target))
    }

    if (n_lab >= 28)  target <- min(target, 7.2)
    if (n_lab >= 40)  target <- min(target, 6.6)
    if (n_lab >= 60)  target <- min(target, 5.9)
    if (n_lab >= 90)  target <- min(target, 5.2)
    if (n_lab >= 130) target <- min(target, 4.7)
    if (n_lab >= 180) target <- min(target, 4.2)

    if (mean_nchar >= 8)  target <- target - 0.15
    if (mean_nchar >= 12) target <- target - 0.25
    if (max_nchar >= 20)  target <- target - 0.15

    if (!is.null(inner_width) && is.finite(inner_width) && inner_width > 0) {
      labels_per_in <- n_lab / inner_width

      if (labels_per_in >= 8)  target <- min(target, 6.3)
      if (labels_per_in >= 11) target <- min(target, 5.6)
      if (labels_per_in >= 14) target <- min(target, 5.0)
      if (labels_per_in >= 18) target <- min(target, 4.5)
    }

    max(min_size, target)
  }

  choose_n_dodge <- function(info, angle_eff, inner_width = NULL) {
    if (!isTRUE(auto_dodge)) {
      return(1L)
    }

    n_lab <- info$n
    mean_nchar <- info$mean_nchar

    if (n_lab <= 35L) {
      return(1L)
    }

    if (!is.null(angle_eff) && abs(angle_eff) >= 80) {
      return(1L)
    }

    n_dodge <- 1L

    if (n_lab >= 70 || mean_nchar >= 16) {
      n_dodge <- 2L
    }
    if (n_lab >= 140 || mean_nchar >= 24) {
      n_dodge <- 3L
    }

    if (!is.null(inner_width) && is.finite(inner_width) && inner_width > 0) {
      labels_per_in <- n_lab / inner_width
      if (labels_per_in >= 14) n_dodge <- max(n_dodge, 2L)
      if (labels_per_in >= 22) n_dodge <- max(n_dodge, 3L)
    }

    min(as.integer(max_dodge), n_dodge)
  }

  scale_patchwork_annotation_theme <- function(x, base_size_local) {
    if (!is_patchwork_obj(x)) {
      return(x)
    }

    ann_theme <- NULL
    if (!is.null(x$patches$annotation) &&
        !is.null(x$patches$annotation$theme)) {
      ann_theme <- x$patches$annotation$theme
    }

    if (is.null(ann_theme)) {
      ann_theme <- ggplot2::theme()
    }

    x$patches$annotation$theme <- ann_theme + ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = max(min_size, base_size_local * 0.66)
      ),
      plot.subtitle = ggplot2::element_text(
        size = max(min_size, base_size_local * 0.58)
      ),
      plot.caption = ggplot2::element_text(
        size = max(min_size, base_size_local * 0.54)
      )
    )

    x
  }

  apply_one <- function(g, inner_width_local = inner_width, inner_height_local = inner_height) {
    if (!inherits(g, "ggplot") || is_patchwork_obj(g)) {
      return(g)
    }

    info <- get_x_labels_info(g)
    if (is.null(info)) {
      return(g)
    }

    base_size <- theme_base_size %||% get_local_theme_size(g) %||% get_theme_text_size(g)
    angle_eff <- angle %||% get_theme_angle(g)

    if (info$n <= 12L) {
      return(g)
    }

    n_dodge <- choose_n_dodge(info, angle_eff, inner_width = inner_width_local)
    target_size <- choose_target_size(base_size, info, inner_width = inner_width_local)

    if (isTRUE(verbose)) {
      msg <- sprintf(
        paste0(
          "util_gg_stagger_x_axis_labels(): n_labels=%d, mean_nchar=%.1f, ",
          "inner_width=%s, size %.1f -> %.1f, n.dodge=%d"
        ),
        info$n,
        info$mean_nchar,
        if (is.null(inner_width_local)) "NULL" else format(inner_width_local),
        base_size,
        target_size,
        n_dodge
      )
      message(msg)
    }

    th <- get_theme_obj(g)

    axis_text_x_old <- NULL
    axis_text_y_old <- NULL
    plot_title_old <- NULL
    plot_subtitle_old <- NULL
    text_old <- NULL

    if (!is.null(th)) {
      axis_text_x_old <- th$axis.text.x
      axis_text_y_old <- th$axis.text.y
      plot_title_old <- th$plot.title
      plot_subtitle_old <- th$plot.subtitle
      text_old <- th$text
    }

    inherit_el <- function(el, default = ggplot2::element_text()) {
      if (is.null(el)) default else el
    }

    axis_text_x_old <- inherit_el(axis_text_x_old)
    axis_text_y_old <- inherit_el(axis_text_y_old)
    plot_title_old <- inherit_el(plot_title_old)
    plot_subtitle_old <- inherit_el(plot_subtitle_old)
    text_old <- inherit_el(text_old)

    scaled_base_size <- base_size

    out <- g + ggplot2::theme(
      text = ggplot2::element_text(
        family = text_old$family,
        face = text_old$face,
        colour = text_old$colour,
        size = scaled_base_size,
        hjust = text_old$hjust,
        vjust = text_old$vjust,
        angle = text_old$angle,
        lineheight = text_old$lineheight,
        margin = text_old$margin,
        debug = text_old$debug
      ),
      axis.text.x = ggplot2::element_text(
        family = axis_text_x_old$family,
        face = axis_text_x_old$face,
        colour = axis_text_x_old$colour,
        size = target_size,
        hjust = axis_text_x_old$hjust %||% 1,
        vjust = axis_text_x_old$vjust %||% 0.5,
        angle = angle_eff %||% axis_text_x_old$angle,
        lineheight = axis_text_x_old$lineheight,
        margin = axis_text_x_old$margin,
        debug = axis_text_x_old$debug
      ),
      axis.text.y = ggplot2::element_text(
        family = axis_text_y_old$family,
        face = axis_text_y_old$face,
        colour = axis_text_y_old$colour,
        size = max(min_size, min(scaled_base_size * y_text_scale,
                                 max(min_size, target_size * y_text_scale))),
        hjust = axis_text_y_old$hjust,
        vjust = axis_text_y_old$vjust,
        angle = axis_text_y_old$angle,
        lineheight = axis_text_y_old$lineheight,
        margin = axis_text_y_old$margin,
        debug = axis_text_y_old$debug
      ),
      plot.title = ggplot2::element_text(
        family = plot_title_old$family,
        face = plot_title_old$face,
        colour = plot_title_old$colour,
        size = max(min_size, scaled_base_size * title_scale),
        hjust = plot_title_old$hjust,
        vjust = plot_title_old$vjust,
        angle = plot_title_old$angle,
        lineheight = plot_title_old$lineheight,
        margin = plot_title_old$margin,
        debug = plot_title_old$debug
      ),
      plot.subtitle = ggplot2::element_text(
        family = plot_subtitle_old$family,
        face = plot_subtitle_old$face,
        colour = plot_subtitle_old$colour,
        size = max(min_size, scaled_base_size * subtitle_scale),
        hjust = plot_subtitle_old$hjust,
        vjust = plot_subtitle_old$vjust,
        angle = plot_subtitle_old$angle,
        lineheight = plot_subtitle_old$lineheight,
        margin = plot_subtitle_old$margin,
        debug = plot_subtitle_old$debug
      )
    )

    if (n_dodge > 1L) {
      guide_args <- list(n.dodge = n_dodge)
      if (!is.null(angle_eff)) {
        guide_args$angle <- angle_eff
      }

      out <- out + ggplot2::guides(
        x = do.call(ggplot2::guide_axis, guide_args)
      )
    }

    out
  }

  recurse <- function(x,
                      inner_width_local = inner_width,
                      inner_height_local = inner_height,
                      widths_local = patchwork_widths,
                      heights_local = patchwork_heights) {

    if (inherits(x, "ggplot") && !is_patchwork_obj(x)) {
      return(apply_one(
        x,
        inner_width_local = inner_width_local,
        inner_height_local = inner_height_local
      ))
    }

    if (is_patchwork_obj(x)) {
      plots <- x$patches$plots
      if (is.null(plots) || !length(plots)) {
        return(scale_patchwork_annotation_theme(
          x,
          base_size_local = theme_base_size %||% 11
        ))
      }

      n <- length(plots)
      w_prop <- normalize_weights(widths_local, n)
      h_prop <- normalize_weights(heights_local, n)

      for (i in seq_along(plots)) {
        child_width <- inner_width_local
        child_height <- inner_height_local

        if (!is.null(inner_width_local)) {
          child_width <- inner_width_local * w_prop[i]
        }
        if (!is.null(inner_height_local)) {
          child_height <- inner_height_local * h_prop[i]
        }

        plots[[i]] <- recurse(
          plots[[i]],
          inner_width_local = child_width,
          inner_height_local = child_height,
          widths_local = NULL,
          heights_local = NULL
        )
      }

      x$patches$plots <- plots
      x <- scale_patchwork_annotation_theme(
        x,
        base_size_local = theme_base_size %||% 11
      )

      return(x)
    }

    x
  }

  out <- tryCatch(
    recurse(p),
    error = function(e) {
      util_warning(
        "util_gg_stagger_x_axis_labels() failed; returning input unchanged."
      )
      p
    }
  )

  if (is.null(out)) {
    return(p)
  }

  out
}


# IDEA: For PDF output, support https://cran.r-project.org/web/packages/xmpdf/index.html

