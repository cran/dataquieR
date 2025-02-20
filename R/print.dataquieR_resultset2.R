#' Generate a HTML-based report from a [dataquieR] report
#'
#' @param x [dataquieR report v2][dq_report2].
#' @param dir [character] directory to store the rendered report's files,
#'   a temporary one,
#'   if omitted. Directory will be created, if missing, files may be
#'   overwritten inside that directory
#' @param view [logical] display the report
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param block_load_factor [numeric] multiply size of parallel compute blocks
#'                                    by this factor.
#' @param advanced_options [list] options to set during report computation,
#'                                see [options()]
#' @param ... additional arguments:
#' @param dashboard [logical] dashboard mode: `TRUE`: create a dashboard only,
#'                            `FALSE`: don't create a dashboard at all,
#'                            `NA` or missing: create a "normal" report with
#'                            a dashboard included.
#'
#' @return file names of the generated report's HTML files
#' @export
#' @importFrom utils browseURL
print.dataquieR_resultset2 <- function(
  x,
  dir,
  view = TRUE,
  disable_plotly = FALSE,
  block_load_factor = 4,
  advanced_options =  list(),
  dashboard = NA,
  ...) {
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
  })

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
          paste(conditionMessage(e), collapse = "\n")
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


  util_setup_rstudio_job("Rendering dq_report2 to HTML...")
  start_time <- Sys.time()

  if (length(dir) == 0) {
    dir <- tempfile()
    util_message("No output directory given (with dir=), setting it to %s",
                 dQuote(dir))
  }

  content_dir <- dir
  content_file <- file.path(content_dir, "index.html")
  dir <- file.path(dir, ".report")

  if (file.exists(dir)) {
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
    # IDEA: Group by Variables, if wanted: https://datatables.net/extensions/rowgroup/examples/initialisation/customRow.html
    title <- attr(report, "title")
    if (is.null(title)) {
      title <- "Data Quality Report"
    }

    tb <- util_dashboard_table(summary(report)) #TODO: test with filter to un-existing functions.

    mrg <- 2

    if (attr(report, "label_col") ==
        attr(tb, "label_col")) {
      pn <- c("SummaryPlot", "SummaryPlotList")
      tb$Figure <- mapply(
        SIMPLIFY = FALSE,
        lb = tb[[attr(tb, "label_col")]],
        cn = tb$call_names,
        function(lb, cn) {
          rn <- paste0(cn, ".", lb)
          if (rn %in% names(report)) {
            if (any(pn %in%
                    names(report[[rn]]))) {

              rrn <- head(intersect(pn, names(report[[rn]])), 1)

              if (util_is_gg(report[[rn]][[rrn]])) {
                r <- htmltools::plotTag(
                  {
                    withr::local_par(list(
                      mar = rep(mrg, 4),
                      oma = rep(0, 4)
                    ))
                    suppressWarnings(suppressMessages(
                      print(report[[rn]][[rrn]])
                    ))
                  },
                  width = 250,
                  height = 200,
                  alt = paste("Figure")
                )
                as.character(r)
              } else {
                NA_character_
              }
            } else {
              NA_character_
            }
          } else {
            NA_character_
          }
        }
      )
    }

    tb$value[!is.na(tb$Figure) &
               tb$value %in% c("T", "F")] <-
      tb$Figure[!is.na(tb$Figure) &
                  tb$value %in% c("T", "F")]

    if ("des_summary_continuous" %in% colnames(report)) {
      des <- report[, "des_summary_continuous", "SummaryTable"]$SummaryTable
      des[[attr(report, "label_col")]] <-
        attr(des$Variables, "plain_label")
      des$GraphCon <- des$Graph
      des$Graph <- NULL
      if(nrow(des) > 0) {
        tb <- suppressWarnings(merge(tb, des, all.x = TRUE,
                                     by = attr(report, "label_col"),
                                     suffixes = c("", "")))
        tb <- util_fix_merge_dups(tb)
      }
    }

    if ("des_summary_categorical" %in% colnames(report)) {
      des <- report[, "des_summary_categorical", "SummaryTable"]$SummaryTable
      des[[attr(report, "label_col")]] <-
        attr(des$Variables, "plain_label")
      des$GraphCat <- des$Graph
      des$Graph <- NULL
      if(nrow(des) > 0) {
        tb <- suppressWarnings(merge(tb, des, all.x = TRUE,
                                     by = attr(report, "label_col"),
                                     suffixes = c("", "")))
        tb <- util_fix_merge_dups(tb)
      }

    }

    if ("des_summary_continuous" %in% colnames(report) &&
        "des_summary_categorical" %in% colnames(report)  ) {
      tb$Graph <- tb$GraphCon
      tb$GraphCon <- NULL
      tb$Graph[is.na(tb$Graph)] <- tb$GraphCat[is.na(tb$Graph)]
      tb$GraphCat <- NULL
    } else if ("des_summary_continuous" %in% colnames(report)) {
      tb$Graph <- tb$GraphCon
      tb$GraphCon <- NULL
    } else if ("des_summary_categorical" %in% colnames(report)) {
      tb$Graph <- tb$GraphCat
      tb$GraphCat <- NULL
    }

    # put Graph to the left
    tb <- tb[, intersect(unique(c("Graph", colnames(tb))), colnames(tb)), FALSE]

    if (is.na(dashboard)) { # make links
      tb0 = tb
      for (cn in intersect(colnames(tb), c(
          attr(report, "label_col"),
          "Graph",
          VAR_NAMES
        ))) {
        cnt <- tb0[[cn]]
        lb <- tb0[[attr(report, "label_col")]]
        cl <- tb0[["call_names"]]
        tb[[cn]] <- mapply(SIMPLIFY = FALSE,
                                cnt = cnt,
                                lb = lb,
                                cl = cl,
                                function(cnt, lb, cl) {
                                  href <- paste0("VAR_", prep_link_escape(lb,
                                                                          html = TRUE),
                                                 ".html#",
                                                 htmltools::urlEncodePath(prep_link_escape(as.character(lb))),
                                                 ".", cl)
                                  as.character(htmltools::a(htmltools::HTML(cnt),
                                                            href = href))
                                })
      }
      rm("tb0")
    }

    var_class_col_js_idx <- which(colnames(tb) == "var_class") - 1
    Class_col_js_idx <- which(colnames(tb) == "Class") - 1
    class_raw_col_js_idx <- which(colnames(tb) == "class") - 1
    name_col_js_idx <- which(colnames(tb) == attr(report, "label_col")) - 1

    colnames(tb) <- util_translate(colnames(tb), ns = "dashboard_table")

    my_dashboard <-
      util_html_table(
        tb,
        descs = setNames(rep("", ncol(tb)),
                         colnames(tb)),
        output_format = "HTML",
        searchBuilder = TRUE, # -- var_class not ok and var_class no unclear
        col_tags = list(
          init = util_translate(unique(c(attr(report, "label_col"),
                                         VAR_NAMES,
                                         "Metric",
                                         "value",
                                         "Graph",
                                         "Class",
                                         "Call",
                                         "var_class")), ns = "dashboard_table"),
          all = colnames(tb)
        ),
        initial_col_tag = "init",

        # '{"criteria":[{"condition":"!=","data":"var_class","type":"string","value":["Ok"]},{"condition":"!null","data":"Class","type":"string","value":[]}],"logic":"AND"}'
        # xx <- jsonlite::parse_json(...)
        # cat(deparse(xx), sep = "\n")
        init_search =
          list(criteria = list(list(condition = "!=",
                                    data =
                                      util_translate("var_class",
                                                     ns = "dashboard_table"),
                                    type = "string",
                                    value = list(util_get_labels_grading_class()[["1"]])),
                               list(condition = "!null",
                                    data = util_translate("Class", ns = "dashboard_table"),
                                    type = "string", value = list())), logic = "AND"),
        additional_init_args = list(
          grading_cols = util_translate(unique(c("Class",
                                                 "class",
                                                 "var_class")), ns = "dashboard_table"),
          secondary_order = setNames(list(
            c(var_class_col_js_idx, name_col_js_idx, class_raw_col_js_idx)
          ), nm = util_translate(unique(c("var_class")), ns = "dashboard_table")),
          grading_order = util_get_labels_grading_class(),
          grading_colors = util_get_colors(),
          fg_colors = util_get_fg_color(util_get_colors())
        ) # , this is done in js in report_dt.js:sort_vert_dt() -- if (type == 'sort')
        # additional_columnDefs = list(list(
        #   orderData = c(var_class_col_js_idx, class_raw_col_js_idx),
        #   targets = Class_col_js_idx
        # ))
      )
  } else {
    my_dashboard <- NULL
  }

  if (!is.na(dashboard) && dashboard) {
    htmltools::save_html(my_dashboard,
                         libdir = file.path(content_dir, "lib"),
                         file = content_file)

    fnms <- setNames(list(content_file), nm = paste0("dash-", title))

  } else {
    pages <- util_generate_pages_from_report(report, template,
                                             disable_plotly = disable_plotly,
                                             progress = progress,
                                             progress_msg = progress_msg,
                                             block_load_factor =
                                               block_load_factor,
                                             dir = dir,
                                             my_dashboard = my_dashboard)

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
                                      html_dependency_dataquieR()
    )

    title <- attr(report, "title")
    if (is.null(title)) {
      title <- "Data Quality Report"
    }

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
        paste(as.character(round(difftime(end_time, start_time, units = "mins"), 2)),
              "min"),
        "\"}",
        file = file.path(dir, "renderinfo.js")
    )

    util_hide_file_windows(dir)

    progress_msg(sprintf("Wrote %s", paste0(dQuote(fnms), collapse = ", ")))

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
  if (isTRUE(getOption('knitr.in.progress'))) {
    return(NextMethod())
  }
  if (length(try(unique(
               vapply(x,
                      function(dqr) attr(attr(dqr, "call"), "entity"),
                      FUN.VALUE = character(1))), silent = TRUE)) == 1 &&
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
                  p <- plotly::config(p, responsive = TRUE)
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

# IDEA: For PDF output, support https://cran.r-project.org/web/packages/xmpdf/index.html
