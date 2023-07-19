#' Convert a [dataquieR report v2][dq_report2] to a named list of web pages
#'
#' @param report [dataquieR report v2][dq_report2].
#' @param template [character] template to use, only the name, not the path
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param progress [`function`] lambda for progress in percent -- 1-100
#' @param progress_msg [`function`] lambda for progress messages
#' @param block_load_factor [numeric] multiply size of parallel compute blocks
#'                                    by this factor.
#'
#' @return named list, each entry becomes a file with the name of the entry.
#'         the contents are `HTML` objects as used by `htmltools`.
#' @examples
#' \dontrun{
#' devtools::load_all()
#' prep_load_workbook_like_file("meta_data_v2")
#' report <- dq_report2("study_data", dimensions = NULL, label_col = "LABEL");
#' save(report, file = "report_v2.RData")
#' report <- dq_report2("study_data", label_col = "LABEL");
#' save(report, file = "report_v2_short.RData")
#' }
util_generate_pages_from_report <- function(report, template,
                                            disable_plotly,
                                            progress = progress,
                                            progress_msg = progress_msg,
                                            block_load_factor) {
  util_ensure_suggested(pkg = c("htmltools"),
                                goal = "generate interactive HTML-reports.")
  have_plot_ly <- util_ensure_suggested(pkg = c("plotly"),
                                        goal = "generate interactive figures in plain HTML-reports.",
                                        err = FALSE)
  if (disable_plotly) have_plot_ly <- FALSE

  if (have_plot_ly) {
    plot_figure <- util_plot_figure_plotly
  } else {
    plot_figure <- util_plot_figure_no_plotly
  }

  vars_in_rep <- rownames(report)

  meta_data <- attr(report, "meta_data")
  label_col <- attr(report, "label_col")
  warn_pred_meta <- attr(report, "warning_pred_meta")
  title <- attr(report, "title")
  subtitle <- attr(report, "subtitle")

  pages <- list()
  call_env <- environment()

  append_single_page <- function(drop_down_to_attach, # main menu entry (first-level)
                                 div_name, # sub-menu entry (second-level)
                                 file_name, # name where the page is stored, it is possible to add more than one page to a file
                                 ...) { # ... are the contents in htmltools compatible objects
    # Setup ####
    if (file_name %in% names(call_env$pages)) {
      fil <- call_env$pages[[file_name]]
    } else {
      fil <- list()
    }
    all_ids <- unlist(lapply(call_env$pages, names))
    if (div_name %in% all_ids) {
      util_error("Cannot create report, single page with ID %s already exists",
                 dQuote(div_name))
    }
    sp <- util_attach_attr(htmltools::div(
      ...,
      class = "singlePage",
      id = div_name
    ), dropdown = drop_down_to_attach)
    fil[[div_name]] <- sp
    call_env$pages[[file_name]] <- fil
    invisible(NULL)
  }

  # Note on changed labels
  if (nchar(attr(report, "label_modification_text")) > 0) {
    notes_labels <- htmltools::div(
      htmltools::h3("Label modifications"),
      util_html_table(attr(report, "label_modification_table"),
                      dl_fn = "Label_modifications")
    )
  } else {
    notes_labels <- htmltools::div()
  }

  # Page 1 ####
  # Technical information about the R session and the report
  properties <- attr(report, "properties")
  if (!is.list(properties)) {
    properties <- list(error = "No report properties found, report file corrupted?")
  }

  if (is.list(properties)) {
    # TODO: jsTree
    p <- properties
    p[vapply(p, is.call, FUN.VALUE = logical(1))] <-
      vapply(lapply(p[vapply(p, is.call, FUN.VALUE = logical(1))], deparse),
             paste, collapse = "\n", FUN.VALUE = character(1))
    p <- p[vapply(p, is.vector, FUN.VALUE = logical(1))]
    p <- p[vapply(p, length, FUN.VALUE = integer(1)) == 1]
    p <- data.frame(`  ` = names(p),
                    ` ` = unlist(unname(p)),
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  }

  # Information on dataset sizes
  info_sd <-
    data.frame(
      "Number of ..." = c(
        "observations in study data set",
        "variables in study data set",
        "variables with item-level metadata"
      ),
      " " = c(
        length(attr(report, "study_data_dimnames")[[1]]),
        length(attr(report, "study_data_dimnames")[[2]]),
        length(intersect(
          attr(report, "study_data_dimnames")[[2]],
          meta_data[[VAR_NAMES]]))
        ),
      # TODO: Does it suffice to check for VAR_NAMES in the metadata? Should we also check if any valid metadata information apart from the name is given?
      check.names = FALSE
    )
  info_md <-
    data.frame(
      "Number of ..." = "variables in item-level metadata",
      " " = nrow(meta_data),
      check.names = FALSE
    )
  if ("meta_data_segment" %in% names(attributes(report)) &&
      nrow(attr(report, "meta_data_segment")) > 0) {
    info_md <-
      rbind(info_md, list("segments", nrow(attr(
        report, "meta_data_segment"
      ))))
  }

  # number of indicators and descriptors per dimension
  dqi <- util_get_concept_info("dqi")
  dq_ind_or_desc <- data.frame(
    "Abbreviation" =
      unique(dqi$abbreviation[which(!is.na(dqi$`dataquieR function`))]))
  dq_ind_or_desc$Dimension <- dims[substr(dq_ind_or_desc$Abbreviation, 1, 3)]
  list_dqi_in_report <- lapply(
    report,
    function(result) {
      res_out <- NULL
      # check which data quality indicators are targeted by this function
      # using the dq control table
      dqi_abr <- dqi$abbreviation[
        grepl(attr(result, "function_name"), dqi$`dataquieR function`)]
      # check all result objects for abbreviated data quality indicators in
      # their header
      obj_to_scan <- names(result)[vapply(result, function(res_obj) {
        is.data.frame(res_obj) && nrow(res_obj) > 0 &&
          any(vapply(dq_ind_or_desc$Abbreviation,
                     function(abr) {
                       any(grepl(abr, colnames(res_obj)))
                     }, FUN.VALUE = logical(1)))
      }, FUN.VALUE = logical(1))]
      # If there are some, report only those indicators that are included in the
      # outputs (SummaryTable, DataframeTable, etc.).
      if (length(obj_to_scan) > 0) {
        res_out <- unlist(lapply(obj_to_scan, function(res) {
          dqi_abr <- dq_ind_or_desc$Abbreviation[
            vapply(dq_ind_or_desc$Abbreviation,
                   function(abr) {
                     any(grepl(abr, colnames(result[[res]])))
                   },
                   FUN.VALUE = logical(1))]
          if (any(grepl("GRADING", colnames(result[[res]])))) {
            grad_col <- grep("GRADING", colnames(result[[res]]))
            if (all(is.na(result[[res]][, grad_col]))) {
              dqi_res <- "descriptor"
            } else {
              dqi_res <- "indicator"
            }
          } else {
            dqi_res <- "descriptor"
          }
          setNames(rep(dqi_res, length(dqi_abr)), dqi_abr)
        }))
      } else if (length(dqi_abr) > 0) {
        # check all result objects for a GRADING column in
        # their header
        obj_w_grad <- names(result)[vapply(result, function(res_obj) {
          is.data.frame(res_obj) && nrow(res_obj) > 0 &&
            any(grepl("GRADING", colnames(res_obj)))
        }, FUN.VALUE = logical(1))]
        if (length(obj_w_grad) > 0) {
          res_out <- unlist(lapply(obj_w_grad, function(res) {
            if ("GRADING" %in% colnames(result[[res]])) {
              if (all(is.na(result[[res]][, "GRADING"]))) {
                dqi_res <- "descriptor"
              } else {
                dqi_res <- "indicator"
              }
            } else {
              dqi_res <- "descriptor"
            }
            setNames(rep(dqi_res, length(dqi_abr)), dqi_abr)
          }))
        } else if ("ReportSummaryTable" %in% names(result) &&
            nrow(result$ReportSummaryTable) > 0 |
            "SummaryPlot" %in% names(result) && !inherits(result$SummaryPlot, "dataquieR_NULL") |
            "SummaryPlotList" %in% names(result) && !inherits(result$SummaryPlotList, "dataquieR_NULL")
        ) {
          res_out <- c(res_out,
                       setNames(rep("descriptor", length(dqi_abr)), dqi_abr))
        }
      }
      return(res_out)
    }
  )
  dqi_short <- unlist(unname(list_dqi_in_report))
  dq_ind_or_desc$Indicator <- FALSE
  dq_ind_or_desc$Indicator[
    which(dq_ind_or_desc$Abbreviation %in%
            unique(names(dqi_short)[dqi_short == "indicator"]))] <- TRUE
  dq_ind_or_desc$Descriptor <- FALSE
  dq_ind_or_desc$Descriptor[
    which(dq_ind_or_desc$Abbreviation %in%
            unique(names(dqi_short)[dqi_short == "descriptor"]))] <- TRUE
  if (util_ensure_suggested("summarytools",
                            goal =
                            "descriptive summary statistics in the report",
                            err = FALSE) &&
      inherits(attr(report, "summary_stat"), "summarytools") &&
      inherits(attr(report, "summary_descr"), "summarytools")) {
    dq_ind_or_desc$Descriptor[match(c("acc_ud_shape", "acc_ud_scale"), dq_ind_or_desc$Abbreviation)] <- TRUE
  }

  ind_per_dim <- tapply(dq_ind_or_desc$Indicator,
                        dq_ind_or_desc$Dimension,
                        sum)[dims]
  desc_per_dim <- tapply(dq_ind_or_desc$Descriptor,
                         dq_ind_or_desc$Dimension,
                         sum)[dims]

  info_dim_dq <- data.frame("Dimension" = unname(dims),
                            "Number of data quality indicators" = ind_per_dim,
                            "Number of data quality descriptors" = desc_per_dim,
                            check.names = FALSE)

  append_single_page("General",
                     "Report information",
                     "report.html",
                     htmltools::tagList(
                       htmltools::h1(title),
                       htmltools::h2(subtitle),
                       htmltools::h3("Study data summary"),
                       htmltools::browsable(util_formattable(
                         info_sd,
                         min_color = c(255, 255, 255),
                         max_color = c(255, 255, 255),
                         style_header = c(
                           "font-weight: bold;width: 15em;text-align: left;",
                           "font-weight: bold;width: 6em;"
                         )
                       )),
                       htmltools::h3("Metadata summary"),
                       htmltools::p(warn_pred_meta),
                       htmltools::browsable(util_formattable(
                         info_md,
                         min_color = c(255, 255, 255),
                         max_color = c(255, 255, 255),
                         style_header = c(
                           "font-weight: bold;width: 15em;text-align: left;",
                           "font-weight: bold;width: 6em;"
                         )
                       )),
                       htmltools::h2("Scope of the data quality assessment"),
                       htmltools::browsable(util_formattable(
                         info_dim_dq,
                         min_color = c(255, 255, 255),
                         max_color = c(33, 102, 172),
                         style_header = paste0(
                           "font-weight: bold;text-align: right;",
                           c("width: 6em;", "width: 12em;", "width: 12em;")
                         )
                       )),
                       htmltools::h2("About this report"),
                       htmltools::p("The table below summarizes technical information about this report, the R session and the operating system."),
                       util_html_table(p,
                                       dl_fn = "Report_Metadata"),
                       htmltools::p(htmltools::tags$i(id = "render-time",
                                                      sprintf("Rendered using %s %s at %s",
                                                              utils::packageName(),
                                                              as.character(packageVersion(
                                                                utils::packageName())),
                                                              as.character(Sys.time()))
                       )),
                       htmltools::tags$script( # https://stackoverflow.com/a/34579496
                         '$(function() {
                              var xx = $("#render-time").html()
                              var data = window.renderingData
                              if (data.hasOwnProperty("renderingTime")) {
                                $("#render-time").html(xx + " in " + data.renderingTime)
                              } else {
                                $("#render-time").html(xx + " -- no rendering time available.")
                              }
                              console.log(data);
                          });'),
                       htmltools::hr(),
                       htmltools::h2("Related literature"),
                       do.call(htmltools::tagList, lapply(format(
                         utils::readCitationFile(system.file("CITATION",
                                                             package = utils::packageName()),
                                                 list(Encoding = "UTF-8")),
                         style="HTML"),
                         htmltools::HTML)),
                       htmltools::tags$button(class = "clipbtn",
                                              `data-clipboard-text` = paste(
                                                format(
                                                  utils::readCitationFile(
                                                    system.file("CITATION",
                                                                package =
                                                                  utils::packageName()),
                                                    list(Encoding = "UTF-8")),
                                                  style="bibtex"),
                                                collapse = "\n\n"),
                                              "\U1F4CB BibTeX",
                                              title = "copy BibTeX to clipboard")
                     ))

  # Page 2 ####
  apmat <-
    util_html_table(summary(report, aspect = "applicability", FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = meta_data,
                    label_col = label_col,
                    dl_fn = "Applicability_Matrix",
                    output_format = "HTML"
  )

  ismat <-
    util_html_table(summary(report, aspect = "issue", FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = meta_data,
                    label_col = label_col,
                    dl_fn = "Issue_Matrix",
                    output_format = "HTML"
  )

  # TODO ismat_indicator_based_and_with_metrics
  # TODO apmat_not_segments_and_other_non_item_levels

  ermat <-
    util_html_table(summary(report, aspect = "error", FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = meta_data,
                    label_col = label_col,
                    dl_fn = "Error_Matrix",
                    output_format = "HTML"
  )

  anamat <-
    util_html_table(summary(report, aspect = "anamat",
                            FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = meta_data,
                    label_col = label_col,
                    dl_fn = "Error_Matrix",
                    output_format = "HTML"
    )

  # TODO: add some hints if there is an integrity issue
  append_single_page("General",
                     "Item-level data quality summary",
                     "report.html",
                     htmltools::htmlTemplate(
                       text_ = readLines(system.file("templates", template, "overview.html",
                                                     package = utils::packageName())),
                       ismat = ismat,
                       apmat = apmat,
                       ermat = ermat,
                       anamat = anamat,
                       notes_labels = notes_labels,
                       util_float_index_menu = util_float_index_menu,
                       util_map_labels = util_map_labels,
                       util_get_concept_info = util_get_concept_info,
                       util_abbreviate = util_abbreviate)
  )


  meta_data_frames <-
    grep("^meta_data", names(attributes(report)), value = TRUE)

  meta_data_titles <- c(
    meta_data_segment = "Segment-level metadata",
    meta_data_dataframe = "Dataframe-level metadata",
    meta_data_cross_item = "Cross-item-level metadata",
    meta_data = "Item-level metadata"
  )

  for (mdn in meta_data_frames) {

    xlmd <- attr(report, mdn) # x level metadata

    if (mdn == "meta_data") {
      # these two columns should have been replaced by the v1->v2 conversion of the metadata in
      # prep_meta_data_v1_to_item_level_meta_data (.util_internal_normalize_meta_data)
      xlmd[[MISSING_LIST]] <- NULL
      xlmd[[JUMP_LIST]] <- NULL
    }

    for (cn in grep("_TABLE$", colnames(xlmd), value = TRUE)) {

      xlmd[!util_empty(xlmd[[cn]]), cn] <- vapply(FUN.VALUE = character(1),
        xlmd[!util_empty(xlmd[[cn]]), cn],
        FUN = function(tn) {
          paste(as.character(htmltools::a(href = paste0(tn, ".html"), tn)),
                collapse = "")
        }
      )

    }

    meta_data_table <- util_html_table(
      xlmd, # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
      meta_data = meta_data,
      label_col = label_col,
      dl_fn = mdn,
      output_format = "HTML" # needed for generating plain html, "RMD" would generate a mix between Rmd and html
    )

    tmpl <- system.file("templates", template, paste0(mdn, ".html"),
                        package = utils::packageName())
    if (!file.exists(tmpl)) {
      tmpl <- system.file("templates", template,
                          paste0("generic_meta_data.html"),
                          package = utils::packageName())
    }

    meta_data_title <- meta_data_titles[[mdn]]
    if (length(meta_data_title) != 1 ||
        !is.character(meta_data_title) ||
        util_empty(meta_data_title)) {
      meta_data_title <- mdn
    }

    if (length(meta_data_table) == 0) {
      meta_data_table <- htmltools::p(paste(meta_data_title,
                                            "were not provided."))
    }

    append_single_page("General",
                       meta_data_title,
                       paste0(mdn, ".html"),
                       htmltools::htmlTemplate(
                         tmpl,
                         meta_data_name = mdn,
                         meta_data_title =
                           # Use upper case for all words, not only the first one
                           gsub("(^|[[:space:]]|[[:punct:]])([[:alpha:]])",
                                "\\1\\U\\2",
                                meta_data_title,
                                perl = TRUE),
                         meta_data_table = meta_data_table
                         )
    )
  }

  referred_tables <- attr(report, "referred_tables")
  if (length(referred_tables)) { # show all tables referred to by the report
    for (reftab in names(referred_tables)) {
      append_single_page("General",
                         paste("Table", sQuote(reftab)),
                         paste0(reftab, ".html"),
                         htmltools::h1(dQuote(reftab)),
                         util_html_table(
                           referred_tables[[reftab]], # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
                           meta_data = meta_data,
                           label_col = label_col,
                           dl_fn = reftab,
                           output_format = "HTML" # needed for generating plain html, "RMD" would generate a mix between Rmd and html
                         )
      )
    }
  }




  # generate Venn diagram
  v_in_m <- meta_data[[VAR_NAMES]]
  v_in_s <- attr(report, "study_data_dimnames")[[2]] # dimnames [[1]] is missing
  if (util_ensure_suggested("ggvenn",
                        goal =
                "display Venn diagrams about the meta_data/study_data coverage",
                err = FALSE)) {

    overlap <-
      plot_figure(ggvenn::ggvenn(
        list(`Item-level metadata` = v_in_m, `Study data` = v_in_s),
        set_name_size = 4,
        text_size = 3,
        stroke_size = 0.5))
  } else {
    overlap <-
      util_html_table(data.frame(
        check.names = FALSE,
        ` ` = c("Study Variables with metadata",
                "Study Variables w/o metadata",
                "Variables from DD w/o Study Data"),
        `#` = c(length(intersect(v_in_m, v_in_s)),
                length(setdiff(v_in_s, v_in_m)),
                length(setdiff(v_in_m, v_in_s)))
      ),
      meta_data = meta_data,
      label_col = label_col,
      dl_fn = "StudyMetaDataOverlap",
      output_format = "HTML")
  }

  append_single_page("General",
                     "Metadata and study data mapping",
                     "report.html",
                     htmltools::h1("Metadata and Study Data Mapping"),
                     overlap)

  if (util_ensure_suggested("summarytools",
                            goal =
                            "descriptive summary statistics in the report",
                            err = FALSE) &&
      inherits(attr(report, "summary_stat"), "summarytools") &&
      inherits(attr(report, "summary_descr"), "summarytools")) {

    summary_stat <- util_html_table(attr(report, "summary_stat"),
                                    meta_data = meta_data,
                                    label_col = label_col,
                                    dl_fn = "summary_stat",
                                    output_format = "HTML")
    summary_descr <- util_html_table(attr(report, "summary_descr"),
                                    meta_data = meta_data,
                                    label_col = label_col,
                                    dl_fn = "summary_descr",
                                    output_format = "HTML")

    append_single_page("General",
                       "Descriptive statistics",
                       "report.html",
                       htmltools::htmlTemplate(
                         text_ = readLines(system.file("templates", template, "descriptive.html",
                                                       package = utils::packageName())),
                         summary_stat = summary_stat,
                         summary_descr = summary_descr,
                         util_float_index_menu = util_float_index_menu,
                         util_map_labels = util_map_labels,
                         util_get_concept_info = util_get_concept_info,
                         util_abbreviate = util_abbreviate)
    )
  }

  # rfile <- tempfile()
  # prep_save_report(report, file = rfile)
  # sfile = tempfile()
  # cat(sprintf("report <- prep_load_report(\"%s\")\n", rfile), file = sfile)
  if (!getOption("parallelMap.mode", "local") %in% c("local", "multicore")) {
    util_error(c("On a non-local/multicore cluster,",
                 "parallel rendering of reports is not supported.",
                 "Please call %s before",
                 "rendering the report."),
               dQuote("parallelMap::parallelStop()")
    )
    # IDEA: Addd to DESCRIPTION:
    # biocViews: Software
    # and
    # Suggests: SharedObject
    # BUT: This is not working, the package in unstable and has trouble with
    # the ulimti/stack size
    # if (util_ensure_suggested("SharedObject", "render reports in parallel",
    #                           err = FALSE)) {
    #   r <- report
    #   rm(report)
    #   gc(verbose = FALSE, full = TRUE)
    #   util_message("Sharing report with the workers")
    #   report <- SharedObject::share(r)
    # } else {
    #   util_error(c("On a non-local/multicore cluster,",
    #                "parallel rendering of reports is not supported",
    #                "without having %s installed. Please call %s before",
    #                "rendering the report, or install %s from %s."),
    #              dQuote("SharedObject"),
    #              dQuote("parallelMap::parallelStop()"),
    #              dQuote("SharedObject"),
    #              dQuote(
    # "http://www.bioconductor.org/packages/release/bioc/html/SharedObject.html")
    #              )
    # }
  }

  suppressWarnings(parallelMap::parallelExport("report", "have_plot_ly", "template"))
  parallelMap::parallelLibrary(utils::packageName())
  # parallelMap::parallelSource(sfile, master = FALSE)

# TODO: if https://community.plotly.com/t/cant-show-heatmap-inside-div-error-something-went-wrong-with-axis-scaling/30616/9 is fixed, use auto-sizing

  dim_pages <- util_html_for_dims(
    report,
    use_plot_ly = have_plot_ly,
    template = template,
    block_load_factor = block_load_factor
  )

  dim_pages <- dim_pages[vapply(dim_pages, length, FUN.VALUE = integer(1))
                         != 0]

  progress_msg("Page generation", "Mounting indicator pages")
  i <- 0
  n <- dim(dim_pages)
  for (args in dim_pages) {
    do.call(append_single_page, args) # creates the indicator related pages because cur_var has not yet been set
    progress(i/n * 100)
  }

  util_setup_rstudio_job("Page generation: Single Variable View")

  progress_msg("Page generation", "Generating single variable pages")

  i <- 0
  n <- nrow(report)

  cores <- getOption("parallelMap.cpus", 1)
  if (!is.numeric(cores) || !util_is_integer(cores) || !is.finite(cores)) {
    cores <- 1
  }
  block_size <- block_load_factor * cores
  nblocks <- ceiling(nrow(report) / block_size)
  for (cur_block in seq_len(nblocks) - 1) { # create the single variable pages
    block_indices <- seq(1 + cur_block * block_size,
                         min(cur_block * block_size + block_size,
                                              nrow(report)))
    vars_in_chunk <- rownames(report)[block_indices]
    progress(i/n * 100)
    progress_msg("Page generation", sprintf("Single Variables %s",
                                            paste(sQuote(vars_in_chunk),
                                                  collapse = ", ")))
    chunk_of_pages <- parallelMap::parallelLapply(
      vars_in_chunk,
      function(cur_var) {
        util_html_for_var(
          report,
          cur_var = cur_var,
          use_plot_ly = have_plot_ly,
          template = template,
          note_meta = warn_pred_meta)
      }
    )
    chunk_of_pages <- do.call(`c`, chunk_of_pages)
    for (args in chunk_of_pages) {
      do.call(append_single_page, args)
    }
    i <- i + length(vars_in_chunk)
    progress(i/n * 100)
  }

# browser()
  pages
}
