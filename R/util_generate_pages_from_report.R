#' Convert a [dataquieR report v2][dq_report2] to a named list of web pages
#'
#' @param report [dataquieR report v2][dq_report2].
#' @param template [character] template to use, only the name, not the path
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param progress [`function`] lambda for progress in percent -- 1-100
#' @param progress_msg [`function`] lambda for progress messages
#' @param block_load_factor [numeric] multiply size of parallel compute blocks
#'                                    by this factor.
#' @param dir [character] output directory for potential `iframes`.
#' @param my_dashboard [list] of class `shiny.tag.list` featuring a dashboard or
#'                            missing or `NULL`
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
#'
#' @family html
#' @concept process
#' @keywords internal
util_generate_pages_from_report <- function(report, template,
                                            disable_plotly,
                                            progress = progress,
                                            progress_msg = progress_msg,
                                            block_load_factor,
                                            dir,
                                            my_dashboard) {
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
  label_meta_data_hints <- attr(report, "label_meta_data_hints")

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

    curr_url <- htmltools::tagGetAttribute(.menu_env$menu_entry(
      sprintf("%s#%s", file_name, div_name)), "href")

    # https://matthewjamestaylor.com/custom-tags?utm_content=cmp-true
    # e.g. <r-1 curr_url=></r-1> // needs to have a hyphen, attributes in custom tags don't need the prefix data-
    curr_url <- htmltools::tag("dataquier-data", varArgs = list(`curr-url` =
                                                      gsub("\"", "\\\"",
                                                           curr_url,
                                                           fixed = TRUE)))

    sp <- util_attach_attr(htmltools::div(
      ...,
      curr_url,
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
      util_html_table(util_df_escape(attr(report, "label_modification_table")),
                      dl_fn = "Label_modifications")
    )
  } else {
    notes_labels <- htmltools::div()
  }

  # Page 1 ####
  # Technical information about the R session and the report and first Overview
  repsum <- summary(report)
  summaryplots <- plot(repsum, dont_plot = TRUE)
  comat <- print(repsum, dont_print = TRUE)

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
    p[vapply(p, inherits, "POSIXt", FUN.VALUE = logical(1))] <-
      vapply(p[vapply(p, inherits, "POSIXt", FUN.VALUE = logical(1))],
             paste, collapse=" ", FUN.VALUE = character(1))
    p <- p[vapply(p, is.vector, FUN.VALUE = logical(1))]
    p <- p[vapply(p, length, FUN.VALUE = integer(1)) == 1]
    p <- data.frame(`  ` = names(p),
                    ` ` = unlist(unname(p)),
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  }

  # Information on dataset sizes
  vinsd <- attr(report, "study_data_dimnames")[[2]]
  vinmd <- meta_data[[VAR_NAMES]]
  with_md <- intersect(vinsd, vinmd)
  wo_md <- setdiff(vinsd, vinmd)
  if (length(wo_md) > 0) {
    mark <- function(x) {
      paste0("<strong>", x, "</strong")
    }
  } else {
    mark <- identity
  }
  info_sd <-
    data.frame(
      "Number of ..." = c(
        "observations in study data set",
        mark("variables in study data set"),
        mark("variables with item-level metadata")
      ),
      " " = c(
        length(attr(report, "study_data_dimnames")[[1]]),
        mark(as.character(length(vinsd))),
        mark(as.character(length(with_md)))),
      # TODO: Does it suffice to check for VAR_NAMES in the metadata? Should we also check if any valid metadata information apart from the name is given?
      check.names = FALSE)
  info_sd_hover <- info_sd
  info_sd_hover[] <- ""
  if (length(wo_md) > 0) {
    info_sd_hover[2, ] <- paste0(
      "<span style=\"color:#ffffbb\">Variables without metadata: ",
      util_pretty_vector_string(wo_md, n_max = 20),
      "</span>; Variables in study data: ",
      util_pretty_vector_string(vinsd, n_max = 20))
  } else {
    info_sd_hover[2, ] <- util_pretty_vector_string(vinsd, n_max = 20)
  }
  if (length(wo_md) > 0) {
    info_sd_hover[3, ] <- paste0(
      "<span style=\"color:#ffffbb\">Variables without metadata: ",
     util_pretty_vector_string(wo_md, n_max = 20),
     "</span>; Variables with metadata: ",
     util_pretty_vector_string(with_md, n_max = 20))
  } else {
    info_sd_hover[3, ] <- util_pretty_vector_string(with_md, n_max = 20)
  }
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

  ## create a table with the summary of indicators and descriptors
  info_dim_dq <- util_generate_table_indicators_descriptors(report)
  # info_dim_dq <- rbind(info_dim_dq,
  #                      data.frame(Dimension = "Descriptors",
  #                                 `No. DQ indicators` = 0,
  #                                 `No. DQ descriptors` = 2,
  #                                 stringsAsFactors = FALSE,
  #                                 check.names = FALSE
  #                      ))

  # TODO: Discuss the indicator descriptor assignment problem with reference to dimensions
  info_dim_dq[info_dim_dq$Dimension == "Accuracy", "No. DQ descriptors"] <-
    info_dim_dq[info_dim_dq$Dimension == "Accuracy", "No. DQ descriptors"] + 5
  # The 5 descriptors are Location Parameters, Spread, Skewness, Kurtosis, and the graph
  info_dim_dq[info_dim_dq$Dimension == "Completeness", "No. DQ descriptors"] <-
    info_dim_dq[info_dim_dq$Dimension == "Completeness", "No. DQ descriptors"] + 1
  # The descriptor is the columns Missing/Valid

  info_dim_dq[["No. DQ descriptors"]] <- NULL

  append_single_page("General",
                     "Report information",
                     "report.html",
                     htmltools::tagList(
                       htmltools::h1(title),
                       htmltools::h2(subtitle),
                       htmltools::h3("Study data summary"),
                       htmltools::browsable(util_formattable(
                         escape_all_content = FALSE,
                         info_sd,
                         min_color = c(255, 255, 255),
                         max_color = c(255, 255, 255),
                         style_header = c(
                           "font-weight: bold;width: 15em;text-align: left;",
                           "font-weight: bold;width: 6em;"
                         ),
                         hover_texts = info_sd_hover
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
                       htmltools::h2("Data Quality Summary"),
                       summaryplots,
                       htmltools::a(
                         href = "#Item-level data quality summary",
                         "Display Detailed View"),
                       htmltools::h2("Technical information"),
                       htmltools::p("The table below summarizes technical information about this report, the R session and the operating system."),
                       util_html_table(util_df_escape(p),
                                       dl_fn = "Report_Metadata"),
                       htmltools::p(htmltools::tags$i(id = "render-time",
                                                      sprintf("Rendered using %s %s at %s",
                                                              utils::packageName(),
                                                              util_dataquieR_version(),
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
  # TODO apmat_not_segments_and_other_non_item_levels
  # TODO: add some hints if there is an integrity issue
  append_single_page("General",
                     "Item-level data quality summary",
                     "report.html",
                     htmltools::htmlTemplate(
                       text_ = readLines(system.file("templates", template, "overview.html",
                                                     package = utils::packageName())),
                       comat = comat,
                       summaryplots = summaryplots,
                       # ismat = ismat,
                       # apmat = apmat,
                       # ermat = ermat,
                       # anamat = anamat,
                       notes_labels = notes_labels,
                       util_float_index_menu = util_float_index_menu,
                       util_map_labels = util_map_labels,
                       util_get_concept_info = util_get_concept_info,
                       util_abbreviate = util_abbreviate)
  )

  if (!missing(my_dashboard) && inherits(my_dashboard, "shiny.tag.list")) {
    append_single_page("General",
                       "Item-level data quality dashboard",
                       "dashboard.html",
                       my_dashboard)
  }

# Removed all Venn diagram section
  # generate Venn diagram
#  v_in_m <- meta_data[[VAR_NAMES]]
#  v_in_s <- attr(report, "study_data_dimnames")[[2]] # dimnames [[1]] is missing
#  if (util_ensure_suggested("ggvenn",
#                        goal =
#                "display Venn diagrams about the meta_data/study_data coverage",
#                err = FALSE)) {

#    overlap <-
#      plot_figure(ggvenn::ggvenn(
#        list(`Item-level metadata` = v_in_m, `Study data` = v_in_s),
#        set_name_size = 4,
#        text_size = 3,
#        stroke_size = 0.5))
#  } else {
#    overlap <-
#      util_html_table(data.frame(
#        check.names = FALSE,
#        ` ` = c("Study Variables with metadata",
#                "Study Variables w/o metadata",
#                "Variables from DD w/o Study Data"),
#        `#` = c(length(intersect(v_in_m, v_in_s)),
#                length(setdiff(v_in_s, v_in_m)),
#                length(setdiff(v_in_m, v_in_s)))
#      ),
#      meta_data = meta_data,
#      label_col = label_col,
#      dl_fn = "StudyMetaDataOverlap",
#      output_format = "HTML")
#  }

 # append_single_page("General",
 #                   "Metadata and study data mapping",
 #                   "report.html",
 #                   htmltools::h1("Metadata and Study Data Mapping"),
 #                   overlap)

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
    block_load_factor = block_load_factor,
    repsum = repsum,
    dir = dir
  )

  rendered_repsum <- comat

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
    vars_in_chunk <- intersect(
      vars_in_chunk,
      util_map_labels(
        ifnotfound = NA_character_,
        attr(report, "study_data_dimnames")[[2]],
        attr(report, "meta_data"),
        to = attr(report, "label_col"))
    )
    vars_in_chunk <- vars_in_chunk[!is.na(vars_in_chunk)]
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
          note_meta = warn_pred_meta,
          rendered_repsum = rendered_repsum,
          dir = dir)
      }
    )
    chunk_of_pages <- do.call(`c`, chunk_of_pages)
    for (args in chunk_of_pages) {
      do.call(append_single_page, args)
    }
    i <- i + length(vars_in_chunk)
    progress(i/n * 100)
  }

  meta_data_frames <-
    grep("^meta_data", names(attributes(report)), value = TRUE)

  meta_data_titles <- c(
    meta_data_segment = "Segment-level metadata",
    meta_data_dataframe = "Dataframe-level metadata",
    meta_data_cross_item = "Cross-item-level metadata",
    meta_data = "Item-level metadata",
    meta_data_item_computation = "Item-computation-level metadata"
  )

  for (mdn in meta_data_frames) {
    xlmd <- attr(report, mdn) # x level metadata

    if (mdn == "meta_data") {
      # these two columns should have been replaced by the v1->v2 conversion of the metadata in
      # prep_meta_data_v1_to_item_level_meta_data (.util_internal_normalize_meta_data)
      xlmd[[MISSING_LIST]] <- NULL
      xlmd[[JUMP_LIST]] <- NULL

      # there can be specific notes for item-level metadata attributes
      if (length(label_meta_data_hints)) {
        meta_data_h <- force(htmltools::tagList(
          htmltools::div( # TODO: warnings should be easy to understand if they appear in the report, and stand out while still matching the overall style of the report
            # style = htmltools::css(
            #   `background-color` = "#aaaaaa",
            #   width = "80%",
            #   left = "10%",
            #   position = "relative"
            # ),
            htmltools::tags$em(htmltools::pre(
              style = htmltools::css(
                `white-space` = "pre-wrap"
              ),
              do.call(
                paste,
                c(list(collapse = "\n"),
                  lapply(lapply(label_meta_data_hints, conditionMessage),
                         paste, collapse = "\n")))
            )))))
        label_meta_data_hints <- meta_data_h
      } else {
        label_meta_data_hints <- NULL
      }
    } else {
      label_meta_data_hints <- NULL
      # TODO: include warnings from util_normalize_cross_item on the cross-item metadata level page
    }

    xlmd[, !grepl("_TABLE$", colnames(xlmd))] <-
      util_df_escape(xlmd[, !grepl("_TABLE$", colnames(xlmd)),
                                drop = FALSE])

    for (cn in grep("_TABLE$", colnames(xlmd), value = TRUE)) {

      xlmd[!util_empty(xlmd[[cn]]), cn] <- vapply(FUN.VALUE = character(1),
                                                  xlmd[!util_empty(xlmd[[cn]]), cn],
                                                  FUN = function(tn) {
                                                    paste(as.character(htmltools::a(href = paste0(
                                                      prep_link_escape(tn, html = TRUE), ".html"), tn)),
                                                          collapse = "")
                                                  }
      )

    }

    # Hover text for headers of metadata tables
    text_to_display <- util_get_hovertext(mdn)

    # filter text_to_display only for headers actually present in the metadata
    text_to_display <- text_to_display[names(text_to_display)  %in% names(xlmd)]

    attr(xlmd, "description") <- text_to_display


    meta_data_table <- util_html_table(
      xlmd, # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
      meta_data = meta_data,
      label_col = label_col,
      dl_fn = mdn,
      output_format = "HTML", # needed for generating plain html, "RMD" would generate a mix between Rmd and html
      col_tags =
        lapply(lapply(setNames(nm = VARATT_REQUIRE_LEVELS_ORDER),
               util_get_var_att_names_of_level,  cumulative = FALSE),
               function(x) {
                 union(VAR_NAMES, x)
               }
        )
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

    append_single_page("Metadata",
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
                         label_meta_data_hints = label_meta_data_hints,
                         meta_data_table = meta_data_table
                       )
    )
  }

  referred_tables <- attr(report, "referred_tables")
  if (length(referred_tables)) { # show all tables referred to by the report
    for (reftab in names(referred_tables)) {
      if (nrow(referred_tables[[reftab]]) > 1000) {
        append_single_page("Metadata",
                           paste("Table", sQuote(reftab)),
                           paste0(prep_link_escape(reftab, html = TRUE), ".html"),
                           htmltools::h1(dQuote(reftab)),
                           util_html_table(
                             util_df_escape(head(referred_tables[[reftab]], 1000)), # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
                             meta_data = meta_data,
                             label_col = label_col,
                             dl_fn = reftab,
                             output_format = "HTML" # needed for generating plain html, "RMD" would generate a mix between Rmd and html
                           ),
                           htmltools::tags$hr(),
                           htmltools::p(
                             sprintf("Showing only the first 1000 rows of a table with %d rows",
                                     nrow(referred_tables[[reftab]]))
                           )
        )
      } else {
        append_single_page("Metadata",
                           paste("Table", sQuote(reftab)),
                           paste0(prep_link_escape(reftab, html = TRUE), ".html"),
                           htmltools::h1(dQuote(reftab)),
                           util_html_table(
                             util_df_escape(referred_tables[[reftab]]), # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
                             meta_data = meta_data,
                             label_col = label_col,
                             dl_fn = reftab,
                             output_format = "HTML" # needed for generating plain html, "RMD" would generate a mix between Rmd and html
                           )
        )
      }
    }
  }


  rsts <- util_get_rule_sets()
  for (rstsn in names(rsts)) {
    ..tb <- util_df_escape(rsts[[rstsn]])
    if ("indicator_metric" %in% colnames(..tb)) {
      ..tb[["indicator_metric"]] <- lapply(..tb[["indicator_metric"]],
                                         function(im) {
                                           as.character(
                                             htmltools::span(
                                               title = im, # TODO: for downloads in Excel by datatables.js, use an alternative technical column
                                               util_translate_indicator_metrics(im)
                                               ))
                                         })
    }

    #add hover text to headers of table Grading ruleset
    text_to_display <- util_get_hovertext("grading_rulesets")
    attr(..tb, "description") <- text_to_display

    append_single_page("Metadata",
                       paste("Grading Ruleset", dQuote(rstsn)),
                       paste0("rulesets.html"),
                       htmltools::htmlTemplate(
                         system.file("templates", template,
                                     paste0("grading_ruleset_text.html"),
                                     package = utils::packageName()),
                         meta_data_name = "grading_rulesets",
                         meta_data_title =
                           "Grading Rulesets",
                         meta_data_table = util_html_table(..tb)
                       )
    )
  }

  ..tb <- util_df_escape(util_get_ruleset_formats())
  if ("color" %in% colnames(..tb)) {
    ..tb[["color"]] <- lapply(..tb[["color"]],
                                         function(cl) {
                                           as.character(
                                             htmltools::span( # TODO: for downloads in Excel by datatables.js, use an alternative technical column
                                               style = htmltools::css(
                                                 background_color =
                                                   util_col2rgb(cl),
                                                 color =
                                                   util_get_fg_color(
                                                     util_col2rgb(cl))
                                               ),
                                               cl
                                             ))
                                         })
  }

  #add hover text to headers of table Ruleset formats
  text_to_display <- util_get_hovertext("grading_formats")
  attr(..tb, "description") <- text_to_display


  append_single_page("Metadata",
                     "Ruleset Formats",
                     paste0("ruleset_formats.html"),
                     htmltools::htmlTemplate(
                       system.file("templates", template,
                                   paste0("ruleset_formats_text.html"),
                                   package = utils::packageName()),
                       meta_data_name = "grading_rulesets",
                       meta_data_title =
                         "Ruleset Formats",
                       meta_data_table =
                         util_html_table(..tb)
                     )
  )


  pages
}
