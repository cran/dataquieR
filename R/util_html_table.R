#' The jack of all trades device for tables
#'
#' @param ... passed to `DT::datatable`
#' @param tb the table as [data.frame]
#' @param columnDefs column specifications for the `datatables` JavaScript
#'                   object
#' @param autoWidth passed to the `datatables` JavaScript library
#' @param hideCols columns to hide (by name)
#' @param rowCallback passed to the `datatables` JavaScript library
#'                    (with default)
#' @param copy_row_names_to_column add a column 0 with `rownames`
#' @param tb_rownames number of columns from the left considered as row-names
#' @param meta_data the data dictionary for labels and similar stuff
#' @param options individually overwrites defaults in `options`
#'                passed to `DT::datatable`
#' @param link_variables considering row names being variables, convert
#'                      row names to links to the variable specific reports
#' @param rotate_headers rotate headers by 90 degrees
#' @param colnames column names for the table (defaults to `colnames(tb)`)
#' @param is_matrix_table create a heat map like table without padding
#' @param filter passed to `DT::datatable`
#' @param colnames_aliases2acronyms abbreviate column names considering being
#'                                  analysis matrix columns by their acronyms
#'                                  defined in square.
#' @param label_col label col used for mapping labels in case of
#'                  `link_variables` is used (that argument set to `TRUE` and
#'                  `Variables` or `VAR_NAMES` in `meta_data`)
#' @param output_format target format `RMD` or `HTML`, for `RMD`, markdown will
#'        be used in the output, for `HTML`, only `HTML` code is being generated
#' @param fillContainer see `DT::datatable`
#' @param dl_fn file name for downloaded table -- see
#'      [https://datatables.net/reference/button/excel](https://datatables.net/reference/button/excel)
#' @param cols_are_indicatormetrics [logical] cannot be `TRUE`,
#'        `colnames_aliases2acronyms` is `TRUE`. `cols_are_indicatormetrics`
#'        controls, if the columns are really function calls or, if
#'        `cols_are_indicatormetrics` has been set to `TRUE`, the columns are
#'        indicator metrics.
#' @param rotate_for_one_row [logical] rotate one-row-tables
#' @param descs [character] descriptions of the columns for the hover-box shown
#'                          for the column names, if not missing, this overrides
#'                          the existing description stuff from known column
#'                          names. If you have an attribute "description" of the `tb`, then it
#'                          overwrites everything and appears as hover text
#' @param title [character] title for download formats, see
#'        [https://datatables.net/extensions/buttons/examples/html5/titleMessage.html](https://datatables.net/extensions/buttons/examples/html5/titleMessage.html)
#' @param messageTop  [character] subtitle for download formats, see
#'        [https://datatables.net/extensions/buttons/examples/html5/titleMessage.html](https://datatables.net/extensions/buttons/examples/html5/titleMessage.html)
#' @param messageBottom  [character] footer for download formats, see
#'        [https://datatables.net/extensions/buttons/examples/html5/titleMessage.html](https://datatables.net/extensions/buttons/examples/html5/titleMessage.html)
#' @return the table to be added to an `rmd`/´`html` file as
#'         [htmlwidgets::htmlwidgets]
#' @seealso [util_formattable()]
#'
#' @family summary_functions
#' @concept html
#' @keywords internal
#'
util_html_table <- function(tb,
                  filter = "top",
                  columnDefs = NULL,
                  autoWidth = FALSE,
                  hideCols = character(0),
                  rowCallback = DT::JS("function(r,d) {$(r).attr('height', '2em')}"),
                  copy_row_names_to_column = !is.null(tb) &&
                    length(rownames(tb)) == nrow(tb) &&
                    !is.integer(attr(tb, "row.names")) && !all(seq_len(nrow(tb))
                                                               == rownames(tb)),
                  link_variables = TRUE,
                  tb_rownames = FALSE,
                  meta_data,
                  rotate_headers = FALSE,
                  fillContainer = TRUE,
                  ...,
                  colnames,
                  descs,
                  options = list(),
                  is_matrix_table = FALSE,
                  colnames_aliases2acronyms = is_matrix_table &&
                    !cols_are_indicatormetrics,
                  cols_are_indicatormetrics = FALSE,
                  label_col = LABEL,
                  output_format = c("RMD", "HTML"), # TODO: Order bny VAR_ORDER, if clicked Variables column not on metadata view (NOT named VAR_NAMES)
                  dl_fn = "*",
                  rotate_for_one_row = FALSE,
                  title = dl_fn,
                  messageTop = NULL,
                  messageBottom = NULL
                  ) { # caveat: the fixed columns filter may not work.
# TODO: https://datatables.net/forums/discussion/75723/adding-custom-colors-for-cells-in-excel-export line #15 in "//custom button for cashflow excel generation" shows, how to add custom styles, https://datatables.net/extensions/buttons/examples/html5/excelCellShading.html shows, how to conditionally apply those. Use this to enable cell background suiting the HTML version of the table.
# TODO: add comments (descs) to header cells in excel export

  force(copy_row_names_to_column)

  util_ensure_suggested("htmltools", "Generating nice tables")
  util_ensure_suggested("DT", "Generating nice tables")

  if (is.null(tb) || nrow(tb) == 0 || ncol(tb) == 0) {
    return()
  }

  class(tb) <- setdiff(class(tb), "dataquieR_result")

  util_expect_scalar(rotate_for_one_row, check_type = is.logical)

  output_format <- util_match_arg(output_format)

  util_expect_scalar(cols_are_indicatormetrics, check_type = is.logical)
  util_expect_scalar(colnames_aliases2acronyms, check_type = is.logical)
  util_expect_scalar(is_matrix_table, check_type = is.logical)

  util_stop_if_not(!(cols_are_indicatormetrics && colnames_aliases2acronyms))

  if (missing(meta_data)) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      # https://stackoverflow.com/a/33121933
      meta_data <- knitr::knit_global()[["report"]][["meta_data"]]
    }
  }

  description <- attr(tb, "description")
  tb <- as.data.frame.matrix(tb,
                             stringsAsFactors = FALSE)
  attr(tb, "description") <- description

  log_cols <- vapply(tb, is.logical, FUN.VALUE = logical(1))
  tb[, log_cols] <-
    vapply(tb[, log_cols, drop = FALSE],
           as.character, FUN.VALUE = character(nrow(tb)))
  numcols <- prep_datatype_from_data(tb) %in%
    c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)
  tb[, numcols] <-
    vapply(tb[, numcols, drop = FALSE], scales::number,
           FUN.VALUE = character(nrow(tb)))

  if (copy_row_names_to_column) {
    description <- attr(tb, "description")
    tb <- cbind.data.frame(data.frame(Variables = rownames(tb), # TODO: Check, if Variable still is used instead of plural somewhere
                                      stringsAsFactors = FALSE), tb)
    attr(tb, "description") <- description
    rownames(tb) <- NULL
  }


  if (link_variables && any(c("Variables", VAR_NAMES) %in%
                            base::colnames(tb)) && !missing(meta_data)) {
    Variables <- intersect(base::colnames(tb),
                           c("Variables", VAR_NAMES))
    Variables <- head(Variables, 1)

    if (Variables == VAR_NAMES &&
        all(c(VAR_NAMES, label_col) %in% names(meta_data))) {
      tb[[Variables]] <- util_map_labels(
        tb[[Variables]],
        meta_data = meta_data,
        to = label_col,
        from = VAR_NAMES,
        ifnotfound = tb[[Variables]])
    }

    if (VAR_NAMES %in% names(meta_data)) {
      vn <- util_map_labels(
        tb[[Variables]],
        meta_data = meta_data,
        to = VAR_NAMES,
        from = label_col,
        ifnotfound = tb[[Variables]]
      ) } else {
        vn <- tb[[Variables]]
      }

    if (LABEL %in% names(meta_data)) {
      lb <- util_map_labels(
        tb[[Variables]],
        meta_data = meta_data,
        to = LABEL,
        from = label_col,
        ifnotfound = tb[[Variables]]
      )} else {
        lb <- tb[[Variables]]
      }

    if (LONG_LABEL %in% names(meta_data)) {
      llb <- util_map_labels(
        tb[[Variables]],
        meta_data = meta_data,
        to = LONG_LABEL,
        from = label_col,
        ifnotfound = tb[[Variables]]
      ) } else {
        llb <- tb[[Variables]]
      }

    nice_lb <- tb[[Variables]]
    if (all(util_empty(nice_lb) == util_empty(llb))) {
      nice_lb <- llb
    } else if (all(util_empty(nice_lb) == util_empty(lb))) {
      nice_lb <- lb
    }

    href <- tb[[Variables]]

    if (Variables == VAR_NAMES) {
      data <- vn
      coltitle <- nice_lb
    } else {
      data <- nice_lb
      coltitle <- vn
    }

    .filter <- data

    if (output_format == "RMD") { # a proxy to detect the old rmd based output engine
      href <- paste0("#", prep_link_escape(href,
                                           html = FALSE))
      coltitle <- prep_title_escape(coltitle,
                                 html = FALSE)
      data <- prep_title_escape(data,
                        html = FALSE)
      links <- mapply(
        href = href,
        title = coltitle,
        filter = .filter,
        data,
        SIMPLIFY = FALSE,
        FUN = htmltools::a
      )
    } else {
      href <- paste0("VAR_", prep_link_escape(href,
                                              html = TRUE),
                     ".html#",
                     htmltools::urlEncodePath(as.character(href)))
      coltitle <- prep_title_escape(coltitle,
                                 html = TRUE)
      data <- prep_title_escape(data,
                                html = TRUE)
      onclick <- sprintf(
        'if (window.hasOwnProperty("dq_report2") && window.dq_report2 && window.location != "%s") { if (all_ids.all_ids.includes("%s")) { window.location = "%s" } else { window.alert("No result avaialable"); } }',
                                           href,
                                           href,
                                           href)
      links <- mapply(
        href = sprintf('javascript:console.log("%s")', href),
        onclick = onclick,
        title = coltitle,
        filter = .filter,
        data,
        SIMPLIFY = FALSE,
        FUN = htmltools::a
      )
    }

    tb[[Variables]] <- vapply(links,
                              FUN = as.character,
                              FUN.VALUE = character(1))
  }


  if (rotate_for_one_row && nrow(tb) == 1) {   #TODO: maybe the description is lost here
    tb <- data.frame(Name = base::colnames(tb),
                     Value = unlist(tb[1, , TRUE], recursive = FALSE,
                                    use.names = FALSE),
                     stringsAsFactors = FALSE)
  }


  if (missing(colnames)) {
    colnames <- base::colnames(tb)
  }

  if (is.null(columnDefs)) {
    columnDefs <-
      list(
        list(width = '5em',
             targets = (setdiff(seq_len(ncol(tb)), which(colnames(tb) %in% hideCols))) - 1
        ),
        list(visible = FALSE,
             searchable = FALSE,
             targets = which(colnames(tb) %in% hideCols) - 1
        )
      )
  }

  columnDefs <- c(
    columnDefs,
    list(
      list(
        targets = 0,
        className = "dt-nowrap max_60vw"
      ),
      list(
        targets = (setdiff(seq_len(ncol(tb)), which(colnames(tb) %in% hideCols))) - 1,
        render = DT::JS("sort_vert_dt")
      )
    )
  )

  fnames <-
    vapply(colnames,
           util_map_by_largest_prefix,
           haystack = names(.manual$titles),
           FUN.VALUE = character(1))

  ftitles <-
    vapply(fnames, function(fn) {
      r <- util_alias2caption(fn, long = TRUE) # .manual$titles[[fn]];
      if (length(r) != 1) r <- NA_character_;
      r
    }, FUN.VALUE = character(1))

  ftitles[is.na(ftitles)] <- colnames[is.na(ftitles)]

  coltitles <-
    vapply(colnames, function(cn) {
      r <- util_alias2caption(cn, long = TRUE) # .manual$titles[[fn]];
      if (length(r) != 1) r <- NA_character_;
      r
    }, FUN.VALUE = character(1))

  ftitles[is.na(ftitles)] <- colnames[is.na(ftitles)]

  if (output_format == "HTML") {
    ftitles <- vapply(lapply(ftitles, htmltools::h3),
                      as.character, FUN.VALUE = character(1))
  } else {
    # ftitles <- paste("# ", ftitles)
  }

  fnames[is.na(fnames)] <- colnames[is.na(fnames)]

  if (colnames_aliases2acronyms) {
    suffixes <-
      mapply(SIMPLIFY = FALSE, cn = colnames, fn = fnames,
             FUN = function(cn, fn) {
        if (startsWith(cn, fn)) {
          substr(cn, nchar(fn) + 1 + 1, nchar(cn)) # name + "_" (first +1), start is the next character (second +1)
        } else {
          cn
        }
      })
    acronyms <-
      util_map_labels(fnames,
                      util_get_concept_info("implementations"),
                      to = "dq_report2_short_title",
                      from = "function_R",
                      ifnotfound = util_abbreviate(fnames))
    suffixes <- gsub("_", " ", vapply(suffixes, as.character,
                                      FUN.VALUE = character(1)))
    suffixes[!util_empty(suffixes)] <- paste0(":",
                                             abbreviate(
                                               suffixes[!util_empty(suffixes)],
                                               minlength = 3
                                              )
                                            )
    acronyms <- paste0(acronyms, suffixes)
    names(acronyms) <- colnames
  } else if (cols_are_indicatormetrics) {
    # acronyms <- colnames
    acronyms <- util_translate_indicator_metrics(colnames, short = TRUE)
  } else {
    acronyms <- colnames
    names(acronyms) <- colnames
  }

  if (cols_are_indicatormetrics) {
    fdescs <- rep("TODO", length(colnames))
  } else {
    fdescs <- vapply(fnames, FUN.VALUE = character(1), util_function_description)
  }

  if (missing(descs)) {
    if(!is.null(attr(tb, "description"))){
      descs <- attr(tb, "description")
    } else if (cols_are_indicatormetrics) {
      descs <- rep("TODO", length(colnames))
    } else {
      descs <- vapply(colnames, FUN.VALUE = character(1), util_col_description)
    }
  } else {
    util_expect_scalar(descs, allow_more_than_one = TRUE, check_type =
                         is.character)
    util_stop_if_not(
      `Need one description per column` =
        length(descs) == length(colnames))
  }

  acs <- acronyms[colnames]

  if (requireNamespace("htmltools", quietly = TRUE)) {
    htmlescape <- function(x) {
      htmltools::htmlEscape(x, attribute = TRUE)
    }
  } else {
    htmlescape <- function(x) {
      gsub("&|<|>|'|\"|\r|\n", ".", x)
    }
  }

  descs <- htmlescape(descs)
  fdescs <- htmlescape(fdescs)
  acs <- htmlescape(acs)

  if (all(is.na(acs)) && all(colnames == "")) {
    acs <- colnames
  }

  if (rotate_headers) {
    cssClass <- c("vertDT")
    cn <-
      paste0("<div class=\"colheader\" colname=\"",
             coltitles,
             "\" title=\"",
             paste(coltitles, descs, sep = "<br />\n\n"),
             "\">",
             vapply(
               strsplit(
                 acs,
                 "", fixed = TRUE),
               function(letters) {
                 paste0("<span>",
                        paste0(letters,
                               collapse = ""),
                        "</span>")
               },
               FUN.VALUE = character(1)),
             "</div>")
  } else {
    cssClass <- "myDT"
    cn <- paste0("<span colname=\"",
                 coltitles,
                 "\" title=\"",
                 paste(coltitles, descs, sep = "<br />\n\n"),
                 "\">",
                 acs,
                 "</span>")
  }

  if (!is.null(title)) {
    title <- dQuote(title) # these ugly quotes are needed to prevent cleverly Excel from guessing, a title like "2020-01-01" may be a date to be displayed as its integer representation
  }
  if (!is.null(messageTop)) {
    messageTop <- dQuote(messageTop) # these ugly quotes are needed to prevent cleverly Excel from guessing, a title like "2020-01-01" may be a date to be displayed as its integer representation
  }
  if (!is.null(messageBottom)) {
    messageBottom <- dQuote(messageBottom) # these ugly quotes are needed to prevent cleverly Excel from guessing, a title like "2020-01-01" may be a date to be displayed as its integer representation
  }


  .options <- list(
    dom = "Bt",
    # dom = "Bltp",
     buttons = list(
#       list( # TODO: https://stackoverflow.com/a/65830545 but Excel also cannot handle inline images
#         extend = "collection",
#         text = "xxx",
#         action = DT::JS(
#           "function (e, dt, node, config) {
#             var tab = $(dt.table().container()).find('table')[1];
#             debugger
#             window.open('data:application/vnd.ms-excel,' + encodeURIComponent( tab.outerHTML));
#           }
#           "
# #          window.open('data:application/vnd.ms-excel,' + encodeURIComponent( document.getElementById('tableComments').outerHTML));"
#         )
#       ),
      list(
        extend = 'copy',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        exportOptions = list( orthogonal = 'filter',
                              format = list(
                                header = DT::JS("function ( data, columnIdx ) {
                                  return $(data).attr(\"colname\");
                                }")
                              ))
      ),
      list(
        extend = 'excel',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter',
                              format = list(
                                header = DT::JS("function ( data, columnIdx ) {
                                  return $(data).attr(\"colname\");
                                }")
                              )),
        customize =   # https://datatables.net/forums/discussion/60535/freeze-lock-first-row-in-datatables-excel-export-file to freeze first column and first row
          # https://datatables.net/reference/button/excelHtml5
          DT::JS('function (xlsx) {
            var sheet = xlsx.xl.worksheets["sheet1.xml"];

            var freezePanes =
              \'<sheetViews><sheetView tabSelected="1" workbookViewId="0"><pane xSplit="1" ySplit="3" topLeftCell="B4" activePane="bottomRight" state="frozen"/></sheetView></sheetViews>\';
            var current = sheet.children[0].innerHTML;
            current = freezePanes + current;
            sheet.children[0].innerHTML = current;
          }
        '),
        autoFilter = TRUE
      ),
      list(
        extend = 'csv',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter',
                              format = list(
                                header = DT::JS("function ( data, columnIdx ) {
                                  return $(data).attr(\"colname\");
                                }")
                              ))
      ),
      list(
        extend = 'pdf',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter',
                              format = list(
                                header = DT::JS("function ( data, columnIdx ) {
                                  return $(data).attr(\"colname\");
                                }")
                              ))
      ),
      list(
        extend = 'print',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        message = dl_fn,
        exportOptions = list( orthogonal = 'filter',
                              format = list(
                                header = DT::JS("function ( data, columnIdx ) {
                                  return $(data).attr(\"colname\");
                                }")
                              ))
      )
    ),
    columnDefs = columnDefs, # https://github.com/rstudio/DT/issues/29
    autoWidth = autoWidth,
    rowCallback = rowCallback,
    autoFill = TRUE,
    scrollX = TRUE,
    scrollY = "55vh",
    scrollCollapse = TRUE,
    paging = FALSE,
    responsive = TRUE,
    fixedColumns = list(leftColumns = 1 + tb_rownames) # https://stackoverflow.com/a/51623663
  )

  .options[names(options)] <- options

  if (is_matrix_table) {
    cssClass <- paste(cssClass, "matrixTable", collapse = " ")
  }

  # TODO: Add links to table columns/rows not only the cells.
  dtable <- DT::datatable(tb,
                          escape = FALSE,
                          class = cssClass,
                          filter = filter,
                          rownames = tb_rownames,
                          extensions = c("FixedColumns", # https://stackoverflow.com/a/51623663
                                         "Buttons"),
                          options = .options,
                          colnames = cn,
                          fillContainer = fillContainer,
                          ...)

  jqui <- rmarkdown::html_dependency_jqueryui()
  jqui$stylesheet <- "jquery-ui.min.css"

  dtable$dependencies <- c(
    dtable$dependencies,
    list(
      rmarkdown::html_dependency_jquery(),
      jqui,
      html_dependency_report_dt() # always, since sort_vert is indep. from vert heads
    )
  )

  # if (is_matrix_table)  {
  #   htmltools::span(class = "matrixTable", dtable)
  #   # .dataTables_filter {    display: none;  }
  # } else {
  if (output_format == "HTML") {
    htmltools::tagList(htmltools::div(class = "table_top_spacer"),
                       htmltools::div(class = "table_result",
                   dtable), htmltools::br(style = "clear: both"))
  } else {
    dtable
  }
  #  }
}
