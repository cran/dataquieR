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
#'      [https://datatables.net/reference/button/excel](https://datatables.net/reference/button/excel))
#' @param cols_are_indicatormetrics [logical] cannot be `TRUE`,
#'        `colnames_aliases2acronyms` is `TRUE`. `cols_are_indicatormetrics`
#'        controls, if the columns are really function calls or, if
#'        `cols_are_indicatormetrics` has been set to `TRUE`, the columns are
#'        indicator metrics.
#' @param rotate_for_one_row [logical] rotate one-row-tables
#'
#' @return the table to be added to an `rmd`/´`html` file as
#'         [htmlwidgets::htmlwidgets]
#' @seealso [util_formattable()]
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
                  colnames, options = list(),
                  is_matrix_table = FALSE,
                  colnames_aliases2acronyms = is_matrix_table &&
                    !cols_are_indicatormetrics,
                  cols_are_indicatormetrics = FALSE,
                  label_col = LABEL,
                  output_format = c("RMD", "HTML"), # TODO: Order bny VAR_ORDER, if clicked Variables column not on metadata view (NOT named VAR_NAMES)
                  dl_fn = "*",
                  rotate_for_one_row = FALSE) { # caveat: the fixed columns filter may not work.

  force(copy_row_names_to_column)

  util_ensure_suggested("htmltools", "Generating nice tables")

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

  tb <- as.data.frame.matrix(tb,
                             stringsAsFactors = FALSE)
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
    tb <- cbind.data.frame(data.frame(Variables = rownames(tb), # TODO: Check, if Variable still is used instead of plural somewhere
                                      stringsAsFactors = FALSE), tb)
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
      title <- nice_lb
    } else {
      data <- nice_lb
      title <- vn
    }

    .filter <- data

    if (output_format == "RMD") { # a proxy to detect the old rmd based output engine
      href <- paste0("#", prep_link_escape(href,
                                           html = FALSE))
      title <- prep_title_escape(title,
                                 html = FALSE)
      data <- prep_title_escape(data,
                        html = FALSE)

    } else {
      href <- paste0("VAR_", prep_link_escape(href,
                                              html = TRUE),
                     ".html#",
                     htmltools::urlEncodePath(as.character(href)))
      title <- prep_title_escape(title,
                                 html = TRUE)
      data <- prep_title_escape(data,
                                html = TRUE)
    }

    links <- mapply(
      href = href,
      title = title,
      filter = .filter,
      data,
      SIMPLIFY = FALSE,
      FUN = htmltools::a
    )

    tb[[Variables]] <- vapply(links,
                              FUN = as.character,
                              FUN.VALUE = character(1))
  }


  if (rotate_for_one_row && nrow(tb) == 1) {
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
    descs <- rep("TODO", length(colnames))
    fdescs <- rep("TODO", length(colnames))
  } else {
    descs <- vapply(colnames, FUN.VALUE = character(1), util_col_description)
    fdescs <- vapply(fnames, FUN.VALUE = character(1), util_function_description)
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
      paste0("<div class=\"colheader\" title=\"",
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
    cn <- paste0("<span title=\"",
                 paste(coltitles, descs, sep = "<br />\n\n"),
                 "\">",
                 acs,
                 "</span>")
  }

  .options <- list(
    dom = "Bt",
    # dom = "Bltp",
    buttons = list(
      list(
        extend = 'copy',
        exportOptions = list( orthogonal = 'filter' )
      ),
      list(
        extend = 'excel',
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter' )
      ),
      list(
        extend = 'csv',
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter' )
      ),
      list(
        extend = 'pdf',
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter' )
      ),
      list(
        extend = 'print',
        message = dl_fn,
        exportOptions = list( orthogonal = 'filter' )
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

  dtable$dependencies <- c(
    dtable$dependencies,
    list(
      rmarkdown::html_dependency_jquery(),
      rmarkdown::html_dependency_jqueryui(),
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
