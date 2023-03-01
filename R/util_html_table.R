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
#'
#' @return the table to be added to an `rmd`/´`html` file as
#'         [htmlwidgets::htmlwidgets]
util_html_table <- function(tb, # TODO: Finish tidy-up -- done?
                  filter = "top", # TODO: Check with strange labels (report2, esp.)
                  columnDefs = NULL,
                  autoWidth = FALSE,
                  hideCols = character(0),
                  rowCallback = DT::JS("function(r,d) {$(r).attr('height', '2em')}"),
                  copy_row_names_to_column = length(rownames(tb)) == nrow(tb) &&
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
                  colnames_aliases2acronyms = is_matrix_table,
                  label_col = LABEL,
                  output_format = c("RMD", "HTML"),
                  dl_fn = "*") { # caveat: the fixed columns filter may not work.

  util_ensure_suggested("htmltools", "Generating nice tables")

  if (is.null(tb)) {
    return()
  }

  class(tb) <- setdiff(class(tb), "dataquieR_result")

  output_format <- util_match_arg(output_format)

  if (missing(meta_data)) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      # https://stackoverflow.com/a/33121933
      meta_data <- knitr::knit_global()[["report"]][["meta_data"]]
    }
  }

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
                     htmltools::urlEncodePath(href))
      title <- prep_title_escape(title,
                                 html = TRUE)
      data <- prep_title_escape(data,
                                html = TRUE)
    }

    links <- mapply(
      href = href,
      title = title,
      data,
      SIMPLIFY = FALSE,
      FUN = htmltools::a
    )

    tb[[Variables]] <- vapply(links,
                              FUN = as.character,
                              FUN.VALUE = character(1))
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
      r <- .manual$titles[[fn]];
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
  } else {
    acronyms <- colnames
    names(acronyms) <- colnames
  }

  if (requireNamespace("Rdpack", quietly = TRUE)) {
    rd <- Rdpack::Rdo_fetch(package = "dataquieR")
    # rd$acc_distributions.Rd
    util_get_col_description <- function(x, ...) {
      ""
    }
    util_get_function_description <-
      function(x, ...) {
        if (x %in% names(.manual$descriptions)) {
          d <- .manual$descriptions[[x]]
          if (length(d) == 0) {
            d <- sprintf("Description of %s", dQuote(x))
          } else {
            d <- paste0(d, collapse = "\n")
          }
        } else {
          d <- sprintf("Description of %s", dQuote(x))
        }
        d
        # x <- paste0(x, ".Rd")
        # if (is.null(rd[[x]])) return("")
        # # paste0(capture.output(tools::Rd2HTML(rd[[x]], fragment = TRUE)),
        # #        collapse = "\n")
        # if (output_format == "HTML") {
        #   h <- paste0(capture.output(tools::Rd2HTML(rd[[x]])),
        #          collapse = "\n")
        #   sub(perl = TRUE, '(?msi).*<body.*?>(.*)</body>.*', '\\1', h)
        #
        # } else {
        #   paste0(capture.output(tools::Rd2txt(rd[[x]])),
        #          collapse = "\n")
        # }
      }
  } else {
    util_warning("Without installed Rdpack, no documentation will be copied to the dataquieR dq_report.")
    util_get_col_description <- function(x, ...) {
      ""
    }
    util_get_function_description <-
      function(x, ...) {
        sprintf("Description of %s", dQuote(x))
      }
  }

  descs <- vapply(colnames, FUN.VALUE = character(1), util_get_col_description)
  fdescs <- vapply(fnames, FUN.VALUE = character(1), util_get_function_description)

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
             paste(ftitles, colnames, descs, fdescs, sep = "\n\n"),
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
                 paste(ftitles, colnames, descs, fdescs, sep = "\n\n"),
                 "\">",
                 acs,
                 "</span>")
  }

  .options <- list(
    dom = "Bt",
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
    scrollY = "75vh",
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
