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
#' @param col_tags [list] if not `NULL`, a named `list()`, names are names used
#'                        to name a newly created  column-group hide/show
#'                        button, elements are column names belonging to each
#'                        column groups as defined by `colnames`
#' @param searchBuilder [logical] if `TRUE`, display a `searchBuilder`-Button.
#' @param init_search [list] object to initialize `searchBuilder`, see [`datatables.net`](https://datatables.net/reference/type/SearchBuilder.Criteria)
#' @param initial_col_tag [character] `col_tags` entry to activate initially
#' @param additional_init_args [list] if not missing or `NULL`, arguments passed to `JavaScript`, if `searchBuilder == TRUE`
#' @param additional_columnDefs [list] additional `columnDefs`, can be missing or `NULL`
#'
#' @return the table to be added to an `rmd`/´`html` file as
#'         [htmlwidgets::htmlwidgets]
#' @seealso `util_formattable()`
#'
#' @family summary_functions
#' @concept html
#' @noRd
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
                  messageBottom = NULL,
                  col_tags = NULL,
                  searchBuilder = FALSE,
                  initial_col_tag,
                  init_search,
                  additional_init_args,
                  additional_columnDefs
                  ) { # caveat: the fixed columns filter may not work.
# TODO: add comments (descs) to header cells in excel export
# TODO: add figures to exports

  force(copy_row_names_to_column)

  util_ensure_suggested("htmltools", "Generating nice tables")
  util_ensure_suggested("DT", "Generating nice tables")
  util_ensure_suggested("htmlwidgets", "Generating nice tables")

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
    if (isTRUE(getOption('knitr.in.progress')) &&
        exists("report", envir = knitr::knit_global())) {
      # https://stackoverflow.com/a/33121933
      meta_data <- knitr::knit_global()[["report"]][["meta_data"]]
    }
  }
  description <- attr(tb, "description")
  plain_label <- attr(tb[["Variables"]], "plain_label")
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
    vapply(tb[, numcols, drop = FALSE], util_round_to_decimal_places,
                                                    # format, # scales::number
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

    pretty_lb <- prep_get_labels( # TODO: use also translations for other labels
      resp_vars = tb[[Variables]],
      item_level = meta_data,
      label_col = label_col,
      label_class = "LONG",
      resp_vars_are_var_names_only = Variables == VAR_NAMES,
      resp_vars_match_label_col_only = Variables == "Variables"
    )

    if (is.null(plain_label)) {
      from <- tb[[Variables]]
    } else {
      from <- plain_label
    }

    if (Variables == VAR_NAMES &&
        all(c(VAR_NAMES, label_col) %in% names(meta_data))) {
      tb[[Variables]] <- util_map_labels(
        from,
        meta_data = meta_data,
        to = label_col,
        from = VAR_NAMES,
        ifnotfound = from)
    }

    if (VAR_NAMES %in% names(meta_data)) {
      vn <- util_map_labels(
        from,
        meta_data = meta_data,
        to = VAR_NAMES,
        from = label_col,
        ifnotfound = from
      ) } else {
        vn <- from
      }

    if (LABEL %in% names(meta_data)) {
      lb <- util_map_labels(
        from,
        meta_data = meta_data,
        to = LABEL,
        from = label_col,
        ifnotfound = from
      )} else {
        lb <- from
      }

    if (LONG_LABEL %in% names(meta_data)) {
      llb <- util_map_labels(
        from,
        meta_data = meta_data,
        to = LONG_LABEL,
        from = label_col,
        ifnotfound = from
      ) } else {
        llb <- from
      }

    # nice_lb <- tb[[Variables]]
    # if (all(util_empty(nice_lb) == util_empty(llb))) {
    #   nice_lb <- llb
    # } else if (all(util_empty(nice_lb) == util_empty(lb))) {
    #   nice_lb <- lb
    # }
    nice_lb <- pretty_lb

    if (Variables == VAR_NAMES) {
      href <- tb[[Variables]]
    } else {
      href <- from
    }

    if (Variables == VAR_NAMES) {
      data <- vn
      hover_title <- nice_lb
    } else {
      data <- nice_lb
      hover_title <- vn
    }

    .filter <- data

    if (output_format == "RMD") { # a proxy to detect the old rmd based output engine
      href <- paste0("#", prep_link_escape(href,
                                           html = FALSE))
      hover_title <- prep_title_escape(hover_title,
                                 html = FALSE)
      data <- prep_title_escape(data,
                        html = FALSE)
      links <- mapply(
        href = href,
        title = hover_title,
        filter = .filter,
        data,
        SIMPLIFY = FALSE,
        FUN = htmltools::a
      )
    } else {
      href <- paste0("VAR_", prep_link_escape(href,
                                              html = TRUE),
                     ".html#",
                     htmltools::urlEncodePath(prep_link_escape(as.character(href))))
      if (!is.null(plain_label)) { # if we have a plain_label attribute, we should consider the entries in the Variables column HTML
        data <- lapply(tb[["Variables"]], htmltools::HTML)
        hover_title <- prep_title_escape(plain_label,
                                         html = TRUE)
      } else {
        data <- prep_title_escape(data,
                                  html = TRUE)
        hover_title <- prep_title_escape(hover_title,
                                         html = TRUE)
      }
      onclick <- sprintf(
        'if (window.hasOwnProperty("dq_report2") && window.dq_report2 && window.location != "%s") { if (all_ids.all_ids.includes("%s")) { window.location = "%s" } else { window.alert("No result available"); } }',
                                           href,
                                           href,
                                           href)
      # prep_get_labels() or l18n
      hover_text <- mapply(SIMPLIFY = FALSE,
                           hover_title = hover_title,
                           vn = vn,
                           lb = lb,
                           llb = llb,
                           function(hover_title, vn, lb, llb) {
                             .m <- NULL
                             try({
                               .m <- meta_data[meta_data[[VAR_NAMES]] == vn, , FALSE]
                               .m <- data.frame(fix.empty.names = FALSE,
                                               check.names = FALSE,
                                               ` ` = unlist(.m[1, , TRUE]))
                               .m <- .m[!is.na(.m[[1]]), , FALSE]
                               .m <- head(.m, 5)
                               .m <- withr::with_options(list(knitr.kable.NA = ''),
                                                              htmltools::HTML(
                                                                knitr::kable(.m,
                                                                             "html")))
                             }, silent = TRUE)
                             htmltools::div(
                               style = htmltools::css(
                                 overflow_y = "scroll",
                                 max_height = "80vh"),
                               htmltools::h5(hover_title),
                               htmltools::tags$ul(
                                 htmltools::tags$li(vn),
                                 htmltools::tags$li(lb),
                                 htmltools::tags$li(llb)
                               ),
                               .m
                              )
                           })
      links <- mapply(
        href = sprintf('javascript:console.log("%s")', href),
        onclick = onclick,
        title = hover_text,
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

  # insert another label column
  if (exists("nice_lb") && exists("vn")) {
    ..is_html_escaped <- (identical(attr(tb, "is_html_escaped"), TRUE))
    ..description <- attr(tb, "description")
    ..plain_label <- attr(tb[["Variables"]], "plain_label")
    var_col <- which(base::colnames(tb) == "Variables")
    if (length(var_col) == 1) {
      ..left <- seq_len(var_col - 1)
      ..right <- var_col + seq_len(ncol(tb) - var_col)
      tb <- cbind(tb[..left], Variables = vn,
                  Labels = tb[["Variables"]],
                  tb[..right])
      if (!missing(descs)) {
        if (is.null(names(descs))) {
          descs <- c(descs[..left],
                     "Variables",
                     "Labels",
                     descs[..right])
        } else {
          descs <- c(descs[..left],
                     Variables = "Variables",
                     Labels = "Labels",
                     descs[..right])
        }
      }
    }
    attr(tb, "is_html_escaped") <- ..is_html_escaped
    attr(tb, "description") <- ..description
    attr(tb[["Variables"]], "plain_label") <- ..plain_label

  }

  # rotate ----
  if (rotate_for_one_row && nrow(tb) == 1) {   #TODO: maybe the description is lost here
    is_html_escaped <- (identical(attr(tb, "is_html_escaped"), TRUE))
    tb <- data.frame(Name = base::colnames(tb),
                     Value = unlist(tb[1, , TRUE], recursive = FALSE,
                                    use.names = FALSE),
                     stringsAsFactors = FALSE)
    attr(tb, "is_html_escaped") <- is_html_escaped
  }

  # column attributes ----
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
    acronyms <- util_translate_indicator_metrics(colnames,
                                                 short = TRUE,
                                                 long = FALSE)
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
      descs <- descs[colnames]
      descs[is.na(descs)] <- ""
      names(descs) <- colnames
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
    if (searchBuilder) {
      cn <- paste(coltitles)
    } else {
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
    }
  } else {
    cssClass <- "myDT"
    if (searchBuilder) {
      cn <- paste(coltitles)
    } else {
      cn <- paste0("<span colname=\"",
                   coltitles,
                   "\" title=\"",
                   paste(coltitles, descs, sep = "<br />\n\n"),
                   "\">",
                   acs,
                   "</span>")
    }
  }

  if (!is.null(title)) {
    title <- dQuote(title) # these ugly quotes are needed to prevent cleverly Excel from guessing, a title like "2020-01-01" may be a date to be displayed as its integer representation
  }
  if (!is.null(messageTop)) {
    messageTop <- dQuote(messageTop) # these ugly quotes are needed to prevent cleverly Excel from guessing, a title like "2020-01-01" may be a date to be displayed as its integer representation
  } else {
    messageTop <- "-" # TODO: Make messsageTop == null work in JS (ie in ~/git/gitlab/QualityIndicatorFunctions/QualityIndicatorFunctions/inst/report-dt-style/report_dt.js)
  }
  if (!is.null(messageBottom)) {
    messageBottom <- dQuote(messageBottom) # these ugly quotes are needed to prevent cleverly Excel from guessing, a title like "2020-01-01" may be a date to be displayed as its integer representation
  }

  col_tags_filter_buttons <- list()
  if (!is.null(col_tags)) {
    for (ct in names(col_tags)) {
      in_group <- colnames %in% col_tags[[ct]]
      col_tags_filter_buttons <- c(col_tags_filter_buttons, list(list(
        extend = 'colvisGroup',
        text = ct,
        show = which(in_group) - 1,
        hide = which(!in_group) - 1,
        name = ct
        # text = 'No Col.',
        # hide = ':visible',
        # show = ':hidden'
      )))
    }
  }

  messageTop = DT::JS(sprintf('function() { return setMsgTop.call(this, "%s"); }',
                              gsub('"', '\\"', messageTop, fixed = TRUE)))

  if (!missing(additional_columnDefs) && !is.null(additional_columnDefs) &&
      is.list(additional_columnDefs)) {
    columnDefs <- c(columnDefs, additional_columnDefs)
  }

  .options <- list(
    dom = "Bt",
    # dom = "Bltp",
     buttons = c(list(
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
        extend = 'colvis',
        collectionLayout = 'fixed columns',
        postfixButtons = list('colvisRestore', '<input class="search_curr_colvis_input" onkeyup="search_curr_colvis()"></input>', '<button onclick="search_curr_colvis()">Toggle</button>'),
        # columns: ':not(.noVis)',
        columnText = DT::JS("function (dt, idx, title) {
          return dt.column(idx).header().textContent;
        }")
      )),
      col_tags_filter_buttons,
      list(
        list(
        extend = 'copy',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        exportOptions = list( orthogonal = 'filter',
                              rows = DT::JS("rowFilter"),
                              columns = ':visible',
                              format = list(
                                body = DT::JS("function (data, row, column, node) {
                                  r = data + \"\";
                                  if (r.match(/\"data:image\\//g)) {
                                    r = \"not supported in clipboard\"
                                  }
                                  if (r.match(/<table *>/g)) {
                                    r = \"not supported in clipboard\"
                                  }
                                  r = r.replaceAll(/\\<br *\\/?\\>/g,\"  \");
                                  return r;
                                }"),
                                header = DT::JS("function ( data, columnIdx ) {
                                  if ($(data).length > 0)
                                    return $(data).attr(\"colname\");
                                  else
                                    return data;
                                }")
                              ))
      ),
      list(
        extend = 'excel',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        filename = dl_fn,
        exportOptions = list( orthogonal = 'display',
                              rows = DT::JS("rowFilter"),
                              columns = ':visible',
                              format = list(
                                body = DT::JS("function (data, row, column, node) {
                                  r = data + \"\";
                                  if (r.match(/\"data:image\\//g)) {
                                    r = \"not yet supported in Excel\"
                                  }
                                  if (r.match(/<table *>/g)) {
                                    r = \"not yet supported in Excel\"
                                  }
                                  r = r.replaceAll(/\\<br *\\/?\\>/g,\":\\n \");
                                  r = $('<div />').append(r).text().trim();
                                  if (isNumeric(r.replaceAll(/\\s/g, ''))) {
                                    r = r.replaceAll(/\\s/g, '')
                                  }
                                  return r;
                                }"),
                                header = DT::JS("function ( data, columnIdx ) {
                                  if ($(data).length > 0)
                                    return $(data).attr(\"colname\");
                                  else
                                    return data;
                                }")
                              )),
                              customize =
                                # https://datatables.net/forums/discussion/60535/freeze-lock-first-row-in-datatables-excel-export-file to freeze first column and first row
                                # https://datatables.net/reference/button/excelHtml5
                                DT::JS('customize_excel')
                              ,
        autoFilter = TRUE
      ),
      list(
        extend = 'csv',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        filename = dl_fn,
        exportOptions = list( orthogonal = 'filter',
                              rows = DT::JS("rowFilter"),
                              columns = ':visible',
                              format = list(
                                body = DT::JS("function (data, row, column, node) {
                                  r = data + \"\";
                                  if (r.match(/\"data:image\\//g)) {
                                    r = \"not supported in csv\"
                                  }
                                  if (r.match(/<table *>/g)) {
                                    r = \"not supported in csv\"
                                  }
                                  r = r.replaceAll(/\\<br *\\/?\\>/g,\"\\n\");
                                  return r;
                                }"),
                                header = DT::JS("function ( data, columnIdx ) {
                                  if ($(data).length > 0)
                                    return $(data).attr(\"colname\");
                                  else
                                    return data;
                                }")
                              ))
      ),
      list(
        extend = 'pdf',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        filename = dl_fn,
        orientation = "landscape",
        # pageSize = "A0",
        exportOptions = list( orthogonal = 'display',
                              rows = DT::JS("rowFilter"),
                              columns = ':visible',
                              stripNewlines = FALSE,
                              format = list(
                                body = DT::JS("function (data, row, column, node) {
                                  r = data + \"\";
                                  if (r.match(/\"data:image\\//g)) {
                                    r = \"not yet supported in pdf\"
                                  }
                                  if (r.match(/<table *>/g)) {
                                    r = \"not yet supported in pdf\"
                                  }
                                  r = r.replaceAll(/\\<br *\\/?\\>/g,\":\\n\");
                                  r = $('<div />').append(r).text().trim();
                                  if (isNumeric(r.replaceAll(/\\s/g, ''))) {
                                    r = r.replaceAll(/\\s/g, '')
                                  }
                                  return r;
                                }"),
                                header = DT::JS("function ( data, columnIdx ) {
                                  if ($(data).length > 0)
                                    return $(data).attr(\"colname\");
                                  else
                                    return data;
                                }")
                              )),
                              customize =   # https://datatables.net/forums/discussion/60535/freeze-lock-first-row-in-datatables-excel-export-file to freeze first column and first row
                                # https://datatables.net/reference/button/excelHtml5
                                DT::JS('customize_pdf')

      ),
      list(
        extend = 'print',
        title = title,
        messageTop = messageTop,
        messageBottom = messageBottom,
        message = dl_fn,
        autoPrint = TRUE,
        exportOptions = list( orthogonal = 'display',
                              rows = DT::JS("rowFilter"),
                              columns = ':visible',
                              stripHtml = FALSE,
                              format = list(
                                body = DT::JS("function (data, row, column, node) {
                                  r = data + \"\";
                                  if (r.match(/\"data:image\\//g)) {
//                                    r = \"not supported in print\"
                                  }
                                  if (r.match(/<table *>/g)) {
//                                    r = \"not supported in print\"
                                  }
//                                  r = r.replaceAll(/\\<br *\\/?\\>/g,\"\\n\");
                                  return r;
                                }"),
                                header = DT::JS("function ( data, columnIdx ) {
                                  if ($(data).length > 0)
                                    return $(data).attr(\"colname\");
                                  else
                                    return data;
                                }")
                              ))
      )
    )),
    # https://github.com/rstudio/DT/issues/29
    columnDefs = c(list(list(className = 'dt-right', targets="_all")),
                   columnDefs), #to do: make -only- numbers right aligned
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

  if (searchBuilder) {
    .options[["searchBuilder"]] <- TRUE
    .options[["search"]] <- list(return = TRUE)
    .options[["dom"]] <- paste0("Q", .options[["dom"]])
  }


  .options[names(options)] <- options

  if (is_matrix_table) {
    cssClass <- paste(cssClass, "matrixTable", collapse = " ")
  }

  ict <- "null"
  if (!missing(initial_col_tag)) {
    ict <- sprintf('"%s"', initial_col_tag)
  }

  is <- "null"
  if (searchBuilder && !missing(init_search)) {
    if (util_ensure_suggested("jsonlite",
                              goal = "search filter init in tables",
                              err = FALSE)) {
      is <- jsonlite::toJSON(init_search, auto_unbox = TRUE)
    }
  }
  aia <- "null"
  if (searchBuilder && !missing(additional_init_args)) {
    if (util_ensure_suggested("jsonlite",
                              goal = "additional init in JS tables",
                              err = FALSE)) {
      aia <- jsonlite::toJSON(additional_init_args, auto_unbox = TRUE)
    }
  }

  ids <- as.character(floor(runif(2, min = 0, max = .Machine$integer.max)))
  pref <- vapply(nchar(.Machine$integer.max) - nchar(ids),
         function(n) { paste0(rep("0", n), collapse = "") },
         FUN.VALUE = character(1))
  my_id <- paste0("id", paste0(pref, ids), collapse = "")

  # TODO: Add links to table columns/rows not only the cells.
  dtable <- DT::datatable(tb,
                          elementId = my_id,
                          callback =
                            DT::JS(sprintf("return(dataquieRdtCallback({ 'initialColTag': %s, 'initSearch': %s, 'additional_init_args': %s, 'table': table }));", ict, is, aia)),
                          escape = FALSE,
                          class = cssClass,
                          filter = filter,
                          rownames = tb_rownames,
                          extensions = c("FixedColumns", # https://stackoverflow.com/a/51623663
                                         "Buttons",
                                         "SearchBuilder"),
                          options = .options,
                          colnames = cn,
                          fillContainer = fillContainer,
                          ...)

  js <- sprintf("if (!window.dtConfig) {
                  window.dtConfig = {}
                }
                window.dtConfig['%s'] = %s;", my_id, aia)

  dtable <- htmlwidgets::prependContent(dtable,
                                        htmltools::tags$script(
                                  DT::JS(js)))

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
