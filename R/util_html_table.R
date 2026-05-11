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
#' @param df_escape [logical] apply `util_df_escape()` to the table
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
                  additional_columnDefs,
                  kv_table = !is.null(tb) && isTRUE(attr(tb, "kv_table")),
                  df_escape = FALSE
                  ) {
# caveat: the fixed columns filter may not work.
# TODO: add comments (descs) to header cells in excel export
# TODO: add figures to exports

  if (is.null(tb) || nrow(tb) == 0 || ncol(tb) == 0) {
    return()
  }

  force(copy_row_names_to_column)
  force(kv_table)

  util_ensure_suggested("htmltools", "Generating nice tables")
  util_ensure_suggested("DT", "Generating nice tables")
  util_ensure_suggested("htmlwidgets", "Generating nice tables")

  util_expect_scalar(rotate_for_one_row, check_type = is.logical)
  util_expect_scalar(cols_are_indicatormetrics, check_type = is.logical)
  util_expect_scalar(colnames_aliases2acronyms, check_type = is.logical)
  util_expect_scalar(is_matrix_table, check_type = is.logical)
  util_expect_scalar(df_escape, check_type = is.logical)

  util_stop_if_not(!(cols_are_indicatormetrics && colnames_aliases2acronyms))

  #  if we are currently being knit get our metadata from knitr
  if (missing(meta_data)) {
    if (isTRUE(getOption('knitr.in.progress')) &&
        exists("report", envir = knitr::knit_global())) {
      # https://stackoverflow.com/a/33121933
      meta_data <- knitr::knit_global()[["report"]][["meta_data"]]
    }
  }

  plain_label <- NULL
  try(plain_label <- attr(tb[["Variables"]], "plain_label"), silent = TRUE)

  tb <- util_html_table_prepare_data(tb
                                     , rotate_for_one_row
                                     , copy_row_names_to_column
                                     , df_escape = df_escape)

  if (link_variables && any(c("Variables", VAR_NAMES) %in%
                            base::colnames(tb)) && !missing(meta_data)) {
    result <- util_html_table_link_variables(tb = tb, descs = descs, meta_data = meta_data, label_col = label_col)
    tb <- result[["tb"]]
    if (!is.null(result[["descs"]])){
      descs <- result[["descs"]]
    }
  }

  # rotate ----
  if (rotate_for_one_row && nrow(tb) == 1) {
    tb <- util_table_rotator(tb)
    # is_html_escaped <- (identical(attr(tb, "is_html_escaped"), TRUE))
    # kv_table <- TRUE
    # tb <- data.frame(Name = base::colnames(tb),
    #                  Value = unlist(tb[1, , TRUE], recursive = FALSE,
    #                                 use.names = FALSE),
    #                  stringsAsFactors = FALSE)
    # attr(tb, "is_html_escaped") <- is_html_escaped
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
        targets = (which(colnames(tb) %in% c(VAR_NAMES, "Variables", "Labels", LABEL))) - 1,
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
  fnames[is.na(fnames)] <- colnames[is.na(fnames)]

  ftitles <- vapply(lapply(ftitles, htmltools::h3),
                      as.character, FUN.VALUE = character(1))

  coltitles <-
    vapply(colnames, function(cn) {
      r <- util_alias2caption(cn, long = TRUE) # .manual$titles[[fn]];
      if (length(r) != 1) r <- NA_character_;
      r
    }, FUN.VALUE = character(1))


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

  descs <- htmltools::htmlEscape(descs, attribute = TRUE)
  fdescs <- htmltools::htmlEscape(fdescs, attribute = TRUE)
  acs <- htmltools::htmlEscape(acs, attribute = TRUE)

  if (all(is.na(acs)) && all(colnames == "")) {
    acs <- colnames
  }

  cssClass <- if (rotate_headers) c("vertDT") else "myDT"

  if (rotate_headers && !searchBuilder) {
    cn <- paste0("<div class=\"colheader\" colname=\"",
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
   } else if (!searchBuilder) {
    cn <- paste0("<span colname=\"",
                   coltitles,
                   "\" title=\"",
                   paste(coltitles, descs, sep = "<br />\n\n"),
                   "\">",
                   acs,
                   "</span>")

  } else {
    cn <- paste(coltitles)
  }

  # these quotes are needed to prevent Excel from guessing
  # a title like "2020-01-01" may be a date to be displayed
  # as its integer representation
  # TODO: Make messsageTop == null work in JS (ie in ~/git/gitlab/QualityIndicatorFunctions/QualityIndicatorFunctions/inst/report-dt-style/report_dt.js)

  quote_ifnotnull <- function(v, default = v) {
    if(!is.null(v)) {
      return(dQuote(v))
    } else {
      return(default)
    }
   }

  title <- quote_ifnotnull(title)
  messageTop <- quote_ifnotnull(messageTop, "-")
  messageBottom <- quote_ifnotnull(messageBottom)

  messageTop = DT::JS(sprintf('function() { return setMsgTop.call(this, "%s"); }',
                              gsub('"', '\\"', messageTop, fixed = TRUE)))

  col_tags_filter_buttons <- list()
  if (!is.null(col_tags)) {
    for (ct in names(col_tags)) {
      css_class <- attr(col_tags[[ct]], "cssClass")
      in_group <- colnames %in% col_tags[[ct]]
      col_tags_filter_buttons <- c(col_tags_filter_buttons, list(list(
        extend = 'colvisGroup',
        className = paste('buttons-colvisGroup', css_class),
        text = ct,
        show = which(in_group) - 1,
        hide = which(!in_group) - 1,
        name = ct
      )))
    }
  }

  if (!missing(additional_columnDefs) && !is.null(additional_columnDefs) &&
      is.list(additional_columnDefs)) {
    columnDefs <- c(columnDefs, additional_columnDefs)
  }

  if (kv_table && ncol(tb) == 2) { # key-value-table, align to the center
    columnDefs <-
      c(list(list(className = 'dt-right', targets=0),
           list(className = 'dt-left', targets=1)),
      columnDefs)
  } else {
    columnDefs <- c(
      list(list(className = "dt-left", targets = "_all")),  # sane default
      util_get_datatables_alignments(tb),
      columnDefs
    )
  }

  .options <- list(
    dom = "Bt",
    buttons = c(
      list(
        list(
          extend = 'colvis',
          collectionLayout = 'fixed columns',
          postfixButtons = list( 'colvisRestore'
                               , '<input class="search_curr_colvis_input" onkeyup="search_curr_colvis()"></input>'
                               , '<button onclick="search_curr_colvis()">Toggle</button>'),
          columnText = DT::JS("function (dt, idx, title) {
              return dt.column(idx).header().textContent;
            }")
      )),
      col_tags_filter_buttons,
      util_make_export_buttons(dl_fn, title, messageBottom, messageTop)
      ),
    # https://github.com/rstudio/DT/issues/29
    columnDefs = columnDefs,
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

  is <- aia <- ict <- "null"

  if (searchBuilder) {
    result <- util_configure_search_builder(.options, init_search, additional_init_args)
    .options <- result[[".options"]]
    is <- result[["init_search"]]
    aia <- result[["additional_init_args"]]
  }

  if (!missing(initial_col_tag)) {
    ict <- sprintf('"%s"', initial_col_tag)
  }

  .options[names(options)] <- options

  if (is_matrix_table) {
    cssClass <- paste(cssClass, "matrixTable", collapse = " ")
  }

  ids <- as.character(floor(runif(2, min = 0, max = .Machine$integer.max)))
  pref <- vapply(nchar(.Machine$integer.max) - nchar(ids),
         function(n) { paste0(rep("0", n), collapse = "") },
         FUN.VALUE = character(1))
  my_id <- paste0("id", paste0(pref, ids), collapse = "")

  if (prep_is_translated(colnames(tb))) {
    colnames(tb) <- as.character(colnames(tb))
  }

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

  if (is_matrix_table) {
    dtable <- htmlwidgets::onRender(dtable,
       "function(el, x) {
          if (!window.jQuery) return;
          var $ = window.jQuery;

          function findWrapper() {
            // el is usually inside the wrapper; if not, fall back to closest wrapper in parents
            var $el = $(el);
            var $w = $el.closest('.dataTables_wrapper');
            return $w.length ? $w : $el.parent().closest('.dataTables_wrapper');
          }

          function syncIn($root) {
            // copy computed bg from pre -> td, then clear pre bg
            $root.find('tbody td > pre').each(function () {
              var pre = this;
              var td = pre.parentNode;
              if (!td) return;

              var bg = window.getComputedStyle(pre).backgroundColor;
              if (!bg || bg === 'transparent' || bg === 'rgba(0, 0, 0, 0)') return;

              td.style.backgroundColor = bg;
              pre.style.backgroundColor = 'transparent';
            });
          }

          function syncAll() {
            var $w = findWrapper();
            if ($w && $w.length) {
              // all tables within this widget's wrapper (main + clones)
              syncIn($w);
            } else {
              // fallback: only within el
              syncIn($(el));
            }
          }

          // Try immediately (may be too early, but cheap)
          syncAll();

          // Wait until the actual DataTable is initialized, then hook events
          var tries = 0;
          (function hook() {
            tries++;

            var $w = findWrapper();
            var $main = $w && $w.length ? $w.find('table.dataTable').first() : $(el).find('table.dataTable').first();

            if ($main.length && $.fn.dataTable && $.fn.dataTable.isDataTable($main[0])) {
              // Sync now and on redraw-ish events
              syncAll();
              $main.on('draw.dt column-visibility.dt column-reorder.dt responsive-resize.dt', syncAll);
              return;
            }

            if (tries < 100) setTimeout(hook, 50); // ~5s max
          })();
       }")
  }

  js <- sprintf("if (!window.dtConfig) {
                  window.dtConfig = {}
                }
                window.dtConfig['%s'] = %s;", my_id, aia)

  dtable <- htmlwidgets::prependContent( dtable,
                                         htmltools::tags$script(DT::JS(js)) )

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

  # dtable <- htmlwidgets::onRender(dtable, paste0("function(el, x) { utilHtmlTableInit(el, x, ", ifelse(isTRUE(fillContainer), "true", "false"), ",) }"))

  htmltools::tagList(htmltools::div(class = "table_top_spacer"),
                     htmltools::div(class = "table_result", dtable),
                     htmltools::br(style = "clear: both"))
}

#' Configure DT options for searchBuilder use
#'
#' @param .options  DT options
#' @param init_search util_html_table()
#' @param additional_init_args see util_html_table()
#'
#' @return a list containing each of the args altered to fit searchBuilder use
#' @keywords internal
#' @noRd
util_configure_search_builder <- function(.options, init_search, additional_init_args){
  .options[["searchBuilder"]] <- TRUE
  .options[["search"]] <- list(return = TRUE)
  .options[["dom"]] <- paste0("Q", .options[["dom"]])

  jsl <- util_ensure_suggested("jsonlite",
                              goal = "search filter init in tables",
                              err = FALSE)

  is <- aia <- "null"

  # return early without init search and additional args if we
  # are unable to generate the JSON
  if (!jsl) {
    return(list(.options = .options, init_search = is, additional_init_args = aia) )
  }

  if (!rlang::is_missing(init_search)) {
    is <- jsonlite::toJSON(init_search, auto_unbox = TRUE)
  }
  if (!rlang::is_missing(additional_init_args)) {
    aia <- jsonlite::toJSON(additional_init_args, auto_unbox = TRUE)
  }

  list(.options = .options, init_search = is, additional_init_args = aia)
}

#' Makes links from the variables in the table
#'
#' @param tb  the table
#' @param descs  descriptions
#' @param meta_data  see util_html_table()
#' @param label_col  see util_html_table()
#'
#' @return a list containing the table and, if applicable, descs
#' @keywords internal
#' @noRd
util_html_table_link_variables <- function(tb, descs, meta_data, label_col) {
  is_html_escaped <- (identical(attr(tb, "is_html_escaped"), TRUE))
  description <- attr(tb, "description")
  plain_label <- attr(tb[["Variables"]], "plain_label")

  Variables <- intersect(base::colnames(tb), c("Variables", VAR_NAMES))
  Variables <- head(Variables, 1)

  pretty_lb <- prep_get_labels( # TODO: use also translations for other labels
    resp_vars = tb[[Variables]],
    item_level = meta_data,
    label_col = label_col,
    label_class = "LONG",
    resp_vars_are_var_names_only = Variables == VAR_NAMES,
    resp_vars_match_label_col_only = Variables == "Variables"
  )

  from <- if (is.null(plain_label)) tb[[Variables]] else plain_label

  if (Variables == VAR_NAMES &&
      all(c(VAR_NAMES, label_col) %in% names(meta_data))) {
    tb[[Variables]] <- util_map_labels(
      from,
      meta_data = meta_data,
      to = label_col,
      from = VAR_NAMES,
      ifnotfound = from)
  }

  map_label_col_to_if_exists <- function (to, default) {
    if (to %in% names(meta_data)) {
      util_map_labels(
        from
      , meta_data = meta_data
      , to = to
      , from = label_col
      , ifnotfound = from
      )
    } else {
      default
    }
  }

  vn <- map_label_col_to_if_exists(VAR_NAMES, default = from)
  lb <- map_label_col_to_if_exists(LABEL, default = from)
  llb <- map_label_col_to_if_exists(LONG_LABEL, default = from)

  if (Variables == VAR_NAMES) {
    href <- tb[[Variables]]
    data <- vn
    hover_title <- pretty_lb
  } else {
    href <- from
    data <- pretty_lb
    hover_title <- vn
  }

  .filter <- data

  href <- paste0("VAR_",
                 prep_link_escape(href, html = TRUE),
                 ".html#",
                 htmltools::urlEncodePath(prep_link_escape(as.character(href))))

  # if we have a plain_label attribute, we should consider
  # the entries in the Variables column HTML
  if (!is.null(plain_label)) {
    data <- lapply(tb[["Variables"]], htmltools::HTML)
    hover_title <- prep_title_escape(plain_label, html = TRUE)
  } else {
    data <- prep_title_escape(data, html = TRUE)
    hover_title <- prep_title_escape(hover_title, html = TRUE)
  }

  onclick <- sprintf( 'if ( window.hasOwnProperty("dq_report2")
                            && window.dq_report2
                            && window.location != "%s") {
                          if (knowID("%s")) {
                              if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();
                              window.location = "%s";
                          } else {
                              window.alert("No result available");
                          }
                        }'
                    , href
                    , href
                    , href )

  mk_hover_text <- function(hover_title, vn, lb, llb) {
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
  }

  # prep_get_labels() or l18n
  hover_text <- mapply(
    hover_title = hover_title,
    vn = vn,
    lb = lb,
    llb = llb,
    SIMPLIFY = FALSE,
    FUN = mk_hover_text)

  links <- mapply(
    href = sprintf('javascript:console.log("%s")', href),
    onclick = onclick,
    title = hover_text,
    filter = .filter,
    data,
    SIMPLIFY = FALSE,
    FUN = htmltools::a
  )


  tb[[Variables]] <- (vapply(links,
          FUN = as.character,
          FUN.VALUE = character(1)))

  var_col <- which(base::colnames(tb) == "Variables")
  if (length(var_col) == 1) {
    ..left <- seq_len(var_col - 1)
    ..right <- var_col + seq_len(ncol(tb) - var_col)
    tb <- cbind(tb[..left], Variables = vn,
                Labels = tb[["Variables"]],
                tb[..right])

    if (!rlang::is_missing(descs)) {
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

  if(rlang::is_missing(descs)) descs <- NULL

  attr(tb, "is_html_escaped") <- is_html_escaped
  attr(tb, "description") <- description
  attr(tb[["Variables"]], "plain_label") <- plain_label

  list(tb = tb, descs = descs)
}

#' Prepares a table for use in util_html_table()
#'
#' @param tb   the table
#' @param rotate_for_one_row   see util_html_table()
#' @param copy_row_names_to_column  see util_html_table()
#' @param df_escape see util_html_table()
#'
#' @return the table
#' @keywords internal
#' @noRd
util_html_table_prepare_data <- function(tb,
                                         rotate_for_one_row,
                                         copy_row_names_to_column,
                                         df_escape) {

  class(tb) <- setdiff(class(tb), "dataquieR_result")

  is_html_escaped <- (identical(attr(tb, "is_html_escaped"), TRUE))
  description <- attr(tb, "description")
  plain_label <- NULL
  try(plain_label <- attr(tb[["Variables"]], "plain_label"), silent = TRUE)

  if (is.matrix(tb)) {
    tb <- as.data.frame.matrix(tb, stringsAsFactors = FALSE)
  }

  # convert logical columns to character strings
  # Find all columns featuring any sort of logical also "TRUE" or so, and convert it to logicals

  ts <- c("t", "true", "1", "+")
  fs <- c("f", "false", "0", "-")
  nas <- c("", "na", NA_character_)
  lgs <- unique(c(ts, fs, nas))

  is_log_col <- function(cl) {
    if (is.logical(cl)) return(TRUE)
    ..dt <- attr(cl, DATA_TYPE)
    if (!is.null(..dt) && !any(is.na(..dt))) {
      if (trimws(tolower(..dt)) %in% c("logical", "bool", "boolean")) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    all(
      trimws(tolower(as.character(cl))) %in% lgs
    )
  }

  as_log_col <- function(cl) {
    if (!is.logical(cl)) {
      clc <- trimws(tolower(as.character(cl)))
      r <- logical(length(cl))
      r[clc %in% ts] <- TRUE
      r[clc %in% fs] <- FALSE
      r[clc %in% nas] <- NA # not really needed
      cl <- r
    }
    cl
  }

  log_cols <- vapply(tb, is_log_col, FUN.VALUE = logical(1))

  tb[, log_cols] <-
    vapply(tb[, log_cols, drop = FALSE],
           as_log_col, FUN.VALUE = logical(nrow(tb)))

  # round all numerical columns and convert to strings
  dtypes <- prep_datatype_from_data(tb, guess_character = TRUE)
  dtypes2 <- lapply(tb, attr, DATA_TYPE)
  if (any(vapply(dtypes2, length, FUN.VALUE = integer(1)) == 1)) {
    for (cl in names(dtypes2)) {
      dt2 <- dtypes2[[cl]]
      if (!is.null(dt2) && !any(is.na(dt2))) {
        dtypes[[cl]] <- dt2
      }
    }
  }
  fltcols <- !log_cols & dtypes %in% c(DATA_TYPES$FLOAT)
  flts_still_char <-
    vapply(tb[, fltcols, drop = FALSE], function(x) util_round_to_decimal_places(as.numeric(x)),
                 FUN.VALUE = character(nrow(tb)))
  tb[, fltcols] <-
    as.numeric(ifelse(trimws(flts_still_char) == "NA", NA, trimws(flts_still_char)))
  intcols <- !log_cols & dtypes %in% c(DATA_TYPES$INTEGER)
  tb[, intcols] <- vapply(tb[, intcols, drop = FALSE], function(x) {
    if (all(suppressWarnings(!is.na(x) & !is.na(as.logical(x)) & is.na(as.integer(x))))) {
      x <- suppressWarnings(as.logical(x))
    }
    as.integer(x)
  }, FUN.VALUE = integer(nrow(tb)))

  txtcols <- !log_cols & dtypes %in% c(DATA_TYPES$STRING)
  if ("Variables" %in% names(txtcols)) {
    txtcols[["Variables"]] <- FALSE
  }
  if (VAR_NAMES %in% names(txtcols)) {
    txtcols[[VAR_NAMES]] <- FALSE
  }
  tb[, txtcols] <- lapply(tb[, txtcols, drop = FALSE], function(cl) {
    if (is.list(cl) && all(vapply(cl, length, FUN.VALUE = integer(1)) == 1L) &&
        all(vapply(lapply(cl, class), length, FUN.VALUE = integer(1)) == 1L) &&
        all(vapply(cl, is.character, FUN.VALUE = logical(1)))) {
      cl <- unlist(cl)
    }
    if (!is.list(cl) && !is.factor(cl) && length(unique(cl)) <= 10) {
      if (!any(grepl("<\\s*[a-zA-Z][^>]*>", cl, perl = TRUE))) {
        try(cl <- as.factor(cl), silent = TRUE)
      }
    }
    cl
  })

  if (copy_row_names_to_column) {
    tb <- cbind.data.frame(data.frame(Variables = rownames(tb),
                                      stringsAsFactors = FALSE), tb)
    rownames(tb) <- NULL
  }

  attr(tb, "is_html_escaped") <- is_html_escaped
  attr(tb, "description") <- description
  attr(tb[["Variables"]], "plain_label") <- plain_label

  if (df_escape) {
    tb <- util_df_escape(tb)
  }

  tb
}


#' Creates extra button definitions for datatables
#'
#' @param dl_fn  download file name
#' @param title  title of all buttons
#' @param messageBottom  bottom message
#' @param messageTop  top message
#'
#' @return list of button definitions for the DT lib
#' @keywords internal
#' @noRd
util_make_export_buttons <- function(dl_fn, title, messageBottom, messageTop) {
  exportHeader <- DT::JS("function (data, columnIdx) {
                           if ($(data).length > 0) {
                             return $(data).attr(\"colname\");
                           } else {
                             return data;
                           }
                         }")

  mkFormatBody <- function(extend, extraJS = "") {
    DT::JS(paste0("function (data, row, column, node) {
                      r = data + \"\";
                      if (r.match(/\"data:image\\//g)) {
                        r = \"not supported in ", extend, "\"
                      }
                      if (r.match(/<table *>/g)) {
                        r = \"not supported in ", extend, "\"
                      }
                      r = r.replaceAll(/\\<br *\\/?\\>/g,\"  \");",
                      extraJS,"
                      return r;
                    }"))
  }

  extraJS <- "r = $('<div />').append(r).text().trim();
              if (isNumeric(r.replaceAll(/\\s/g, ''))) {
                r = r.replaceAll(/\\s/g, '');
              }"

  mkExportOpts <- function ( extend
                           , extraJS = ""
                           , orthogonal = "filter"
                           , rows = DT::JS("rowFilter")
                           , columns = ":visible"
                           , formatBody = mkFormatBody(extend, extraJS)
                           , formatHeader = exportHeader
                           , extraFormat = list()
                           , ... ) {
    c(list(
      orthogonal = orthogonal,
      rows = rows,
      columns = columns,
      format = c(list(
          body = force(formatBody)
        , header = force(formatHeader)
      ), extraFormat)
    ), list(...))
  }

  mkButton <- function (extend
                      , .title = title
                      , .messageTop = messageTop
                      , .messageBottom = messageBottom
                      , exportOptions = mkExportOpts(extend)
                      , ...
                      ) {
      c(list(
          extend = extend
        , title = .title
        , messageTop = .messageTop
        , messageBottom = .messageBottom
        , exportOptions = exportOptions
        ), list(...)
      )
  }

  list(
    mkButton( extend = "copy")
  , mkButton( extend = "excel"
            , filename = dl_fn
            , customize = DT::JS('customize_excel')
            , exportOptions = mkExportOpts( extend = "excel"
                                          , orthogonal = "display"
                                          , extraJS = extraJS )
            , autoFilter = TRUE )
  , mkButton( extend = "csv"
            , filename = dl_fn )
  , mkButton( extend = "pdf"
            , filename = dl_fn
            , orientation = "landscape"
            , customize = DT::JS('customize_pdf')
            , exportOptions = mkExportOpts( extend = "pdf"
                                          , orthogonal = "display"
                                          , extraJS = extraJS )
            )
  , mkButton( extend = "print"
            , message = dl_fn
            , autoPrint = TRUE
            , exportOptions =
                mkExportOpts( stripHtml = FALSE
                            , orthogonal = "display"
                            , formatBody =
                                DT::JS("function(d,x,c,n){ return d + \"\"; }") )
            )
  )

}


util_dt_normalize_cell_text <- function(x, keep_first_number = TRUE) {
  x <- trimws(gsub("&nbsp;", " ",
                   gsub("(?is)<[^>]+>", "",
                        gsub("(?is)<br\\s*/?>", "\n", as.character(x), perl = TRUE),
                        perl = TRUE),
                   fixed = TRUE))

  # Treat trailing percent as numeric-ish (e.g., "30%" -> "30")
  x <- sub("\\s*%\\s*$", "", x)

  if (!keep_first_number) return(x)

  num_pat <- "[-+]?((\\d{1,3}([ ,]\\d{3})+)|(\\d+))(\\.\\d+)?([eE][-+]?\\d+)?|[-+]?\\.\\d+([eE][-+]?\\d+)?"
  m <- regexpr(num_pat, x, perl = TRUE)
  trimws(ifelse(m > 0, regmatches(x, m), x))
}

util_get_datatables_alignments <- function(
    tb,
    prefer_right_for_datetime = TRUE,
    right_types = c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
    center_types = character(0),
    ...
) {
  stopifnot(is.data.frame(tb))

  Map(function(col, j) {
    align <- if (is.integer(col) || is.numeric(col)) "dt-right"
    else if (inherits(col, c("POSIXct", "POSIXt", "Date")))
      if (prefer_right_for_datetime) "dt-right" else "dt-center"
    else if (is.logical(col)) "dt-center"
    else {
      x <- util_dt_normalize_cell_text(col, TRUE)
      x <- x[!is.na(x) & x != ""]
      dtype <- if (!length(x)) DATA_TYPES$INTEGER else prep_robust_guess_data_type(x, ...)
      if (dtype %in% right_types) "dt-right"
      else if (dtype %in% center_types) "dt-center"
      else if (dtype %in% c(DATA_TYPES$DATETIME, DATA_TYPES$TIME))
        if (prefer_right_for_datetime) "dt-right" else "dt-center"
      else "dt-left"
    }
    list(className = align, targets = j - 1L)
  }, unname(tb), seq_along(tb))
}
