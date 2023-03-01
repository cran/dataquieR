#' Convert a [dataquieR report v2][dq_report2] to a named list of web pages
#'
#' @param report [dataquieR report v2][dq_report2].
#' @param template [character] template to use, only the name, not the path
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param progress [`function`] lambda for progress in percent -- 1-100
#' @param progress_msg [`function`] lambda for progress messages
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
                                            progress_msg = progress_msg) {
  have_plot_ly <- util_ensure_suggested(pkg = c("plotly"),
                                        goal = "generate interactive figures in plain HTML-reports.",
                                        err = FALSE)
  if (disable_plotly)
    have_plot_ly <- FALSE
# TODO: fix character set problems with cat(as.character) on Windows (dQuote)
  # creates a link to our website, or to a vignette on the website
  online_ref <- function(fkt_name) {

    fkt_name <- cll_nm2fkt_nm(fkt_name)

    # convert function names to website names
    fkt_name <- strsplit(fkt_name, "_", fixed = TRUE)[[1]]
    fkt_name <- c("VIN", head(fkt_name, 1), "impl", tail(fkt_name, -1))
    fkt_name <- paste(fkt_name, collapse = "_")

    sprintf("https://dataquality.ship-med.uni-greifswald.de/%s.html", fkt_name)
  }

  cll_nm2fkt_nm <- function(fkt_name) {
    # get aliases from report attributes and then replace them by the actual function name
    f <- try(subset(attributes(
      attributes(report)$matrix_list)$function_alias_map,
      alias == fkt_name, "name", drop = TRUE),
      silent = TRUE)
    if (is.vector(f) && is.character(f) && (length(f) == 1)) {
      fkt_name <- f
    }
    # post-process function names
    if (fkt_name %in% c("con_hard_limits",
                        "con_soft_limits",
                        "con_detection_limits")) {
      fkt_name <- "con_limit_deviations"
    } else if (fkt_name %in% c("acc_robust_univariate_outlier")) {
      fkt_name <- "acc_univariate_outlier"
    }

    return(fkt_name)
  }

  if (have_plot_ly) {
    plot_figure <- function(x) {
      withCallingHandlers({
        if (inherits(x, "ggplot") && inherits(x, "patchwork")) {
          x <- htmltools::plotTag(x, "", width = 640) # leave patchworks as is, so far: TODO!!
        } else if (inherits(x, "ggplot")) {
          x <- util_remove_dataquieR_result_class(x)
          x <- x + ggplot2::theme(aspect.ratio = NULL) # prevent a warning
          py <- force(plotly::ggplotly(x, height = 480, width = 1040))
          py <- util_adjust_geom_text_for_plotly(py)
          x <- plotly::layout(py,
                              autosize = FALSE,
                              margin = list(autoexpand = FALSE,
                                            r = 200,
                                            b = 100)
          )
        }
      },
      warning = function(cond) { # suppress a waning caused by ggplotly for barplots
        if (startsWith(conditionMessage(cond),
                       "'bar' objects don't have these attributes: 'mode'")) {
          invokeRestart("muffleWarning")
        }
      })
      x
    }
  } else {
    plot_figure <- function(x) {
      x <- htmltools::plotTag(x, "", width = 640)
      x
    }
  }

  dims <- c(
    int = "Integrity",
    com = "Completeness",
    con = "Consistency",
    acc = "Accuracy"
  )

  # find which dimensions are included in the report
  dims_in_rep <-
    unique(vapply(strsplit(colnames(report), "_"), `[[`, 1,
                  FUN.VALUE = character(1)))

  dims_in_rep <- util_sort_by_order(dims_in_rep, names(dims))

  # match the functions to the dimensions
  functions_in_rep <- lapply(setNames(nm = dims_in_rep),
                             function(prefix)
                              colnames(report)[startsWith(colnames(report),
                                                          prefix)])

  vars_in_rep <- rownames(report)


  pages <- list()
  call_env <- environment()

  append_single_page <- function(drop_down_to_attach, # main menu entry (first-level)
                                 div_name, # sub-menu entry (second-level)
                                 file_name, # name where the page is stored, it is possible to add more than one page to a file
                                 ...) { # ... are the contents in htmltools compatible objects
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

  apmat <-
    util_html_table(summary(report, aspect = "applicability", FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = attr(report, "meta_data"),
                    label_col = attr(report, "label_col"),
                    dl_fn = "Applicability_Matrix",
                    output_format = "HTML"
  )

  ismat <-
    util_html_table(summary(report, aspect = "issue", FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = attr(report, "meta_data"),
                    label_col = attr(report, "label_col"),
                    dl_fn = "Issue_Matrix",
                    output_format = "HTML"
  )

  ermat <-
    util_html_table(summary(report, aspect = "error", FUN = util_get_html_cell_for_result),
                    filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                    is_matrix_table = TRUE, rotate_headers = TRUE,
                    meta_data = attr(report, "meta_data"),
                    label_col = attr(report, "label_col"),
                    dl_fn = "Error_Matrix",
                    output_format = "HTML"
  )

  # TODO: add some hints if there is an integrity issue
  append_single_page("General",
                     "Summary",
                     "report.html",
                     htmltools::htmlTemplate(
                       text_ = readLines(system.file("templates", template, "overview.html",
                                                     package = utils::packageName())),
                       ismat = ismat,
                       apmat = apmat,
                       ermat = ermat,
                       util_float_index_menu = util_float_index_menu,
                       util_map_labels = util_map_labels,
                       util_get_concept_info = util_get_concept_info,
                       util_abbreviate = util_abbreviate)
  )


  ilmd <- attr(report, "meta_data")

  # these two columns should have been replaced by the v1->v2 conversion of the metadata in
  # prep_meta_data_v1_to_item_level_meta_data (.util_internal_normalize_meta_data)
  ilmd[[MISSING_LIST]] <- NULL
  ilmd[[JUMP_LIST]] <- NULL

  meta_data_table <- util_html_table(
    ilmd, # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
    meta_data = attr(report, "meta_data"),
    label_col = attr(report, "label_col"),
    dl_fn = "Item-Level_Metadata",
    output_format = "HTML" # needed for generating plain html, "RMD" would generate a mix between Rmd and html
  )

  append_single_page("General",
                     "Item-level Metadata",
                     "report.html",
                     htmltools::htmlTemplate(
                       system.file("templates", template, "meta_data.html",
                                   package = utils::packageName()),
                       meta_data_table = meta_data_table)
  )

  referred_tables <- attr(report, "referred_tables")
  if (length(referred_tables)) { # show all tables referred to by the report
    for (reftab in names(referred_tables)) {
      append_single_page("General",
                         reftab,
                         "report.html",
                         htmltools::h1(dQuote(reftab)),
                         meta_data_table <- util_html_table(
                           referred_tables[[reftab]], # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available
                           meta_data = attr(report, "meta_data"),
                           label_col = attr(report, "label_col"),
                           dl_fn = "Item-Level_Metadata",
                           output_format = "HTML" # needed for generating plain html, "RMD" would generate a mix between Rmd and html
                         )
      )
    }
  }

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
    append_single_page("General",
                       "Report Information",
                       "report.html",
                       htmltools::tagList(
                         util_html_table(p,
                                         dl_fn = "Report_Information"),
                         htmltools::p(htmltools::tags$i(id = "render-time",
                           sprintf("Rendered using %s %s at %s",
                                   utils::packageName(),
                                   as.character(packageVersion(
                                     utils::packageName())),
                                   as.character(Sys.time()))
                           )),
                         htmltools::tags$script( # https://stackoverflow.com/a/34579496
                         '$(function() {readTextFile("renderinfo.json", function(text){
                              var xx = $("#render-time").html()
                              var data = JSON.parse(text);
                              if (data.hasOwnProperty("renderingTime")) {
                                $("#render-time").html(xx + " in " + data.renderingTime)
                              } else {
                                $("#render-time").html(xx + " -- no rendering time available.")
                              }
                              console.log(data);
                          });});'),
                         htmltools::hr(),
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
  }

  # generate Venn diagram
  v_in_m <- attr(report, "meta_data")[[VAR_NAMES]]
  v_in_s <- attr(report, "study_data_dimnames")[[2]] # dimnames [[1]] is missing
  if (util_ensure_suggested("ggvenn",
                        goal =
                "display Venn diagrams about the meta_data/study_data coverage",
                err = FALSE)) {

    overlap <-
      plot_figure(ggvenn::ggvenn(list(meta_data = v_in_m, study_data = v_in_s),
                   set_name_size = 4, text_size = 3))

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
      meta_data = attr(report, "meta_data"),
      label_col = attr(report, "label_col"),
      dl_fn = "StudyMetaDataOverlap",
      output_format = "HTML")
  }

  append_single_page("General",
                     "meta_data/study_data coverage",
                     "report.html",
                     htmltools::h1("meta_data/study_data coverage"),
                     overlap)

  append_single_page("General",
                     "study_data dimensions",
                     "report.html",
                     htmltools::h1("study_data dimensions"),
                     htmltools::p(sprintf(
                       "There are %d observations of %d study variables in the study data",
                       length(attr(report, "study_data_dimnames")[[1]]),
                       length(attr(report, "study_data_dimnames")[[2]])
                       )))


  # vapply(lapply(report, attr, "error"), length, FUN.VALUE = integer(1)) -- all errors

  # preferred order of the content of the report
  preferred_slots <- c("DataframeData", "DataframeTable", "SegmentData", "SegmentTable",
                       "VariableGroupTable", "VariableGroupData", "ReportSummaryTable",
                       "SummaryPlot", "SummaryPlotList", "SummaryData", "SummaryTable")

  # detect, if x starts with "<prefix>." or equals <prefix> (if results have been combined)
  startsWith_prefix._or_equals_prefix <- function(x, prefix) {
    startsWith(x, paste0(prefix, ".")) || x == prefix
  }

  # generates and converts to htmltools compatible object
  # dqr is an output (indicator) from dataquieR
  # nm is the name used in the report, the alias name of the function call plus the variable name
  # TODO: ensure that in square2 alias names do not have points
  # is_single_var if we are creating a single variable overview page or an indicator summary page
  pretty_print <- function(dqr, nm, is_single_var) {
    # if (dynGet("fkt") == "acc_loess_observer") browser()
    if (!inherits(dqr, "dataquieR_NULL")) { # if the function did not return NULL or trigger an error
      slot <- # stores the name of the result object returned by the indicator function being displayed
        head(intersect( # check if the preferred slots are in the result names
          preferred_slots, names(dqr)), 1) # take the first available result name according to the preferred slots
      if (startsWith_prefix._or_equals_prefix(nm, "con_limit_deviations") || # check, if we are working with a limits function
          startsWith_prefix._or_equals_prefix(nm, "con_hard_limits") ||
          startsWith_prefix._or_equals_prefix(nm, "con_soft_limits") ||
          startsWith_prefix._or_equals_prefix(nm, "con_detection_limits")) {
        if ("SummaryPlotList" %in% names(dqr)) { # add an exception, the histogram is preferred to the table (heat map)
          slot <- "SummaryPlotList"
        }
      }
      if (length(slot) > 0) { # if there is an output, check and collect all warnings/errors/messages
        errors <- attr(dqr, "error") # get and store the errors from the output
        warnings <- attr(dqr, "warning")
        messages <- attr(dqr, "message")
        # extract the condition messages, and ensure that multi-line messages are in a single line
        errors <- vapply(lapply(errors, `[[`, "message"), paste,
                           FUN.VALUE = character(1))
        warnings <- vapply(lapply(warnings, `[[`, "message"), paste,
                           FUN.VALUE = character(1))
        messages <- vapply(lapply(messages, `[[`, "message"), paste,
                           FUN.VALUE = character(1))
        x <- dqr[[slot]] # extract the corresponding result according to slot

        x <- util_remove_dataquieR_result_class(x)

        if (endsWith(slot, "PlotList")) {
          if (length(x) > 1) { # check and warn if there is more than one entry in PlotList
            util_warning(
              c("Internal error: %s with > 1 result should not be in a",
                "v2.0 report"),
              sQuote(slot))
            # TODO: Implement a work-around
            # x <- do.call(htmltools::div, x)
            x <- htmltools::div("%s should not contain > 1 result",
                                sQuote(slot))
          } else {
            if (length(x) == 1) {
              x <- x[[1]] # take the single plot from PLotList
            } else {
              x <- NULL
            }
          }
        }
        # check the class of x
        if (inherits(x, "ReportSummaryTable")) {
          x <- print.ReportSummaryTable(x, view = FALSE)
          # convert to ggplot
        }
        if (inherits(x, "ggplot")) {
          as_plotly <- attr(dqr, "as_plotly")
          if (!is.null(as_plotly) && exists(as_plotly, mode = "function"))
            as_plotly <- get(as_plotly, mode = "function")
          if (have_plot_ly && is.function(as_plotly)) {
            x <- as_plotly(dqr, height = 480, width = 1040)
            x <- util_adjust_geom_text_for_plotly(x)
            x <- plotly::layout(x,
                                autosize = FALSE,
                                margin = list(autoexpand = FALSE,
                                              r = 200,
                                              b = 100)
            )
          } else {
            x <- plot_figure(x)
          }
          # convert to plotly or base 64 plot image
        }
        if (is.data.frame(x)) {
          # check if dataframe is empty
          if (!prod(dim(x))) {
            x <- NULL
          } else {
            rownames(x) <- NULL # TODO: Check, if this is okay, always
            x <- util_html_table(x,
                                 meta_data = attr(report, "meta_data"),
                                 label_col = attr(report, "label_col"),
                                 output_format = "HTML",
                                 dl_fn = nm)
          }
        }
        if (length(x) > 0) {
          if (!inherits(x, "htmlwidget") && # check if output is compatible with htmltools
              !inherits(x, "shiny.tag") &&
              !inherits(x, "shiny.tag.list")) {
            x <- htmltools::p(
              sprintf(paste("Cannot display objects of class(es) %s, yet.",
                            "Please file a feature request."),
                      paste(dQuote(class(x)), collapse = ", ")))
          }
          cll <- attr(dqr, "call")
          if (is.language(cll)) {
            cll <- deparse(cll)
          }
          x <- htmltools::div(
            title = #htmltools::htmlEscape( # this is the text for the hover messages
              paste(errors, warnings, messages, collapse = "\n"),
            #attribute = TRUE),
            class = "dataquieR_result",
            `data-call` = paste0(cll, collapse = "\n"),
            x
          )
        } else {
          x <- NULL
        }
      } else {
        x <- NULL
      }
    } else {
      x <- NULL
    }
    if (!is.null(x)) { # create and add tags and links
      if (all(grepl(".", nm, fixed = TRUE))) { # get the full name, which includes a dot
        anchor <- util_generate_anchor_tag(name = nm,
                                           order_context =
                                             ifelse(
                                               is_single_var,
                                               "variable",
                                               "indicator")
        )
        # the link is most easily created here, but therefore in the wrong position, so later it must be moved
        link <-  util_generate_anchor_link(name = nm,
                                           order_context =
                                             ifelse(
                                               is_single_var,
                                               "variable",
                                               "indicator")
        )
      } else {
        anchor <- NULL
        link <- NULL
      }

      # add variable name to pages that are not single_vars and display multiple variable in same page
      if (!is_single_var && all(grepl(".", nm, fixed = TRUE))) {
        caption <- sub("^[^\\.]*\\.", "", nm)
        if (caption != "[ALL]") {
          caption <- htmltools::h5(caption)
        } else {
          caption <- NULL
        }
      } else {
        caption <- NULL
      }

      x <- htmltools::tagList(anchor = anchor, link = link, caption, x)
      # the link is most easily added here, but still in the wrong position, so later it must be moved
    }
    x
  }

  svp <- list() # takes a single variable page # TODO: Make optional, takes much time

  # show_all_dims is later evaluated in two different scenarios, it should be a function
  show_all_dims <- quote({j <- 0; m <- length(unlist(functions_in_rep, recursive = TRUE));
                         for (dm in names(functions_in_rep)) { # loop over each dimension in report output
    util_message("Dimension %s", dm)
    drop_down <- dims[[dm]] # fetches the name of the content for the first-level drop-down menu
    for (fkt in functions_in_rep[[dm]]) { # loop over the functions in each dimension
      # if (fkt == "acc_loess_observer") browser()
      if (exists("cur_var") && length(cur_var) == 1 && !is.na(cur_var) &&
          is.character(cur_var)) { # if we have cur_var defined we are in single variable context (similar to !missing(cur_var) would do for a function)
        is_single_var <- TRUE
        all_of_f <- report[cur_var, fkt] # extract the single result
      } else {
        is_single_var <- FALSE
        j <- j + 1
        progress(j/m * 100)
        all_of_f <- report[, fkt] # take all results for the current function
      }
      # try to combine results, mostly relevant for indicator function outputs (where single variable is false)
      # check the results for the existence of certain output types
      # each result is a logical vector
      PLOTs <- !vapply(lapply(all_of_f, `[[`, "SummaryPlot"), is.null,
                       FUN.VALUE = logical(1))
      PLOT_LISTSs <- !vapply(lapply(all_of_f, `[[`, "SummaryPlotList"), is.null,
                       FUN.VALUE = logical(1))

      # TODO: Add "VariableGroupTable", "VariableGroupData",

      STs <- !vapply(lapply(all_of_f, `[[`, "SummaryTable"), is.null,
                      FUN.VALUE = logical(1)) # TODO: Do the same for segment and data frame level output
      SDs <- !vapply(lapply(all_of_f, `[[`, "SummaryData"), is.null,
                     FUN.VALUE = logical(1)) # TODO: Do the same for segment and data frame level output
      RSTs <- !vapply(lapply(all_of_f, `[[`, "ReportSummaryTable"), is.null,
                      FUN.VALUE = logical(1))
      NULLs <- vapply(all_of_f, inherits, "dataquieR_NULL",
                      FUN.VALUE = logical(1))

      # collect all errors, warnings, or messages so that they are combined for a combined result
      collapse_msgs <- function(class) { # class is either error, warning or message
        msgs <- lapply(lapply(all_of_f, attr, class), vapply, conditionMessage, FUN.VALUE = character(1)); # extract and create a list of all the messages by class
        # the messages are grouped to avoid repetitions, so the messages are amended
        nms <- gsub("^.*?\\.", "", names(msgs), perl = TRUE)
        # remove variable names
        dfr <- do.call(rbind.data.frame, mapply(SIMPLIFY = FALSE, msgs, nms, FUN = function(msg_lst, varname) {
          msg_lst <- gsub(sprintf("(\\W|^)%s(\\W|$)", varname), "<VARIABLE>", msg_lst, perl = TRUE)
          if (length(msg_lst) > 0) {
            r <- data.frame(varname = varname, message = msg_lst)
          } else {
            r <- data.frame(varname = character(0), message = character(0))
          }
          r
        }))
        # remove numbers
        dfr$message <- gsub("(\\W|^)\\d+(\\W|$)", " <NUMBER> ", dfr$message, perl = TRUE)
        dfr$message <- trimws(gsub(" +", " ", dfr$message, perl = TRUE))
        if (!prod(dim(dfr))) {
          return(character(0))
        }
        # group messages
        dfr <- dplyr::summarize(dplyr::group_by(dfr, message), varname = paste(varname, collapse = ", "))
        msgs <-
          mapply(dfr$varname, dfr$message,
                 FUN = function(name, msg) {
                   nm <- dQuote(strsplit(name, ", ", fixed = TRUE)[[1]])
                   if (length(nm) > 5) { # truncate variable names for the current warning message group
                     nm <- c(head(nm, 4), "...")
                   }
                   paste0("For ", paste(nm, collapse = ", "), ": ", msg)
                 })
        unname(msgs)
      }

      ERRORs <- collapse_msgs("error")
      WARNINGs <- collapse_msgs("warning")
      MESSAGEs <- collapse_msgs("message")

      # combine results for the indicator functions related overview
      # RESs contains a logical vector indicating results from all_of_f that can be combined according to the caller
      # slot indicates which results to combine
      combine_res <- function(all_of_f, RESs, slot) {
        if (!all(RESs | NULLs)) {
          # NULLs contains all results that are NULL
          # check that all results can be combined, except missing results
          browser()
        }

        # extract all call attributes to combine them
        clls <- lapply(all_of_f[RESs & !NULLs], attr, "call")
        clls <- lapply(clls, deparse)
        clls <- vapply(clls, paste0, collapse = "\n",
                       FUN.VALUE = character(1))
        clls <- paste0(clls, "$", slot, collapse = ", \n\t")
        clls <- paste0("rbind(\n\t", clls, "\n)")

        # combine results (ReportSummaryTable and tables)
        # select results according to the logical vectors, extract the corresponding slots, and then bind by row
        # then write the combined result to all_of_f keeping its original structure (a list of encapsulated lists)
        all_of_f <- list(setNames(list(do.call(rbind, lapply(all_of_f[RESs & !NULLs],
                                                      `[[`,
                                                      slot))), nm = slot))
        attr(all_of_f[[1]], "call") <- clls
        # reattach the error/message/warning attributes but now using the combined version (in ERRORs/WARNINGs,...)
        if (any(trimws(ERRORs) != ""))
          attr(all_of_f[[1]], "error") <- list(simpleError(paste(ERRORs, collapse = "\n")))
        else
          attr(all_of_f[[1]], "error") <- list()
        if (any(trimws(WARNINGs) != ""))
          attr(all_of_f[[1]], "warning") <- list(simpleWarning(paste(WARNINGs, collapse = "\n")))
        else
          attr(all_of_f[[1]], "warning") <- list()
        if (any(trimws(MESSAGEs) != ""))
          attr(all_of_f[[1]], "message") <- list(simpleMessage(paste(MESSAGEs, collapse = "\n")))
        else
          attr(all_of_f[[1]], "message") <- list()
        names(all_of_f) <- fkt

        return(all_of_f)
      }

      if (is_single_var) { # use results as they are, not combined by combine_res. see else branch below for comments on mapply
        output <- do.call(htmltools::tagList, unname(mapply(dqr = all_of_f,
                                                            nm = names(all_of_f),
                                                            FUN = pretty_print,
                                                            MoreArgs =
                                                              list(
                                                                is_single_var =
                                                                  is_single_var),
                                                            SIMPLIFY = FALSE)))
        # if (fkt == "con_inadmissible_categorical" && cur_var == "SBP_0") browser()
        # if we have some result, then store it in the svp list for the current single variable page
        if (length(output) && !all(vapply(output, is.null, FUN.VALUE = logical(1)))) {
          fkt_name <- cll_nm2fkt_nm(fkt)
          # title <- .manual$titles[[cll_nm2fkt_nm(fkt)]]
          title <- util_alias2caption(fkt)
          description <- .manual$descriptions[[cll_nm2fkt_nm(fkt)]]

          svp[[fkt]] <- htmltools::tagList(
            htmltools::span( # span is requird to make [[2]][[1]]$link on page navi menu creation below work
            htmltools::h4(style = "margin-top: 3em", # add function name as a ~subtitle, add space to prevent plot.lys to overlap
                          htmltools::a(
                            target = "_blank",
                            href = online_ref(fkt), title)),
                          htmltools::h5(paste(unique(c(fkt, fkt_name)),
                                              collapse = ": ")),
                          htmltools::p(description)),
                                           output)
        } else {
          svp[[fkt]] <- NULL
        }
      } else { # indicator overview page
        if ((startsWith_prefix._or_equals_prefix(fkt, "con_limit_deviations") || # check if we are working with a limits function
            startsWith_prefix._or_equals_prefix(fkt, "con_hard_limits") ||
            startsWith_prefix._or_equals_prefix(fkt, "con_soft_limits") ||
            startsWith_prefix._or_equals_prefix(fkt, "con_detection_limits"))
            && (any(PLOT_LISTSs) || any(PLOTs))) {
          ; # use the limits plots, if available, not the ReportSummaryTable if we are working with a limits function
        # check if we have to combine some single variable results to a multivariable result
        } else if ((!any(RSTs)) && (any(PLOT_LISTSs) || any(PLOTs))) {
          ; # use the plots, if available
          # otherwise, use the ReportSummaryTable, SummaryData, or SummaryTable, in that order of preference
          # and combine the results (rbind) using combine_res
        } else if (any(RSTs)) {
          all_of_f <- combine_res(all_of_f, RSTs, "ReportSummaryTable")
        } else if (any(SDs)) {
          all_of_f <- combine_res(all_of_f, SDs, "SummaryData")
        } else if (any(STs)) {
          all_of_f <- combine_res(all_of_f, STs, "SummaryTable") # TODO: Add "VariableGroupTable", "VariableGroupData",
        }

        # create a page by iterating over all_of_f entries and their respective names
        # for each element the pretty_print function is called, converting each dataquieR result to an htmltools compatible object
        # the do.call(htmltools::tagList) converts the output as an htmltools compatible tag list ("as.tagList.list")
        # we have to unname the list of tags to prevent taglists from generating named html attributes
        page <- do.call(htmltools::tagList, unname(mapply(dqr = all_of_f,
                                                          nm = names(all_of_f),
                                                          FUN = pretty_print,
                                                          MoreArgs =
                                                            list(
                                                              is_single_var =
                                                                is_single_var),
                                                          SIMPLIFY = FALSE
                                                          )))
        # check that the length of the page is positive and that it does not only contain null
        if ((length(page) && !all(vapply(page, is.null, FUN.VALUE = logical(1))))) {
          if ("link" %in% unique(unlist(lapply(page, names)))) { # if we have page navigation links
            # extract link information for the page navigation menu (created by pretty_print)
            all_links <- lapply(page, `[[`, "link")
            # add links to the page navigation menu
            all_links <- all_links[vapply(all_links, length, # remove empty links
                                          FUN.VALUE = integer(1)) != 0]
            all_links <- lapply(all_links, htmltools::tags$li) # add li to each tag to create the items
            all_links <- do.call(htmltools::tagList, all_links) # convert the list to a tag list, which is htmltools compatible
            page_menu <- util_float_index_menu(object = all_links) # create the menu
            # remove the original links (as created by pretty_print in the wrong places)
            page <- lapply(page, function(p) {
              p$link <- NULL
              p
            })
            # generates pages with navigation menu
            # load concept to get current indicator links in reports
            concept <- util_get_concept_info("dqi")
            concept <- concept[c("PublicID", "Indicator", "dataquieR function")]
            fkt2concept <- subset(concept, `dataquieR function` == cll_nm2fkt_nm(fkt))
            # create link tags
            get_links <- function(indicator_id, indicator_name) {
              htmltools::tags$a(target="_blank",
                     href= paste("https://dataquality.ship-med.uni-greifswald.de/id/#", indicator_id, sep = ""),
                     indicator_name)
            }
            # create un-ordered item list for each indicator
            links <- mapply(get_links, indicator_id = fkt2concept$PublicID,
                            indicator_name = fkt2concept$Indicator, SIMPLIFY = FALSE)
            names(links) <- NULL
            items <- lapply(links, htmltools::tags$li)
            items_indicators <- htmltools::tags$ul(items)
            # call template
            page <- htmltools::htmlTemplate(
              system.file("templates", template, "single_indicator_with_menu.html",
                          package = utils::packageName()),
              page_menu = page_menu,
              fkt = fkt,
              fkt_name = cll_nm2fkt_nm(fkt),
              title = .manual$titles[[cll_nm2fkt_nm(fkt)]],
              description = .manual$descriptions[[cll_nm2fkt_nm(fkt)]],
              page = page,
              online_ref = online_ref,
              items_indicators = items_indicators
            )

            if (dm %in% c("acc")) {  # acc is in separate files
              fname <- paste0("dim_", dm, "_", fkt, ".html") # add the function name for accuracy
            } else {
              fname <- paste0("dim_", dm, ".html") # define the file name, one file per dimension if not in accuracy
            }
            # the "dim_" prefix is required because otherwise windows will ignore a file called con.html confusing it with a special device con:
          } else { # for pages without navigation menu, not used
            page <- htmltools::htmlTemplate( #
              system.file("templates", template, "single_indicator.html",
                          package = utils::packageName()),
              fkt = fkt,
              fkt_name = cll_nm2fkt_nm(fkt),
              title = .manual$titles[[cll_nm2fkt_nm(fkt)]],
              description = .manual$descriptions[[cll_nm2fkt_nm(fkt)]],
              page = page,
              online_ref = online_ref
            )
            fname <- paste0("dim_", dm, ".html") # define the file name, one file per dimension if not in accuracy
          }
          append_single_page(drop_down,
                             util_alias2caption(fkt),
                             fname,
                             page)
        }
      }
    }
  } # loop over the dimensions ends here
    if (is_single_var) { # for single variable pages nothing has been added as page yet (using append_single_page)
      # vn <- util_find_var_by_meta(
      #   resp_vars = cur_var,
      #   meta_data = attr(report, "meta_data"),
      #   label_col = attr(report, "label_col"),
      #   target = VAR_NAMES)

      title <- htmltools::h1(cur_var)

      meta_data <- attr(report, "meta_data")
      label_col <- attr(report, "label_col")
      cur_md <- meta_data[meta_data[[label_col]] == cur_var,
                          , FALSE] # subset the metadata for the current variable to show it in the report

      md_info <- htmltools::tagList(
        anchor = util_generate_anchor_tag(cur_var, "meta_data", "variable"),
        link = util_generate_anchor_link(cur_var, "meta_data", "variable"), # these links are moved later
        htmltools::h1("Item-level Metadata"),
        util_html_table(data.frame(Name = names(cur_md),
                                   Value = unname(t(cur_md))),
                        meta_data = attr(report, "meta_data"),
                        label_col = attr(report, "label_col"),
                        copy_row_names_to_column = FALSE,
                        dl_fn = paste("Item-level_Metadata_", cur_var),
                        output_format = "HTML") # ignore row names (created by data.frame)
      )

      svp <- c(list(md_info = list("", list(md_info))), # add the metadata info to the svp keeping the same structure of svp
               svp)

      body <- do.call(htmltools::tagList, unname(svp))
      # move the links to the correct position to create the page navigation menu
      # this time variable-wise (see code above for comments)
      all_links <- lapply(body, function(x) x[[2]][[1]]$link)
      all_links <- all_links[vapply(all_links, length,
                                    FUN.VALUE = integer(1)) != 0]
      all_links <- lapply(all_links, htmltools::tags$li)
      all_links <- do.call(htmltools::tagList, all_links)

      page_menu <- util_float_index_menu(object = all_links)
      body <- lapply(body, function(x) {
        x[[2]][[1]]$link <- NULL
        x
      })
      body <- htmltools::tagList(page_menu, body)

      page <- htmltools::tagList(title, body)

      append_single_page("Single Variables",
                         cur_var,
                         sprintf("VAR_%s.html", prep_link_escape(cur_var, #remove special characters from variable name
                                                                 html = TRUE)),
                         page)

      svp <- list()
    }
  })

# TODO: if https://community.plotly.com/t/cant-show-heatmap-inside-div-error-something-went-wrong-with-axis-scaling/30616/9 is fixed, use auto-sizing

  progress_msg("Page generation", "Generating indicator based pages")

  eval(show_all_dims) # creates the indicator related pages because cur_var has not yet been set

  util_setup_rstudio_job("Page generation")

  progress_msg("Page generation", "Generating single variable pages")

  i <- 0
  n <- nrow(report)

  for (cur_var in rownames(report)) { # create the single variable pages
    progress(i/n * 100)
    util_message("Single Variable %s", cur_var)
    eval(show_all_dims)
    i <- i + 1
    progress(i/n * 100)
  }

# browser()
  pages
}
