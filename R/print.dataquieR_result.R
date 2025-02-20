#' Print a [dataquieR] result returned by [dq_report2]
#' @aliases dataquieR_result
#' @param x [list] a dataquieR result from [dq_report2] or
#'                 [util_eval_to_dataquieR_result]
#' @param ... passed to print. Additionally, the argument `slot` may be passed
#'            to print only specific sub-results.
#' @seealso [util_pretty_print()]
#' @return see print
#' @export
print.dataquieR_result <- function(x, ...) {
  old_opts <- options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.ERRORS_WITH_CALLER = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE,
    dataquieR.MESSAGES_WITH_CALLER = FALSE)
  on.exit(old_opts)
  if (length(attr(x, "message")) > 0) {
    for (m in attr(x, "message")) {
      util_message(m)
    }
  }
  if (length(attr(x, "warning")) > 0) {
    for (w in attr(x, "warning")) {
      util_warning(w)
    }
  }
  error_shown <- FALSE
  if (length(attr(x, "error")) > 0) {
    e <- attr(x, "error")[[1]]
    try(util_error(e))
    error_shown <- TRUE
  }
  attr(x, "message") <- NULL
  attr(x, "warning") <- NULL
  attr(x, "error") <- NULL
  if (inherits(x, "empty")) {
    return()
  }
  class(x) <- setdiff(class(x), c("dataquieR_result", "square_result_list"))
  if (inherits(x, "dataquieR_NULL")) {
    x <- NULL
  }
  opts <- list(...)
  if ("slot" %in% names(opts)) {
    if (opts$slot %in% names(x)) {
      if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                      FALSE)))
        print(x[[opts$slot]])
      else
        invisible(x[[opts$slot]])
    } else {
      if (!error_shown) util_error("Cannot find %s in result", opts$slot)
    }
  } else {
    # TODO: switch based on context (knitting, interactive, ...)
    if (error_shown && is.null(x)) {
      return()
    }
    if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                       FALSE))) {
      print(x, ...) # NextMethod()
    } else {
      invisible(x)
    }
  }
}

ALLOWED_DATAQUIER_RESULT_NAMES <- character(0)

(function() {
  prefixes <-
    c("Result", "Dataframe", "Segment", "Summary", "VariableGroup")
  pre_prefixes <-
    c("Modified", "Flagged", "")
  suffixes <-
    c("Table", "Data")
  singleton <-
    c("ReportSummaryTable",
      "SummaryPlot",
      "SummaryPlotList",
      "PlotlyPlot",
      "DataTypePlotList",
      "DataframeDataList",
      "SegmentDataList",
      "VariableGroupPlotList",
      "ModifiedStudyData",
      "FlaggedStudyData",
      "OtherData",
      "OtherTable",
      "Other")
  all_names <- expand.grid(
    pre_prefixes, prefixes, suffixes
  )
  assign("ALLOWED_DATAQUIER_RESULT_NAMES",
         c(apply(all_names, 1, paste0, collapse = ""), singleton),
         parent.frame())
})()

util_is_gg <- function(x) {
  inherits(x, "gg") || ggplot2::is.ggplot(x)
}

# r <- dq_report2("ship", meta_data_v2 = "ship_meta_v2", dimensions = NULL); r2 <- dq_report2("study_data", meta_data_v2 = "meta_data_v2", dimensions = NULL);
# find internal errors: which(vapply(lapply(r2, attr, "error"), inherits, "dataquieR_invalid_result_error", FUN.VALUE = logical(1)))
# see https://nextcloud.uni-greifswald.de/index.php/apps/onlyoffice/23259435?filePath=%2F5230.group_qig%2FTools%2FdataquieR%2FdataquieR%20revised%20metadata%2Fdq_control%20revised%20v3-9_E8.xlsx
util_dataquieR_result <- function(r) {
  if (inherits(r, "dataquieR_NULL") ||
      length(attr(r, "error")) == 1) {
    return(r)
  }
  util_stop_if_not(is.list(r))
  # if (any(vapply(r, is.null, FUN.VALUE = logical(1)))) browser() -- this somehow happened?!
  # r <- r[!vapply(r, is.null, FUN.VALUE = logical(1))]
  # if (length(r) == 0) {
  #   class(r) <- union(class(r), "dataquieR_NULL")
  #   return(r)
  # }
  util_stop_if_not(all(trimws(names(r)) != ""))
  which_not <- names(r)[
    !startsWith(names(r), "ScalarValue_") &
    !names(r) %in% ALLOWED_DATAQUIER_RESULT_NAMES]
  if (length(which_not) > 0) {
    util_error(c("Internal error, sorry. Found an unexpected",
                 "result %s, please report."),
               util_pretty_vector_string(which_not))
  }
  .util_is_data_frame_or_length0 <- function(x) {
    length(x) == 0 || is.data.frame(x)
  }
  util_stop_if_not(all(vapply(r[endsWith(names(r), "Table")],
                              .util_is_data_frame_or_length0, FUN.VALUE = logical(1))))
  TableSlots <- endsWith(names(r), "Table")  &
    vapply(r, .util_is_data_frame_or_length0, FUN.VALUE = logical(1))
  if (any(TableSlots)) {
    for (TableSlot in names(r)[TableSlots]) {
      class(r[[TableSlot]]) <- union("TableSlot", class(r[[TableSlot]]))
      if (inherits(r[[TableSlot]], "ReportSummaryTable")) {
        class(r[[TableSlot]]) <- union("ReportSummaryTable",
                                       class(r[[TableSlot]]))
      }
    }
  }
  DataSlots <- endsWith(names(r), "Data") &
    ! endsWith(names(r), "StudyData") &
    vapply(r, .util_is_data_frame_or_length0, FUN.VALUE = logical(1))
  if (any(DataSlots)) {
    for (DataSlot in names(r)[DataSlots]) {
      class(r[[DataSlot]]) <- union("DataSlot", class(r[[DataSlot]]))
    }
  }
  StudyDataSlots <- endsWith(names(r), "StudyData") &
    vapply(r, .util_is_data_frame_or_length0, FUN.VALUE = logical(1))
  if (any(StudyDataSlots)) {
    for (StudyDataSlot in names(r)[StudyDataSlots]) {
      class(r[[StudyDataSlot]]) <- union("StudyDataSlot",
                                         class(r[[StudyDataSlot]]))
    }
  }
  if ("Other" %in% names(r)) {
    class(r[["Other"]]) <- union("Other", class(r[["Other"]]))
  }
  if ("ReportSummaryTable" %in% names(r)) {
    util_stop_if_not(inherits(r$ReportSummaryTable, "ReportSummaryTable"))
  }
  if ("PlotlyPlot" %in% names(r)) {
    util_stop_if_not(inherits(r$PlotlyPlot, "plotly"))
  }
  if ("SummaryPlot" %in% names(r)) {
    util_stop_if_not(util_is_gg(r$SummaryPlot))
  }
  if ("SummaryPlotList" %in% names(r)) {
    util_stop_if_not(is.list(r$SummaryPlotList))
    util_stop_if_not(all(vapply(r$SummaryPlotList,
                                util_is_gg,
                                FUN.VALUE = logical(1))))
  }
  if ("DataTypePlotList" %in% names(r)) {
    util_stop_if_not(is.list(r$DataTypePlotList))
    util_stop_if_not(all(vapply(r$DataTypePlotList,
                                util_is_gg,
                                FUN.VALUE = logical(1))))
  }
  if ("DataframeDataList" %in% names(r)) {
    util_stop_if_not(is.list(r$DataframeDataList))
  }
  if ("SegmentDataList" %in% names(r)) {
    util_stop_if_not(is.list(r$SegmentDataList))
  }
  if ("VariableGroupPlotList" %in% names(r)) {
    util_stop_if_not(is.list(r$VariableGroupPlotList))
    util_stop_if_not(all(vapply(r$VariableGroupPlotList,
                                util_is_gg,
                                FUN.VALUE = logical(1))))
  }
  if ("ResultTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$ResultTable))
    util_stop_if_not(length(r$ResultTable) == 0 ||
                       ncol(r$ResultTable) == 0 ||
                       "ResultName" %in% colnames(r$ResultTable))
  }
  if ("ResultData" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$ResultData))
  }
  if ("OtherData" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$OtherTable))
  }
  if ("OtherTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$OtherTable))
  }
  if ("DataframeTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$DataframeTable))
    util_stop_if_not(length(r$DataframeTable) == 0 ||
                       ncol(r$DataframeTable) == 0 ||
                       "DF_NAME" %in% colnames(r$DataframeTable))
  }
  if ("SegmentTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$SegmentTable))
    util_stop_if_not(length(r$SegmentTable) == 0 ||
                       ncol(r$SegmentTable) == 0 ||
                       "Segment" %in% colnames(r$SegmentTable))
  }
  if ("SummaryTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$SummaryTable))
    util_stop_if_not(length(r$SummaryTable) == 0 ||
                       ncol(r$SummaryTable) == 0 ||
                       "Variables" %in% colnames(r$SummaryTable))
  }
  if ("ReportSummaryTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$ReportSummaryTable))
    util_stop_if_not(length(r$ReportSummaryTable) == 0 ||
                       ncol(r$ReportSummaryTable) == 0 ||
                       "Variables" %in% colnames(r$ReportSummaryTable))
    util_stop_if_not(length(r$ReportSummaryTable) == 0 ||
                       ncol(r$ReportSummaryTable) == 0 ||
                       "N" %in% colnames(r$ReportSummaryTable))
  }
  if ("VariableGroupTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$VariableGroupTable))
    util_stop_if_not(length(r$VariableGroupTable) == 0 ||
                       ncol(r$VariableGroupTable) == 0 ||
                       "VARIABLE_LIST" %in% colnames(r$VariableGroupTable))
  }
  class(r) <- union(c("dataquieR_result", "master_result"), class(r))
  r
}

#' Print a `StudyDataSlot` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return see print
#' @export
print.StudyDataSlot <- function(x, ...) {
  util_ensure_suggested("tibble")
  r <- tibble::as_tibble(x)
  if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                  FALSE)))
    print(r)
  else
    invisible(r)
}

#' Print a `DataSlot` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return see print
#' @export
print.DataSlot <- function(x, ...) {
  util_ensure_suggested("htmltools")
  r <- util_html_table(x, output_format = "HTML")
  if (!is.null(r)) r <- htmltools::browsable(r)
  if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                  FALSE))) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      util_ensure_suggested("knitr", "knit-print")
      class(x) <- setdiff(class(x), "DataSlot")
      knitr::knit_print(x)
    } else {
      print(r)
    }
  } else
    invisible(r)
}

#' Print a `TableSlot` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return see print
#' @export
print.TableSlot <- function(x, ...) {
  util_ensure_suggested("htmltools")
  r <- util_make_data_slot_from_table_slot(x)
  r <- util_html_table(r, output_format = "HTML")
  if (!is.null(r)) r <- htmltools::browsable(r)
  if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                     FALSE)))
    print(r)
  else
    invisible(r)
}

#' Print a `master_result` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return `invisible(NULL)`
#' @export
print.master_result <- function(x, ...) {
  util_ensure_suggested("htmltools")
  if (isTRUE(getOption('knitr.in.progress'))) {
    f <- withr::local_tempdir(.local_envir = knitr::knit_global())
  } else {
    f <- withr::local_tempdir(.local_envir = rlang::global_env())
  }
  withr::local_dir(f)
  jqui <- rmarkdown::html_dependency_jqueryui()
  jqui$stylesheet <- "jquery-ui.min.css"
  if (!is.null(attr(x, "function_name")) &&
      attr(x, "function_name") %in%
      c("con_limit_deviations",  # check if we are working with a limits function
        "con_hard_limits",
        "con_soft_limits",
        "con_detection_limits")) {
    # FIXME: Remove special treatment of con_limit_deviations in favor of suitable result slots in DQ_OBS
    x$ReportSummaryTable <- NULL
  }
  doc <- htmltools::tagList(rmarkdown::html_dependency_jquery(),
                            html_dependency_tippy(),
                            html_dependency_clipboard(),
                            html_dependency_dataquieR(iframe = FALSE),
                            jqui,
                            htmltools::div(class = "navbar"),
                     util_pretty_print(dqr = x, nm = attr(x, "cn"),
                                       is_single_var = FALSE,
                                       use_plot_ly = util_ensure_suggested("plotly", "plot interactive figures", err = FALSE),
                                       dir = f,
                                       ...),
                     htmltools::tags$script('$(function(){$("body").css("overflow", ""); })'),
                     # htmltools::tags$script('    setTimeout(function() {
                     # debugger
                     #                        window.dispatchEvent(new Event("resize")) }, 500)')
                     )
  htmltools::save_html(doc, "index.html")
  if (!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                  FALSE))) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      util_ensure_suggested("htmlwidgets", "Render results in RMarkdown")
      #sp <- htmlwidgets::sizingPolicy(fill = TRUE)

      util_ensure_suggested("knitr", "knit-print")
      if (knitr::is_latex_output()) { # TODO: LaTeX
        util_warning(c("%s in R markdown not yet supported by %s for printing",
                       "full results"),
                      sQuote(knitr::pandoc_to()), sQuote(packageName()))
        return("")
      } else if (knitr::is_html_output()) {
        return(knitr::knit_print(
          statichtmlWidget(doc,
                           js =
                             '$(function(){$("body").css("overflow", ""); })')))
      } else { # TODO: Word, ...?
        # knitr::opts_chunk$get("height")
        util_warning(c("%s in R markdown not yet supported by %s for printing",
                       "full results"),
                     sQuote(knitr::pandoc_to()), sQuote(packageName()))
        return("")
      }
    } else {
      viewer <- getOption("viewer", utils::browseURL)
      viewer("index.html")
    }
  }
  invisible(NULL)
}


# #' exportS3Method knitr::knit_print
# knit_print.Slot <- function(x, ...) {
#   util_ensure_suggested("knitr", "Call knit_print")
#   knitr::asis_output("***TEST***")
# }

#' Print a `Slot` object
#'
#' displays all warnings and stuff. then it prints `x`.
#'
#' @param x the object
#' @param ... not used
#'
#' @return calls the next print method
#' @export
print.Slot <- function(x, ...) {
  if (any(inherits(x, c("Other", "ReportSummaryTable")))) {
    return(NextMethod())
  }
  if (length(attr(x, "message")) > 0) {
    for (m in attr(x, "message")) {
      util_message(m)
    }
  }
  if (length(attr(x, "warning")) > 0) {
    for (w in attr(x, "warning")) {
      util_warning(w)
    }
  }
  error_shown <- FALSE
  if (length(attr(x, "error")) > 0) {
    e <- attr(x, "error")[[1]]
    try(util_error(e))
    error_shown <- TRUE
  }
  attr(x, "message") <- NULL
  attr(x, "warning") <- NULL
  attr(x, "error") <- NULL
  withr::with_pdf(NULL,
                  o <- capture.output(r <- NextMethod()))
  if (!is.null(r))
    class(r) <- setdiff(class(r), "Slot")
  if ((!("view" %in% names(list(...))) || (!identical(list(...)[["view"]],
                                                     FALSE)))) {
    print(r)
    # if (any(nzchar(o))) {
    #   cat(o, sep = "\n")
    # }
  } else {
    invisible(r)
  }
}
