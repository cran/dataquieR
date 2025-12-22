#' make an html dashboard for a report
#'
#' @param report [dataquieR report v2][dq_report2]
#' @param make_links [logical] add links to variables
#'
#' @return a `htmltools` compatible dashboard of report results
#'         or `NULL`, if package is missing
#' @noRd
util_setup_dashboard <- function(report, make_links = FALSE) {
  if (!util_ensure_suggested(c("htmltools"), err = FALSE)) {
    return(NULL)
  }
  # IDEA: Group by Variables, if wanted:
  # https://datatables.net/extensions/rowgroup/examples/initialisation/customRow.html
  title <- attr(report, "title")
  if (is.null(title)) {
    title <- "Data Quality Report"
  }

  table <- util_dashboard_table(summary(report)) #TODO: test with filter to un-existing functions.

  #
  # return early if table is empty anyway
  #
  if (is.null(table) || nrow(table) == 0 || ncol(table) == 0) {
      return()
  }

  margin <- 2


  #
  # If SummaryPlot or SummaryPlotList are there and ggplot objects,
  # add them for each result
  #
  if (attr(report, "label_col") ==
      attr(table, "label_col")) {
    plot_names <- c("SummaryPlot", "SummaryPlotList")
    table$Figure <- mapply(
      SIMPLIFY = FALSE,
      var_label = table[[attr(table, "label_col")]],
      call_name = table$call_names,
      function(var_label, call_name) {
        fn_result_name <- paste0(call_name, ".", var_label)
        if (fn_result_name %in% names(report) &&
          any(plot_names %in% names(report[[fn_result_name]]))) {

          result_plot_name <- head(intersect(plot_names
                            , names(report[[fn_result_name]]))
                        , 2)
          if (util_is_gg(report[[fn_result_name]][[result_plot_name]])) {
            result <- htmltools::plotTag(
              {
                withr::local_par(list(
                  mar = rep(margin, 4),
                  oma = rep(0, 4)
                ))
                suppressWarnings(suppressMessages(
                  print(report[[fn_result_name]][[result_plot_name]])
                ))
              },
              width = 250,
              height = 200,
              alt = paste("Figure")
            )
            as.character(result)
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      }
    )
  }

  #
  # if a row has a figure and it's value is "T" or "F", replace the value with the figure
  #
  logicals_with_figure = !is.na(table$Figure) & table$value %in% c("T", "F")
  table$value[logicals_with_figure] <-table$Figure[logicals_with_figure]

  #
  # get continuous and categorical summaries
  #
  add_summary_graphs <- function(table, summary_name, graph_name) {
    if (summary_name %in% colnames(report)) {
      des <- report[, summary_name, "SummaryTable"]$SummaryTable
      des[[attr(report, "label_col")]] <-
        des$Variables
      des[[graph_name]] <- des$Graph
      des$Graph <- NULL
      if (!is.null(dim(des)) && nrow(des) > 0) {
        table <- suppressWarnings(merge(table, des, all.x = TRUE,
                                     by = attr(report, "label_col"),
                                     suffixes = c("", "")))
        table <- util_fix_merge_dups(table)
      }
    }
    table
  }

  table <- add_summary_graphs(table, "des_summary_continuous", "GraphCon")
  table <- add_summary_graphs(table, "des_summary_categorical", "GraphCat")

  if (!"GraphCat" %in% colnames(table)) {
    table$GraphCat <- NA
  }
  if (!"GraphCon" %in% colnames(table)) {
    table$GraphCon <- NA
  }

  #
  #  if both GraphCon and GraphCat are there, GraphCon is default and GraphCat is fallback.
  #  otherwise, take what is there
  #
  if ("des_summary_continuous" %in% colnames(report) &&
      "des_summary_categorical" %in% colnames(report)  ) {
    table$Graph <- table$GraphCon
    table$GraphCon <- NULL
    table$Graph[is.na(table$Graph)] <- table$GraphCat[is.na(table$Graph)]
    table$GraphCat <- NULL
  } else if ("des_summary_continuous" %in% colnames(report)) {
    table$Graph <- table$GraphCon
    table$GraphCon <- NULL
  } else if ("des_summary_categorical" %in% colnames(report)) {
    table$Graph <- table$GraphCat
    table$GraphCat <- NULL
  }

  # put Graph to the left if it is there
  table <- table[, intersect(unique(c("Graph", colnames(table))), colnames(table)), FALSE]

  if (make_links) {
    table0 = table
    #
    # for each of label_col, "Graph" and VAR_NAMES that is in colnames, make a link from the content
    #
    for (cn in intersect(colnames(table), c(
      attr(report, "label_col"),
      "Graph",
      VAR_NAMES
    ))) {
      cnt <- table0[[cn]]
      lb <- table0[[attr(report, "label_col")]]
      cl <- table0[["call_names"]]
      table[[cn]] <- mapply(SIMPLIFY = FALSE,
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
    rm("table0")
  }

  # get indices of colnames and subtract 1 so they make sense for javascript
  var_class_col_js_idx <- which(colnames(table) == "var_class") - 1
  Class_col_js_idx <- which(colnames(table) == "Class") - 1
  class_raw_col_js_idx <- which(colnames(table) == "class") - 1
  name_col_js_idx <- which(colnames(table) == attr(report, "label_col")) - 1

  # translate all colnames
  colnames(table) <- util_translate(colnames(table), ns = "dashboard_table")




  #
  # unlist all columns that are lists
  #
  list_columns <- vapply(table, is.list, FUN.VALUE = logical(1))

  if (any(list_columns)) {
    table[list_columns] <-
      lapply(table[list_columns], unlist )
  }

  #
  # make actual dashboard from the data
  #
  my_dashboard <-
    util_html_table(
      table,
      descs = setNames(rep("", ncol(table)),
                       colnames(table)),
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
        all = colnames(table)
      ),
      initial_col_tag = "init",

      # '{"criteria":[{"condition":"!=","data":"var_class","type":"string","value":["Ok"]},{"condition":"!null","data":"Class","type":"string","value":[]}],"logic":"AND"}'
      # xx <- jsonlite::parse_json(...)
      # cat(deparse(xx), sep = "\n")
      #
      # initial search criteria: var_class != grading_class[[1]] && Class != Null
      #
      init_search =
        list(criteria = list(list(condition = "!=",
                                  data = util_translate("var_class", ns = "dashboard_table"),
                                  type = "string",
                                  value = list(util_get_labels_grading_class()[["1"]])),
                             list(condition = "!null",
                                  data = util_translate("Class", ns = "dashboard_table"),
                                  type = "string", value = list())), logic = "AND"),
      #
      # arguments that are passed directly to javascript
      #
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
  my_dashboard
}
