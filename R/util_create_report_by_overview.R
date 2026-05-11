#' Create an overview of the reports created with `dq_report_by`
#'
#' writes to the files
#' `index.html`, `dashboard.html`, `tables.html` in `output_dir`.
#'
#' @param output_dir [character] the directory in which all reports are searched
#'                               and the overview is saved
#'
#' @return `invisible(NULL)`
#' @noRd
util_create_report_by_overview <- function(output_dir) {

  util_ensure_suggested("jsonlite",
                        goal = "overall-overviews",
                        err = TRUE)

  # prepare the path of the output_dir with the / at the end
  out_dir <- output_dir
  if (!endsWith(out_dir, .Platform$file.sep)) {
    out_dir <- paste0(out_dir, .Platform$file.sep)
  }

  strata_column <- NULL
  rm("strata_column")
  segment_column <- NULL
  rm("segment_column")
  strata_column_label <- NULL
  rm("strata_column_label")
  subgroup <- NULL
  rm("subgroup")
  mod_label <- NULL
  rm("mod_label")
  title <- NULL
  rm("title")
  disable_plotly <- NULL
  rm("disable_plotly")
  rep_id <- NULL
  rm("rep_id")
  start_time <- NULL
  rm("start_time")
  subtitle <- NULL
  rm("subtitle")
  author <- NULL
  rm("author")
  user_info <- NULL
  rm("user_info")
  by_call <- NULL
  rm("by_call")

  list2env(readRDS(file.path(out_dir, "report_by_meta.RDS")),
           envir = environment())
  # this contains as list of
  # @param strata_column [character] name of a study variable to stratify the
  #                                    report by. It can be null
  # @param segment_column [character] name of a metadata attribute
  #                                    usable to split the report in
  #                                    sections of variables. It can be null
  # @param strata_column_label [character] the label of the variable used as
  #                                           strata_column
  # @param subgroup [character] optional, to define subgroups of cases
  # @param mod_label [list] `util_ensure_label()` info
  # @param title [character] a title for the overview `HTML`-file
  # @param disable_plotly [logical] do not use `plotly`, even if installed
  # @param rep_id [character] unique ID for the report to detect changes
  # @param start_time [as.POSIXct] time point when this computation has started

  if (nzchar(trimws(subtitle))) {
    subtitle <- paste0(subtitle, ": ")
  }

  util_expect_scalar(title, check_type = is.character)

  packageName <- utils::packageName()

  doc_title <- title

  # create a summary of summaries
  # import all the R objects containing a report summary and create a list
  sum_names <- list.files(path = output_dir, pattern = "^report_summary_")
  sum_names2 <- vapply(sum_names, function(x)
    paste0(out_dir, x), FUN.VALUE = character(1))
  list_summaries2 <- lapply(sum_names2, FUN = readRDS)

  # create a toc
  toc <- lapply(sum_names, function(x) {
    path_name <- paste0(out_dir, x)
    if (!"report.html" %in%
        list.files(path = file.path(sub("/$", "", out_dir),
                                    sub("^report_summary", "report",
                                        sub(".RDS$", "", x)),
                                    ".report"))) {
      return(NULL)
    }
    temp_file <- readRDS(path_name)
    this1 <- attr(temp_file, "this")
    sdn <- this1$sdn
    path_name <- file.path(output_dir, gsub("[^a-zA-Z0-9_\\.]", "",
                                            sprintf("report_%s", sdn)))
    location <- gsub("[^a-zA-Z0-9_\\.]", "",
                     sprintf("report_%s", sdn))
    toc <- list()
    toc[[this1$stratum]] <- list()
    toc[[this1$stratum]][[this1$segment]] <- location
    return(toc)
  })
  toc <- toc[!vapply(toc, is.null, FUN.VALUE = logical(1))]

  # create a vector containing all variables and their location in folder
  vars_reportlocation <- lapply(sum_names, function(x) {
    name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)
    name_folder <- paste0("report_", name_folder)
    path_name <- paste0(out_dir, x)
    temp_file <- readRDS(path_name)
    this1 <- attr(temp_file, "this")
    used_data_file <- this1$used_data_file
    # leave only file name and extension, if the vector contains a path
    if (grepl(.Platform$file.sep, used_data_file, fixed = TRUE)) {
      used_data_file <- gsub(".+\\/(.+\\..+$)", "\\1", used_data_file)
    }
    stratum <- this1$stratum
    segm <- this1$segment
    # the complete name changes depending of the split selected
    if (is.null(strata_column)) {
      origin_name <- paste0(used_data_file, "-", segm)
    } else if (!is.null(strata_column) && is.null(segment_column)) {
      origin_name <- paste0(used_data_file, "-", stratum)
    } else if (!is.null(strata_column) && !is.null(segment_column)) {
      origin_name <- paste0(used_data_file, "-", stratum, "-", segm)
    }

    temp_res <- this1$result[, colnames(this1$result) %in%
                               c(VAR_NAMES, "call_names"), drop = FALSE]

    if (this1$label_col != VAR_NAMES) {
      temp_varnames_colnames <- this1$meta_data[, colnames(this1$meta_data) %in%
                                                  c(VAR_NAMES, this1$label_col)]
      temp_res$VAR_NAMES <- util_map_labels(temp_res$VAR_NAMES,
                                            meta_data = temp_varnames_colnames,
                                            from = VAR_NAMES,
                                            to = this1$label_col)
    }

    temp_res$md_names <- paste0(temp_res$VAR_NAMES,
                                rep(".", nrow(temp_res)),
                                temp_res$call_names)
    if (nrow(temp_res) == 0) {
      return(setNames(list(), nm = character(0)))
    }
    md_names <- temp_res$md_names
    md_names <- paste0(origin_name, "-", md_names)
    if (is.null(strata_column) || (!is.null(strata_column) &&
                                   length(list_summaries2) == 1)) {
      origin_name <- paste0(origin_name, "-")
      md_names <- sub(origin_name, "", md_names, fixed = TRUE)
    }

    md_names <- unique(md_names)
    label_col_and_folder <- list(setNames(rep(name_folder,
                                              times = length(md_names)),
                                          nm = md_names))
    return(label_col_and_folder)
  })

  vars_reportlocation <- unlist(vars_reportlocation)

  # In case of creation of report by strata (more than one strata) the VAR_NAMES
  # changes so a conversion dataframe is needed to create the links to the folders
  if (!is.null(strata_column) && length(list_summaries2) > 1) {
    # create a vector with the variable_names and the unique variable names
    # created when there is a strata selection
    vars_originalnames <- mapply(x = sum_names,
                                 SIMPLIFY = FALSE,
                                 FUN = function(x) {
                                   name_folder <- gsub("(report_summary_)(.*)\\.RDS",
                                                       "\\2", x)
                                   name_folder <- paste0("report_", name_folder)
                                   path_name <- paste0(out_dir, x)
                                   temp_file <- readRDS(path_name)
                                   this1 <- attr(temp_file, "this")
                                   used_data_file <- this1$used_data_file
                                   if (grepl(.Platform$file.sep,
                                             used_data_file,
                                             fixed = TRUE)) {
                                     used_data_file <- gsub(".+\\/(.+\\..+$)",
                                                            "\\1",
                                                            used_data_file)
                                   }
                                   stratum <- this1$stratum
                                   segm <- this1$segment
                                   # the complete name changes depending of the split selected
                                   if (is.null(strata_column)) {
                                     origin_name <- paste0(used_data_file,
                                                           "-",
                                                           segm)
                                   } else if (!is.null(strata_column) &&
                                              is.null(segment_column)) {
                                     origin_name <- paste0(used_data_file,
                                                           "-",
                                                           stratum)
                                   } else if (!is.null(strata_column) &&
                                              !is.null(segment_column)) {
                                     origin_name <- paste0(used_data_file,
                                                           "-",
                                                           stratum,
                                                           "-",
                                                           segm)
                                   }

                                   temp_res <- this1$result[, colnames(this1$result) %in%
                                                              c(VAR_NAMES),
                                                            drop = FALSE]
                                   temp_res <- temp_res[!duplicated(temp_res),
                                                        ,
                                                        drop = FALSE]
                                   temp_res$LABEL <- rep(NA, nrow(temp_res))
                                   if (this1$label_col != VAR_NAMES) {
                                     temp_varnames_colnames <-
                                       this1$meta_data[, colnames(this1$meta_data) %in%
                                                         c(VAR_NAMES,
                                                           this1$label_col)]
                                     temp_res$LABEL <- util_map_labels(
                                       temp_res$VAR_NAMES,
                                       meta_data = temp_varnames_colnames,
                                       from = VAR_NAMES,
                                       to = this1$label_col
                                     )
                                   }
                                   temp_res$md_names <- rep(NA, nrow(temp_res))
                                   md_names <- temp_res$VAR_NAMES
                                   md_names <- paste0(origin_name, "-", md_names)
                                   if (is.null(strata_column) ||
                                       (!is.null(strata_column) &&
                                        length(list_summaries2) == 1)) {
                                     origin_name <- paste0(origin_name, "-")
                                     md_names <- sub(origin_name,
                                                     "",
                                                     md_names,
                                                     fixed = TRUE)
                                   }
                                   if (nrow(temp_res) > 0) {
                                     temp_res$md_names <- md_names
                                   }

                                   temp_res$new_name_with_label <- paste0(temp_res$LABEL)
                                   new_name_with_label <- temp_res$new_name_with_label
                                   new_name_with_label <- paste0(origin_name,
                                                                 "-",
                                                                 new_name_with_label)
                                   if (is.null(strata_column) ||
                                       (!is.null(strata_column) &&
                                        length(list_summaries2) == 1)) {
                                     origin_name <- paste0(origin_name, "-")
                                     new_name_with_label <- sub(origin_name,
                                                                "",
                                                                new_name_with_label,
                                                                fixed = TRUE)
                                   }
                                   if (nrow(temp_res) > 0) {
                                     temp_res$new_name_with_label <- new_name_with_label
                                   }

                                   temp_res$name_matching_result2_Variables <-
                                     paste0(temp_res$LABEL)
                                   name_matching_result2_Variables <-
                                     temp_res$name_matching_result2_Variables
                                   name_matching_result2_Variables <-
                                     paste0(origin_name,
                                            " - ",
                                            name_matching_result2_Variables)
                                   if (is.null(strata_column) ||
                                       (!is.null(strata_column) &&
                                        length(list_summaries2) == 1)) {
                                     origin_name <- paste0(origin_name, "-")
                                     name_matching_result2_Variables <-
                                       sub(origin_name,
                                           "",
                                           name_matching_result2_Variables,
                                           fixed = TRUE)
                                   }
                                   if (nrow(temp_res) > 0) {
                                     temp_res$name_matching_result2_Variables <-
                                       name_matching_result2_Variables
                                   }
                                   label_col_and_folder <-
                                     data.frame(
                                       new_names_with_varnames = temp_res$md_names,
                                       new_names_with_label = temp_res$new_name_with_label,
                                       result2_Variables_match =
                                         temp_res$name_matching_result2_Variables,
                                       var_names = temp_res$VAR_NAMES,
                                       name_label = temp_res$LABEL
                                     )
                                   return(label_col_and_folder)
                                 })
  } else {
    vars_originalnames <- NULL
  }

  # create a summary of summaries to be included in the overview page
  if (is.null(strata_column)) {
    summary_all <- util_combine_list_report_summaries(list_summaries2,
                                                      type = "unique_vars")
  } else {
    summary_all <- util_combine_list_report_summaries(list_summaries2,
                                                      type = "repeated_vars")
  }

  # render the table and the pie chart of the overview summary
  TBSummaries <-
    util_render_table_dataquieR_summary(summary_all,
                                        folder_of_report = vars_reportlocation,
                                        var_uniquenames = vars_originalnames)
  PlotSummaries <- plot(summary_all,
                        dont_plot = TRUE,
                        disable_plotly = disable_plotly,
                        folder_of_report = vars_reportlocation,
                        var_uniquenames = vars_originalnames)

  dashboard_names <- list.files(path = output_dir,
                                pattern = "^report_dashboard_",
                                full.names = TRUE)

  all_dashboards <- lapply(setNames(nm = dashboard_names), FUN = function(f) {
    db <- readRDS(f)
    if (is.data.frame(db) && nrow(db) > 0) {
      if (is.null(strata_column)) {
        db$fq_VARNAME <- db$..VAR_NAMES
      } else {
        db$fq_VARNAME <- paste0(attr(db, "name_of_study_data"),
                                "-",
                                ifelse(!is.null(strata_column),
                                       paste0(attr(db, "level_name"), "-"),
                                       ""
                                ),
                                ifelse(!is.null(segment_column),
                                       paste0(
                                         util_with_orig_names(db)[[STUDY_SEGMENT]],
                                              "-"),
                                       ""
                                ),
                                db$..VAR_NAMES)
      }
    }
    db
  })
  all_dashboards_df <- util_rbind(data_frames_list = all_dashboards)
  rownames(all_dashboards_df) <- NULL
  full_sum <- util_add_links_to_summary_table(
    attr(summary_all, "this")$result,
    attr(summary_all, "this"),
    folder_of_report = vars_reportlocation)

  orig_cn <- colnames(all_dashboards_df)
  if (!!prod(dim(all_dashboards_df))) {
    to_rm <- util_untranslated_colnames(all_dashboards_df) %in%
      c("title", "href", "popup_href")
  } else {
    to_rm <- rep(FALSE, length(orig_cn))
  }

  ns <- attr(orig_cn, "ns")
  lang <- attr(orig_cn, "lang")
  if (!!length(orig_cn)) {
    class <- attr(orig_cn, "class")
  } else {
    class <- "dataquieR_translated"
  }
  nms <- attr(orig_cn, "names")

  all_dashboards_df[to_rm] <- NULL
  orig_cn <- orig_cn[!to_rm]
  nms <- nms[!to_rm]

  attr(orig_cn, "lang") <- lang
  attr(orig_cn, "ns") <- ns
  attr(orig_cn, "names") <- nms
  attr(orig_cn, "class") <- class

  util_translated_colnames(all_dashboards_df) <-
    orig_cn

  if (!!prod(dim(all_dashboards_df)) && !!prod(dim(full_sum))) {
    DashboardT <-
      merge(x = all_dashboards_df,
            y = full_sum[, -which(colnames(full_sum) == "value"), FALSE],
            by.x = util_translate(c("fq_VARNAME",
                                    "call_names",
                                    "indicator_metric"),
                                  as_this_translation = colnames(all_dashboards_df)),
            by.y = c("VAR_NAMES", "call_names", "indicator_metric"),
            all = FALSE)


    util_translated_colnames(DashboardT) <-
      util_translate(util_translate(colnames(DashboardT),
                                    as_this_translation = colnames(all_dashboards_df),
                                    reverse = TRUE),
                     as_this_translation = colnames(all_dashboards_df)) # TODO: avoid reverse

    DashboardT <- util_fix_columns_in_dashboard_for_overview(
      DashboardT,
      image_dir = file.path(output_dir, "dashboard_images")
    )

    Dashboard <- util_dashboard_table2widget(
      DashboardT,
      unique(lapply(lapply(list_summaries2, attr, "this"), `[[`, "label_col"))[[1]]
    )
  } else {
    Dashboard <- htmltools::HTML("")
  }

  if (!disable_plotly) {
    SunburstSummaries <- plot(summary_all,
                              dont_plot = TRUE,
                              disable_plotly = disable_plotly,
                              hierarchy = TRUE,
                              folder_of_report = vars_reportlocation,
                              var_uniquenames = vars_originalnames)
  } else {
    SunburstSummaries <- htmltools::HTML("")
  }

  # Create an all_ids file
  all_ids <- lapply(sum_names, function(x) {
    name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)

    path_all_ids_file <- file.path(out_dir,
                                   paste0("report_", name_folder),
                                   ".report",
                                   "anchor_list.RDS")
    if (!file.exists(path_all_ids_file)) {
      util_warning(
        "Cannot read %s for %s -- Internal error, sorry. Please report",
        dQuote(path_all_ids_file),
        dQuote(x)
      )
      return(character(0))
    }
    temp_file <- readRDS(path_all_ids_file)
    temp_file <- file.path(fsep = "/",
                           paste0("report_", name_folder),
                           ".report",
                           temp_file)
    return(temp_file)
  })

  # create an overall anchor list with all created results, useful not to
  # create links to non-existing results
  all_ids <- c(unlist(all_ids), "index.html", "dashboard.html", "tables.html")

  cat(
    sep = "",
    "window.all_ids = {\"all_ids\": ",
    paste0("[",
           paste0('"', all_ids, '"', collapse = ", "),
           "]"),
    "}",
    file = file.path(out_dir, "anchor_list.js")
  )

  # creates the html overview page that links all sub-reports created
  if (!is.null(strata_column) && !is.null(segment_column)) {
    level_seg <- names(unlist(toc))
    get_level <- function(str) {
      sub("\\..*", "", str)
    }
    sdlevel <- vapply(level_seg, get_level, FUN.VALUE = character(1))
    get_segm <- function(str) {
      sub(".*\\.", "", str)
    }
    segs <- vapply(level_seg, get_segm, FUN.VALUE = character(1))
    sdlevel <- substr(sdlevel, nchar(strata_column_label) + 2, 10000)
    title <- paste0("Report: ", segment_column, " = ", segs, ", ",
                    strata_column_label, " = ", sdlevel)
  } else if (is.null(strata_column) && !is.null(segment_column)) {
    level_seg <- names(unlist(toc))
    get_level <- function(str) {
      sub("\\..*", "", str)
    }
    sdlevel <- vapply(level_seg, get_level, FUN.VALUE = character(1))
    get_segm <- function(str) {
      sub(".*\\.", "", str)
    }
    segs <- vapply(level_seg, get_segm, FUN.VALUE = character(1))
    title <- paste0("Report: ", segment_column, " = ", segs, ".", sdlevel)
  } else if (!is.null(strata_column) && is.null(segment_column)) {
    sdlevel <- names(unlist(toc))
    sdlevel <- substr(sdlevel, nchar(strata_column_label) + 2, 10000)
    title <- paste("Report: ", strata_column_label, " = ", sdlevel)
  } else if (is.null(strata_column) && is.null(segment_column) &&
             !is.null(subgroup)) {
    if (is.character(title)) {
      title <- paste0(title, ". Subgroup: ", subgroup)
    } else {
      title <- paste0("Report on subgroup: ", subgroup)
    }
  } else {
    title <- paste0("Report on all variables and observations")
  }

  # create the link for the created reports
  if (length(toc) == 0) {
    href <- character(0)
  } else {
    href <- paste0(unlist(toc), "/.report/report.html")
  }
  util_ensure_suggested("htmltools", "generate reports TOC")

  has_legend <- exists("..INFO_SD_NAME_FOR_REPORT", .dataframe_environment())

  overview_css_dep <- htmltools::htmlDependency(
    name = "dataquieR.dq_report_by_overview",
    version = "0.0.1",
    src = system.file("menu", package = "dataquieR"),
    stylesheet = c("dataquieR_overview.css")
  )


  build_report_list <- function(href, title) {
    htmltools::tags$div(
      class = "dq-overview-card dq-overview-card-links",
      htmltools::tags$div(
        class = "dq-overview-card-title",
        "List of created reports"
      ),
      htmltools::tags$ul(
        class = "dq-overview-report-list",
        lapply(seq_along(href), function(i) {
          htmltools::tags$li(
            class = "dq-overview-report-item",
            htmltools::tags$a(
              class = "dq-overview-report-link",
              href = href[[i]],
              title[[i]]
            )
          )
        })
      )
    )
  }

  build_legend_block <- function() {
    info_sd <- as.list(prep_get_data_frame("..INFO_SD_NAME_FOR_REPORT"))

    htmltools::tags$div(
      class = "dq-overview-card dq-overview-card-legend",
      htmltools::tags$div(
        class = "dq-overview-card-title",
        "Legend"
      ),
      htmltools::tags$div(
        class = "dq-overview-legend-grid",
        lapply(names(info_sd), function(x) {
          htmltools::tags$div(
            class = "dq-overview-legend-item",
            htmltools::tags$div(
              class = "dq-overview-legend-key",
              x
            ),
            htmltools::tags$div(
              class = "dq-overview-legend-value",
              as.character(info_sd[[x]])
            )
          )
        })
      )
    )
  }

  build_tab_nav <- function(active_file) {
    tabs <- list(
      list(file = "index.html", label = "Sunburst"),
      list(file = "dashboard.html", label = "Dashboard"),
      list(file = "tables.html", label = "Summary Table")
    )

    htmltools::tags$div(
      class = "dq-overview-tabs",
      lapply(tabs, function(tab) {
        classes <- "dq-overview-tab"
        if (identical(tab$file, active_file)) {
          classes <- paste(classes, "active")
        }
        htmltools::tags$a(
          class = classes,
          href = tab$file,
          `data-no-existance-check` =
            jsonlite::toJSON(TRUE, auto_unbox = TRUE),
          tab$label
        )
      })
    )
  }

  build_footer <- function(start_time, end_time) {
    p <- NULL
    if (is.list(user_info)) {
      # TODO: jsTree
      p <- user_info
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


    htmltools::tagList(
      util_html_table(
        title = prep_title_escape(doc_title, TRUE),
        rotate_for_one_row = TRUE,
        util_rbind(data.frame(
          check.names = FALSE,
          fix.empty.names = FALSE,
          `Overall computation time` =
            as.character(htmltools::span(`data-content` = "renderingTime")),
          `dataquieR version` = util_dataquieR_version(),
          `Timestamp` = format(start_time),
          `Author` = author,
          `Call` =
            as.character(htmltools::pre(paste0(deparse(by_call),
                                               collapse = "\n")))
        ), p)
      )
    )
  }

  build_section_card <- function(title_text, content) {
    htmltools::tags$div(
      class = "dq-overview-section-card",
      htmltools::tags$div(
        class = "dq-overview-section-heading",
        title_text
      ),
      htmltools::tags$div(
        class = "dq-overview-section-body",
        content
      )
    )
  }

  build_overview_toc <- function(active_file,
                                 include_plot_summaries = FALSE,
                                 include_sunburst_summaries = FALSE,
                                 include_dashboard = FALSE,
                                 include_tb_summaries = FALSE,
                                 end_time) {

    summary_sections <- list()

    if (include_plot_summaries) {
      summary_sections <- c(
        summary_sections,
        list(build_section_card("Plots", PlotSummaries))
      )
    }
    if (include_sunburst_summaries) {
      summary_sections <- c(
        summary_sections,
        list(build_section_card("Sunburst", SunburstSummaries))
      )
    }
    if (include_dashboard) {
      summary_sections <- c(
        summary_sections,
        list(build_section_card("Dashboard", Dashboard))
      )
    }
    if (include_tb_summaries) {
      summary_sections <- c(
        summary_sections,
        list(build_section_card("Tables", TBSummaries))
      )
    }

    header_subtitle <- paste0(
      util_by_header_from_args(by_call),
#      "\nInteractive overview of created reports",
      if (is.character(doc_title) && length(doc_title) == 1L &&
          nzchar(doc_title)) {
        paste0(" \u2014 ", doc_title)
      } else {
        ""
      }
    )

    do.call(
      htmltools::tagList,
      list(
        htmltools::tags$script(src = ".report/renderinfo.js",
                               type = "text/javascript"),
        htmltools::div(class = "navbar"),
        htmltools::tags$script(
          type = "text/javascript",
          "window.dq_report_by_overview = true"
        ),
        htmltools::tags$script(
          type = "text/javascript",
          src = "anchor_list.js"
        ),
        htmltools::tags$div(
          class = "dq-overview-app",
          htmltools::tags$div(
            class = "dq-overview-shell",
            htmltools::tags$div(
              class = "dq-overview-topbar",
              htmltools::tags$div(
                class = "dq-overview-eyebrow",
                "dataquieR overview"
              ),
              htmltools::tags$h1(
                class = "dq-overview-title",
                doc_title
              ),
              htmltools::tags$p(
                class = "dq-overview-subtitle",
                paste0(author, ": ", header_subtitle,
                       " (", format(start_time), ")")
              ),
              build_tab_nav(active_file)
            ),
            htmltools::tags$div(
              class = "dq-overview-main",
              summary_sections,
              build_report_list(href = href, title = title),
              if (has_legend) build_legend_block()
            ),
            htmltools::tags$div(
              class = "dq-overview-footer",
              build_footer(start_time = start_time,
                           end_time = end_time)
            )
          )
        )
      )
    )
  }

  if (nchar(mod_label$label_modification_text) > 0) {
    notes_labels <- htmltools::div(
      htmltools::h3("Label modifications"),
      util_html_table(util_df_escape(mod_label$label_modification_table),
                      dl_fn = "Label_modifications")
    )
  } else {
    notes_labels <- htmltools::div()
  }

  logo_rel <- "logo.png"

  file.copy(system.file("logos",
                        "dataquieR_48x48.png",
                        package = packageName),
            file.path(output_dir, logo_rel))

  save_overview_html <- function(toc, file_name) {
    htmltools::save_html(
      htmltools::tagList(
        overview_css_dep,
        rmarkdown::html_dependency_jquery(),
        html_dependency_dataquieR(iframe = FALSE),
        htmltools::tags$head(
          htmltools::tags$script(
            type = "text/javascript",
            "window.dq_report_by_overview = true"
          ),
          htmltools::tags$title(doc_title),
          htmltools::tags$link(
            rel = "icon",
            type = "image/png",
            href = logo_rel
          ),
          htmltools::tags$meta(name = "viewport",
                               content = "width=device-width, initial-scale=1")
        ),
        htmltools::HTML("<!-- done -->"),
        toc,
        htmltools::tags$div(
          class = "dq-overview-shell dq-overview-notes",
          notes_labels
        )
      ),
      file = file.path(output_dir, file_name)
    )
  }

  end_time <- Sys.time()

  toc_index <- build_overview_toc(
    active_file = "index.html",
    include_plot_summaries = FALSE,
    include_sunburst_summaries = TRUE,
    include_dashboard = FALSE,
    include_tb_summaries = FALSE,
    end_time = end_time
  )

  toc_dashboard <- build_overview_toc(
    active_file = "dashboard.html",
    include_plot_summaries = TRUE,
    include_sunburst_summaries = FALSE,
    include_dashboard = TRUE,
    include_tb_summaries = FALSE,
    end_time = end_time
  )

  toc_tables <- build_overview_toc(
    active_file = "tables.html",
    include_plot_summaries = TRUE,
    include_sunburst_summaries = FALSE,
    include_dashboard = FALSE,
    include_tb_summaries = TRUE,
    end_time = end_time
  )

  save_overview_html(toc_index, "index.html")
  save_overview_html(toc_dashboard, "dashboard.html")
  save_overview_html(toc_tables, "tables.html")

  util_write_renderinfo_js_json(
    output_dir,
    rep_id,
    start_time = start_time,
    end_time = end_time
  )

  invisible(NULL)
}

#' Create a Short Subtitle From a dq_report_by Call
#'
#' @param by_call a call to `dq_report_by()` (possibly unevaluated)
#'
#' @return character(1)
#' @noRd
util_by_header_from_args <- function(by_call) {
  stopifnot(!missing(by_call))

  bcl <- rlang::call_match(
    call = by_call,
    fn = dq_report_by
  )

  args <- rlang::call_args(bcl)

  deparse1_safe <- function(x) {
    if (is.null(x)) return(NULL)
    paste(deparse(x), collapse = "")
  }

  clean_expr <- function(x) {
    if (is.null(x)) return(NULL)
    gsub('^"|"$', "", x)
  }

  as_label <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (is.character(x)) {
      return(x)
    }

    if (is.symbol(x)) {
      return(as.character(x))
    }

    if (is.call(x) && identical(x[[1]], as.name("::")) && length(x) == 3) {
      return(as.character(x[[3]]))
    }

    clean_expr(deparse1_safe(x))
  }

  normalize_chr <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (is.character(x)) {
      return(x)
    }

    if (is.symbol(x)) {
      return(as.character(x))
    }

    if (is.call(x) && identical(x[[1]], as.name("c"))) {
      vals <- unlist(lapply(as.list(x)[-1], function(xx) {
        if (is.character(xx)) {
          xx
        } else if (is.symbol(xx)) {
          as.character(xx)
        } else if (is.call(xx) &&
                   identical(xx[[1]], as.name("::")) &&
                   length(xx) == 3) {
          as.character(xx[[3]])
        } else {
          clean_expr(deparse1_safe(xx))
        }
      }), use.names = FALSE)
      return(vals)
    }

    clean_expr(deparse1_safe(x))
  }

  collapse_labels <- function(x) {
    if (!length(x)) {
      return("")
    }
    if (length(x) == 1) {
      return(x)
    }
    if (length(x) == 2) {
      return(paste(x, collapse = " & "))
    }

    paste0(
      paste(x[-length(x)], collapse = ", "),
      ", & ",
      x[length(x)]
    )
  }

  strata <- if ("strata_column" %in% names(args)) {
    as_label(args$strata_column)
  } else {
    NULL
  }

  segment <- if ("segment_column" %in% names(args)) {
    if (is.null(args$segment_column)) NULL else as_label(args$segment_column)
  } else {
    NULL
  }

  dimensions <- if ("dimensions" %in% names(args)) {
    normalize_chr(args$dimensions)
  } else {
    NULL
  }

  default_dims <- c("des", "int", "com", "con")
  full_dims <- c("des", "int", "com", "con", "acc")
  optional_dims <- c("com", "con", "acc")

  if (is.null(dimensions)) {
    dimensions <- default_dims
  }

  dimensions <- unique(tolower(dimensions))

  if (identical(dimensions, "all")) {
    dimensions <- full_dims
  }

  dim_label <- function(x) {
    if (x %in% names(dims)) {
      unname(dims[[x]])
    } else {
      x
    }
  }

  parts <- character()

  if (!is.null(strata)) {
    parts <- c(parts, sprintf("Stratified by %s", strata))
  }

  if (!is.null(segment)) {
    parts <- c(
      parts,
      sprintf(
        "Split by Segments as Defined in %s in the Metadata",
        segment
      )
    )
  }

  present_opt <- intersect(optional_dims, dimensions)
  added_opt <- setdiff(present_opt, intersect(optional_dims, default_dims))
  missing_opt <- setdiff(optional_dims, present_opt)

  if (length(added_opt)) {
    parts <- c(
      parts,
      sprintf(
        "Including %s Checks",
        collapse_labels(vapply(added_opt, dim_label, character(1)))
      )
    )
  }

  if (length(missing_opt) && !identical(sort(missing_opt), "acc")) {
    parts <- c(
      parts,
      sprintf(
        "Without %s Checks",
        collapse_labels(vapply(missing_opt, dim_label, character(1)))
      )
    )
  }

  if (!length(parts)) {
    return("Data Quality Report Bundle")
  }

  paste("Data Quality Report Bundle", paste(parts, collapse = " and "))
}
