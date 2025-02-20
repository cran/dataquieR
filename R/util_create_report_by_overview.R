#' Create an overview of the reports created with `dq_report_by`
#'
#' @param output_dir [character] the directory in which all reports are searched
#'                                and the overview is saved
#' @param strata_column [character] name of a study variable to stratify the
#'                                    report by. It can be null
#' @param segment_column [character] name of a metadata attribute
#'                                    usable to split the report in
#'                                    sections of variables. It can be null
#' @param strata_column_label [character] the label of the variable used as
#'                                           strata_column
#' @param subgroup [character] optional, to define subgroups of cases
#' @param mod_label [list] `util_ensure_label()` info
#' @return an overview of all `dataquieR` reports created with `dq_report_by`
util_create_report_by_overview <- function(output_dir,
                                           strata_column,
                                           segment_column,
                                           strata_column_label,
                                           subgroup,
                                           mod_label) {
  # prepare the path of the output_dir with the / at the end
  out_dir <- output_dir
  if (!endsWith(out_dir, .Platform$file.sep)) {
    out_dir <- paste0(out_dir, "/")
  }

  # create a summary of summaries
  #import all the R objects containing a report summary and create a list
  sum_names <- list.files(path = output_dir, pattern = "^report_summary_")
  sum_names2 <- vapply(sum_names, function(x)
    paste0(out_dir, x), FUN.VALUE = character(1))
  list_summaries2 <- lapply(sum_names2, FUN = readRDS)

  # create a toc
  toc <- lapply(sum_names, function(x) {
    path_name <- paste0(out_dir, x)
    temp_file <- readRDS(path_name)
    this1 <- attr(temp_file, "this")
    sdn <- this1$sdn
    path_name <- file.path(output_dir, gsub("[^a-zA-Z0-9_\\.]", "",
                                            sprintf("report_%s", sdn)))
    location <-  gsub("[^a-zA-Z0-9_\\.]", "",
                      sprintf("report_%s", sdn))
    toc <- list()
    toc[[this1$stratum]] <- list()
    toc[[this1$stratum]][[this1$segment]] <- location
    return(toc)
  })

# create a vector containing all variables and their location in folder
  vars_reportlocation <- lapply(sum_names, function(x) {
    name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)
   # name_folder <- paste0(out_dir, "report_", name_folder)
    name_folder <- paste0("report_", name_folder)
    path_name <- paste0(out_dir, x)
    temp_file <- readRDS(path_name)
    this1 <- attr(temp_file, "this")
    used_data_file <- this1$used_data_file
    #leave only file name and extension, if the vector contains a path
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

    temp_res$md_names <- paste0(temp_res$VAR_NAMES, ".", temp_res$call_names)
    md_names <- temp_res$md_names
    md_names <- paste0(origin_name, "-", md_names)
    if (is.null(strata_column) || (!is.null(strata_column) &&
                                   length(list_summaries2)==1)) {
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

  #In case of creation of report by strata (more than one strata) the VAR_NAMES
  # changes so a conversion dataframe is needed to create the links to the folders
  if (!is.null(strata_column) && length(list_summaries2)>1) {
    # create a vector with the variable_names and the unique variable names
    # created when there is a strata selection
    vars_originalnames <- mapply(x = sum_names,
                                 SIMPLIFY = FALSE,
                                 FUN = function(x) {
      name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)
      # name_folder <- paste0(out_dir, "report_", name_folder)
      name_folder <- paste0("report_", name_folder)
      path_name <- paste0(out_dir, x)
      temp_file <- readRDS(path_name)
      this1 <- attr(temp_file, "this")
      used_data_file <- this1$used_data_file
      #leave only file name and extension, if the vector contains a path
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
                                 c(VAR_NAMES), drop = FALSE]
      temp_res <- temp_res[!duplicated(temp_res), , drop = FALSE]
      temp_res$LABEL <- NA
      if (this1$label_col != VAR_NAMES) {
        temp_varnames_colnames <-
          this1$meta_data[, colnames(this1$meta_data) %in%
                            c(VAR_NAMES, this1$label_col)]
        temp_res$LABEL <- util_map_labels(temp_res$VAR_NAMES,
                                          meta_data = temp_varnames_colnames,
                                          from = VAR_NAMES,
                                          to = this1$label_col)
      }
      temp_res$md_names <- NA
      md_names <- temp_res$VAR_NAMES
      md_names <- paste0(origin_name, "-", md_names)
      if (is.null(strata_column) || (!is.null(strata_column) &&
                                     length(list_summaries2)==1)) {
        origin_name <- paste0(origin_name, "-")
        md_names <- sub(origin_name, "", md_names, fixed = TRUE)
      }
      temp_res$md_names <- md_names

      temp_res$new_name_with_label <- paste0(temp_res$LABEL)
      new_name_with_label <- temp_res$new_name_with_label
      new_name_with_label <- paste0(origin_name, "-", new_name_with_label)
      if (is.null(strata_column) || (!is.null(strata_column) &&
                                     length(list_summaries2)==1)) {
        origin_name <- paste0(origin_name, "-")
        new_name_with_label <- sub(origin_name, "",
                                   new_name_with_label,
                                   fixed = TRUE)
      }
      temp_res$new_name_with_label <- new_name_with_label


      temp_res$name_matching_result2_Variables <- paste0(temp_res$LABEL)
      name_matching_result2_Variables <-
        temp_res$name_matching_result2_Variables
      name_matching_result2_Variables <-
        paste0(origin_name, " - ", name_matching_result2_Variables)
      if (is.null(strata_column) || (!is.null(strata_column) &&
                                     length(list_summaries2)==1)) {
        origin_name <- paste0(origin_name, "-")
        name_matching_result2_Variables <- sub(origin_name, "",
                                               name_matching_result2_Variables,
                                               fixed = TRUE)
      }
      temp_res$name_matching_result2_Variables <-
        name_matching_result2_Variables

      label_col_and_folder <-
        data.frame(new_names_with_varnames = temp_res$md_names,
                   new_names_with_label = temp_res$new_name_with_label,
                   result2_Variables_match =
                     temp_res$name_matching_result2_Variables,
                   var_names = temp_res$VAR_NAMES,
                   name_label = temp_res$LABEL)
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
                                        var_uniquenames = vars_originalnames) #this is null if there are no multiple strata
  Plot_TBSummaries <- plot(summary_all, dont_plot = TRUE)

  #Create an all_ids file
  all_ids_overall <- lapply(sum_names, function(x) {
    name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)
    path_all_ids_file <- paste0(out_dir, "report_", name_folder,
                               "/.report/anchor_list.RDS")
    temp_file <- readRDS(path_all_ids_file)
    temp_file <- temp_file[startsWith(temp_file, "VAR_")]
    temp_file <- paste0("report_", name_folder,
                        "/.report/", temp_file)
    temp_file <- list(temp_file)
    return(temp_file)
  })
  # create an overall anchor list with all created results, useful not to
  # create links to non-existing results
  all_ids_overall <- unlist(all_ids_overall)
  cat(sep = "",
      "window.all_ids = {\"all_ids_overall\": ",
      paste0("[",
             paste0('"', all_ids_overall, '"', collapse = ", "),
             "]"),
      "}",
      file = file.path(out_dir, "anchor_list.js")
  )

  #creates the html overview page that links all sub-reports created
  if (!is.null(strata_column) && !is.null(segment_column)) {
    # extract the names of the level of study data (e.g.,  "SEX_0_F")
    level_seg <- names(unlist(toc))
    get_level <- function(str) {
      x <- sub("\\..*", "", str)
      return(x)
    }
    sdlevel <- vapply(level_seg, get_level, FUN.VALUE = character(1))
    # extract the names of the segments for each strata (e.g., "PART_STUDY")
    get_segm <- function(str) {
      x <- sub(".*\\.", "", str)
      return(x)
    }
    segs <- vapply(level_seg, get_segm, FUN.VALUE = character(1))
    sdlevel <- substr(sdlevel, nchar(strata_column_label) + 2, 10000)
    title <- paste0("Report: ", segment_column, " = ", segs, ", ",
                    strata_column_label, " = ", sdlevel)
  } else if (is.null(strata_column) && !is.null(segment_column)) {
    level_seg <- names(unlist(toc))
    get_level <- function(str) {
      x <- sub("\\..*", "", str)
      return(x)
    }
    sdlevel <- vapply(level_seg, get_level, FUN.VALUE = character(1))
    # extract the names of the segments for each strata (e.g., "PART_STUDY")
    get_segm <- function(str) {
      x <- sub(".*\\.", "", str)
      return(x)
    }
    segs <- vapply(level_seg, get_segm, FUN.VALUE = character(1))
    title <- paste0("Report: ", segment_column, " = ", segs, "." , sdlevel)
  } else if (!is.null(strata_column) && is.null(segment_column)) {
    sdlevel <- names(unlist(toc))
    sdlevel <- substr(sdlevel, nchar(strata_column_label) + 2, 10000)
    title <- paste("Report: ", strata_column_label, " = ", sdlevel)
  } else if (is.null(strata_column) && is.null(segment_column) &&
             !is.null(subgroup)){
    # If there is at least a split for segment or strata, modify title
    if(is.character(title)) {
      title <- paste0(title, ". Subgroup: ", subgroup)
    } else { #if there is only subgroup present, create title
      title <- paste0("Report on subgroup: ", subgroup)
    }
  } else {
    title <- paste0("Report on all variables and observations")
  }


  # create the link for the created reports
  # href <- substr(paste0(unlist(toc), "/index.html"),
  #               nchar(output_dir) + 2, 10000)
  if (length(toc) == 0) {
    href <- character(0)
  } else {
    href <- paste0(unlist(toc), "/index.html")
  }
  util_ensure_suggested("htmltools", "generate reports TOC")

  #create the full table of contents, the actual html file with or without a legend
  if (exists("..INFO_SD_NAME_FOR_REPORT", .dataframe_environment())) {
    toc <-
      htmltools::tagList(
        htmltools::div(class = "navbar"),
        htmltools::tags$script(type = "text/javascript",
                               "window.dq_report_by_overview = true"),
        htmltools::tags$script(type = "text/javascript",
                               src = "anchor_list.js"),
        htmltools::tags$h1(htmltools::HTML("List of created reports")),
        htmltools::tags$ul(htmltools::HTML(
          vapply(
            lapply(
              mapply(
                SIMPLIFY = FALSE, FUN = htmltools::a,
                href = href, title), htmltools::tags$li),
            as.character, FUN.VALUE = character(1)
          ))),
        htmltools::tags$h1(htmltools::HTML("Legend")),
        htmltools::tags$ul(htmltools::HTML(
          vapply(
            lapply(
              lapply(
                names(as.list(prep_get_data_frame("..INFO_SD_NAME_FOR_REPORT")
                  )),
                FUN = function(x) {
                  text <- paste0(x, ": ",
                                 as.list(
                                   prep_get_data_frame(
                                     "..INFO_SD_NAME_FOR_REPORT"))[[x]])
                }
            ), htmltools::tags$li), as.character, FUN.VALUE = character(1)
          )),
        htmltools::tags$h1(htmltools::HTML("Overall summary")),
        #      htmltools::tags$h2(htmltools::HTML("Names of summaries")),
        #      htmltools::tags$ul(htmltools::HTML(
        #        vapply(lapply(sum_names, htmltools::tags$li), as.character,
        #                      FUN.VALUE = character(1)
        #        ))),

        Plot_TBSummaries,
        TBSummaries

        ))
  } else {
    toc <-
      htmltools::tagList(
        htmltools::div(class = "navbar"),
        htmltools::tags$script(type = "text/javascript",
                               "window.dq_report_by_overview = true"),
        htmltools::tags$script(type = "text/javascript",
                               src = "anchor_list.js"),
        htmltools::tags$h1(htmltools::HTML("List of created reports")),
        htmltools::tags$ul(htmltools::HTML(
          vapply(
            lapply(
              mapply(
                SIMPLIFY = FALSE, FUN = htmltools::a, href = href, title),
              htmltools::tags$li), as.character, FUN.VALUE = character(1) )
        )),
        htmltools::tags$h1(htmltools::HTML("Overall summary")),
        #     htmltools::tags$h2(htmltools::HTML("Names of summaries")),
        #     htmltools::tags$ul(htmltools::HTML(
        #        vapply(lapply(sum_names, htmltools::tags$li), as.character,
        #                      FUN.VALUE = character(1)
        #        ))),
        Plot_TBSummaries,
        TBSummaries
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
  htmltools::save_html(htmltools::tagList(toc,
                                          notes_labels),
                       file = file.path(output_dir, "index.html"))
}



