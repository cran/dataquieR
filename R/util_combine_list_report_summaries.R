#' Create a data frame containing all the results from summaries of reports
#'
#' @param to_combine [vector] a list containing the summaries of reports
#'                            obtained with `summary(report)`
#' @param type [character] if `type` is `unique_vars` it means that the variable
#'                        names are unique and there is not need to add a prefix
#'                        to the variables and labels to specify the
#'                        report of origin.
#'                        If `type` is `repeated_vars` a prefix will be used to
#'                        specify the report of origin of each variable
#'
#' @return a summary of summaries of `dataquieR` reports
#'
util_combine_list_report_summaries <- function(to_combine,
                                               type = c("unique_vars",
                                                        "repeated_vars")) {

  # check if all the elements in to_combine are of class "dataquieR_summary"
  for (x in seq_along(to_combine)) {
    util_stop_if_not(inherits(to_combine[[x]], "dataquieR_summary"))
  }

  # Recursive function
  if (length(to_combine) == 0) {
    # create an empty dataframe
    result2 <- data.frame()
    # use var_names as labels
    label_col <- VAR_NAMES
    # create a paragraph stating that there is no result
    res <- "Empty summary"
    class(res) <- "dataquieR_summary"

    this <- new.env(parent = emptyenv())

    for (prop in c("rowmaxes", # maximum category for each row in the summary table
                   # and all the ellipsis_arguments from
                   # util_get_html_cell_for_result, above:
                   "labels_of_var_names_in_report",
                   "alias_names",
                   "stopped_functions",
                   "colors",
                   "labels",
                   "order_of",
                   "filter_of",
                   "ordered_call_names",
                   "ordered_var_names"
    )) {
      this[[prop]] <- NULL
    }
    this[["result"]] <- data.frame()
    this[["title"]] <- ""
    this[["subtitle"]] <- ""
    this[["meta_data"]] <- NULL
    this[["label_col"]] <- VAR_NAMES
    this[["rownames_of_report"]] <- character(0)
    this[["colnames_of_report"]] <- character(0)

    return(util_attach_attr(res,
                            repsum_wide = util_attach_attr(result2,
                                                           label_col = label_col),
                            rule_digest = rlang::hash(list(util_get_rule_sets(),
                                                           util_get_ruleset_formats())),
                            this = this))
  } else if (length(to_combine) == 1) {
    return(to_combine[[1]])
  } else if (length(to_combine) == 2) {
    this1 <- attr(to_combine[[1]], "this")
    this2 <- attr(to_combine[[2]], "this")

    ## Import results
    result_1 <- this1$result
    if (!prod(dim(result_1)) ||
        all(startsWith(as.character(result_1$indicator_metric), "CAT_") |
            startsWith(as.character(result_1$indicator_metric), "MSG_"))) {
      # create an empty data frame
      result_1 <- data.frame()
    }
    result_2 <- this2$result
    if (!prod(dim(result_2)) ||
        all(startsWith(as.character(result_1$indicator_metric), "CAT_") |
            startsWith(as.character(result_1$indicator_metric), "MSG_"))) {
      # create an empty data frame
      result_2 <- data.frame()
    }

    ## import meta_data
    md1 <- this1$meta_data
    md2 <- this2$meta_data

    ## check labels_of_var_names_in_report
    if (length(this1$labels_of_var_names_in_report) == 0) {  #TODO: is this possible?
      this1$labels_of_var_names_in_report <- vector("character")
    }
    if (length(this2$labels_of_var_names_in_report) == 0) {
      this2$labels_of_var_names_in_report <- vector("character")
    }


    #In case the variables are unique the type unique_vars is used
    if (type == "unique_vars") {
      #add a column origin in both result data frames (works also in case empty)
      result_1$origin <- paste0(this1$title)
      result_2$origin <- paste0(this2$title)
      result <- dplyr::bind_rows(result_1, result_2)
      rm(result_1, result_2)

      ## labels_of_var_names_in_report
      labels_of_var_names_in_report <- c(this1$labels_of_var_names_in_report,
                                         this2$labels_of_var_names_in_report)
      labels_of_var_names_in_report <-  #Remove duplicates
        labels_of_var_names_in_report[match(unique(
          names(labels_of_var_names_in_report)),
          names(labels_of_var_names_in_report))]
      #TODO: meta_data can not be missing right??
      ## meta_data
      if (!"new_label" %in% names(md1)) { #in case we are merging the first 2 summaries
        md1$origin <- paste0(this1$title)
        #new column for label_col with the origin followed by the original label_col
        lab1 <- md1[this1$label_col]
        names(lab1) <- "new_label"
        md1 <- cbind(md1, lab1)
      }
      md2$origin <- paste0(this2$title)
      #new column for label_col with the origin followed by the label_col
      lab1 <- md2[this2$label_col]
      names(lab1) <- "new_label"
      md2 <- cbind(md2, lab1)
      rm(lab1)
      #merge the metadata
      meta_data <- merge(md1, md2, all = TRUE) #FIXME: remove duplicates also if summaries came from 2 different metadata files

      #In case of presence of duplicated rows (excluding origin and VARIABLE_ROLE)
      meta_data <- meta_data[!duplicated(meta_data[,
                                                   setdiff(colnames(meta_data),
                                                           c("origin",
                                                             VARIABLE_ROLE)),
                                                   drop = FALSE]), ,
                             drop = FALSE]

      attr(meta_data, "version") <- 2
      attr(meta_data, "normalized") <- TRUE


      ## ordered_var_names
      ordered_var_names <- c(this1$ordered_var_names, this2$ordered_var_names)
      ordered_var_names <-
        ordered_var_names[match(unique(names(ordered_var_names)),
                                names(ordered_var_names))]

      ## rowmaxes - tibble
      rowmaxes <- dplyr::bind_rows(this1$rowmaxes, this2$rowmaxes)

      ## rownames_of_report
      rownames_of_report <- unique(c(this1$rownames_of_report,
                                     this2$rownames_of_report))

    } else if (type == "repeated_vars") { #in case vars may be not unique
      #Report name 1
      if (!"..Origin" %in% names(result_1)) {
        used_data_file <- this1$used_data_file
        #leave only file name and extension, if the vector contains a path
        if (grepl(.Platform$file.sep, used_data_file, fixed = TRUE)) {
          used_data_file <- gsub(".+\\/(.+\\..+$)", "\\1", used_data_file)
        }
        stratum <- this1$stratum
        segm <- this1$segment
        if (segm == "all_variables") {
          #in case there is only strata_column and not segment_column
          origin_name1 <- paste0(used_data_file, "-", stratum)
        } else {
          origin_name1 <- paste0(used_data_file, "-", stratum, "-", segm)
        }
        rm(used_data_file, stratum, segm)
      } else {
        origin_name1 <- unique(this1$result$..Origin)
      }

      #Report name 2
      used_data_file <- this2$used_data_file
      #leave only file name and extension, if the vector contains a path
      if (grepl(.Platform$file.sep, used_data_file, fixed = TRUE)) {
        used_data_file <- gsub(".+\\/(.+\\..+$)", "\\1", used_data_file)
      }
      stratum <- this2$stratum
      segm <- this2$segment
      if (segm == "all_variables") {
        origin_name2 <- paste0(used_data_file, "-", stratum)
      } else {
        origin_name2 <- paste0(used_data_file, "-", stratum, "-", segm)
      }
      rm(used_data_file, stratum)
      gc()

      # check that names are not the same
      orig1 <- as.vector(origin_name1)
      orig2 <- as.vector(origin_name2)
      if (any(orig1 %in% orig2)) {
        util_error("Two reports can not have the same title %s",
                   dQuote(origin_name2))
      }

      #Create a data frame to map var_names and labels to new with prefix
      if (!"..Origin" %in% names(result_1)) {
        df_map1 <- this1$meta_data
        df_map1 <- df_map1[, names(df_map1) %in%
                             c("VAR_NAMES", this1$label_col), drop = FALSE]

        df_map1$new_VAR_NAMES <- paste0(origin_name1, "-",
                                        df_map1$VAR_NAMES)
        df_map1$new_label_col <- paste0(origin_name1, "-",
                                        df_map1[, names(df_map1) %in%
                                                  this1$label_col])
      }

      df_map2 <- this2$meta_data
      df_map2 <- df_map2[, names(df_map2) %in% c("VAR_NAMES", this2$label_col), drop = FALSE]
      df_map2$new_VAR_NAMES <- paste0(origin_name2, "-", df_map2$VAR_NAMES)
      df_map2$new_label_col <- paste0(origin_name2, "-",
                                      df_map2[, names(df_map2) %in%
                                                this2$label_col])

      #add a column origin in both result data frames (works also in case empty)
      if (!"..Origin" %in% names(result_1)) {
        #in case we are merging the first 2 summaries
        result_1$..Origin <- origin_name1
        result_1$Original_var_name <- result_1$VAR_NAMES
        result_1 <- dplyr::mutate(result_1,
                                  VAR_NAMES = paste0(result_1$..Origin,
                                                     "-", result_1$VAR_NAMES))
      }
      #the second summary is always a new one in all loops
      result_2$..Origin <- origin_name2
      result_2$Original_var_name <- result_2$VAR_NAMES
      result_2 <- dplyr::mutate(result_2,
                                VAR_NAMES = paste0(result_2$..Origin,
                                                   "-", result_2$VAR_NAMES))
      result <- rbind(result_1, result_2)

      ## meta_data
      if (!"..Origin" %in% names(md1)) {
        #in case we are merging the first 2 summaries
        md1$..Origin <- origin_name1
        # long label is used for the list of variables in the final table, so if
        # present I add the prefix of ..Origin
        if ("LONG_LABEL" %in% names(md1)) {
          md1 <- dplyr::mutate(md1, LONG_LABEL = paste(md1$..Origin, LONG_LABEL))
        }
      }
      md2$..Origin <- origin_name2
      #long label is used for the list of variables in the final table, so if
      # present I add the prefix of origin
      if ("LONG_LABEL" %in% names(md2)) {
        md2 <- dplyr::mutate(md2, LONG_LABEL = paste(md2$..Origin, LONG_LABEL))
      }

      if (!"new_label" %in% names(md1)) {
        #in case we are merging the first 2 summaries
        #new column for label_col with the ..Origin followed by the label_col
        lab1 <- md1[this1$label_col]
        names(lab1) <- "new_label"
        md1 <- cbind(md1, lab1)
        md1 <- dplyr::mutate(md1, new_label = paste(md1$..Origin, "-",
                                                    md1$new_label))

        ## rowmaxes - replace VAR_NAMES with VAR_NAMES with prefix Report title
        rowmaxes1 <- this1$rowmaxes
        temp1 <- util_map_labels(this1$rowmaxes$VAR_NAMES,
                                 meta_data = df_map1,
                                 to = "new_VAR_NAMES",
                                 from = VAR_NAMES)
        names(temp1) <- NULL
        rowmaxes1$VAR_NAMES <- temp1

        ## rownames_of_report
        rownames_of_report1 <- util_map_labels(this1$rownames_of_report,
                                               meta_data = df_map1,
                                               to = "new_label_col",
                                               from = this1$label_col)

        # add the report title in the VAR_NAMES and LABELS of the
        ## ordered_var_names
        labs_new <- util_map_labels(this1$ordered_var_names,
                                    meta_data = df_map1,
                                    to = "new_label_col",
                                    from = VAR_NAMES)

        # replace var_names with new_var_names
        new_var_names1 <- util_map_labels(this1$ordered_var_names,
                                          meta_data = df_map1,
                                          to = "new_VAR_NAMES",
                                          from = "VAR_NAMES")
        ord1 <- setNames(object = new_var_names1, nm =  labs_new)

        ## labels_of_var_names_in_report
        # replace labels with new_labels
        labs_new <- util_map_labels(this1$labels_of_var_names_in_report,
                                    meta_data = df_map1,
                                    to = "new_label_col",
                                    from = this1$label_col)

        # replace var_names with new_var_names
        new_var_names1 <- util_map_labels(
          names(this1$labels_of_var_names_in_report),
          meta_data = df_map1,
          to = "new_VAR_NAMES",
          from = "VAR_NAMES")
        tb_for_names1 <- setNames(object = labs_new, nm =  new_var_names1)

        # now modify VAR_NAMES also in md1
        md1$Original_var_name <- md1$VAR_NAMES
        md1 <- dplyr::mutate(md1,
                             VAR_NAMES = paste0(md1$..Origin,
                                                "-", md1$VAR_NAMES))
        rm(lab1, labs_new, new_var_names1)
      }

      #new column for label_col with the origin followed by the label_col
      lab1 <- md2[this2$label_col]
      names(lab1) <- "new_label"
      md2 <- cbind(md2, lab1)
      md2 <- dplyr::mutate(md2, new_label = paste(md2$..Origin, "-",
                                                  md2$new_label))

      ## ordered_var_names
      # add the report title in the VAR_NAMES and LABELS of the ordered_var_names
      labs_new <- util_map_labels(this2$ordered_var_names,
                                  meta_data = df_map2,
                                  to = "new_VAR_NAMES",
                                  from = VAR_NAMES)

      # replace var_names with new_var_names
      new_var_names2 <- util_map_labels(this2$ordered_var_names,
                                        meta_data = df_map2,
                                        to = "new_VAR_NAMES",
                                        from = "VAR_NAMES")
      ord2 <- setNames(object = new_var_names2, nm =  labs_new)

      if (!exists("ord1")) {
        ord1 <- this1$ordered_var_names
      }
      ordered_var_names <- c(ord1, ord2)
      ordered_var_names <-
        ordered_var_names[match(unique(names(ordered_var_names)),
                                names(ordered_var_names))]
      rm(lab1, labs_new, new_var_names2, ord1, ord2)

      ## labels_of_var_names_in_report
      # replace labels with new_labels
      labs_new <- util_map_labels(this2$labels_of_var_names_in_report,
                                  meta_data = df_map2,
                                  to = "new_label_col",
                                  from = this2$label_col)

      # replace var_names with new_var_names
      new_var_names2 <- util_map_labels(
        names(this2$labels_of_var_names_in_report),
        meta_data = df_map2,
        to = "new_VAR_NAMES",
        from = "VAR_NAMES")
      tb_for_names2 <- setNames(object = labs_new, nm =  new_var_names2)
      if (!exists("tb_for_names1")) {
        tb_for_names1 <- this1$labels_of_var_names_in_report
      }

      #merge the two vectors
      labels_of_var_names_in_report <- c(tb_for_names1, tb_for_names2)
      #Remove duplicates
      labels_of_var_names_in_report <-
        labels_of_var_names_in_report[match(
          unique(names(labels_of_var_names_in_report)),
          names(labels_of_var_names_in_report))]
      rm(labs_new, new_var_names2, tb_for_names1, tb_for_names2)

      #rowmaxes - tibble
      if (!exists("rowmaxes1")) {
        #From the second round on the name contains already the prefix
        rowmaxes1 <- this1$rowmaxes
      }
      rowmaxes2 <- this2$rowmaxes
      temp_name <- util_map_labels(this2$rowmaxes$VAR_NAMES,
                                   meta_data = df_map2,
                                   to = "new_VAR_NAMES",
                                   from = VAR_NAMES)
      names(temp_name) <- NULL
      rowmaxes2$VAR_NAMES <- temp_name
      rm(temp_name)
      rowmaxes <- rbind(rowmaxes1, rowmaxes2)

      #rownames_of_report
      if (!exists("rownames_of_report1")) {
        #From the second round on the name contains already the prefix
        rownames_of_report1 <- this1$rownames_of_report
      }
      rownames_of_report2 <- util_map_labels(this2$rownames_of_report,
                                             meta_data = md2,
                                             to = "new_label",
                                             from = this2$label_col)
      rownames_of_report <- unique(c(rownames_of_report1, rownames_of_report2))

      # now modify VAR_NAMES also in md2
      md2$Original_var_name <- md2$VAR_NAMES
      md2 <- dplyr::mutate(md2, VAR_NAMES = paste0(md2$..Origin, "-",
                                                   md2$VAR_NAMES))
      #merge the metadata
      meta_data <- merge(md1, md2, all = TRUE)
      attr(meta_data, "version") <- 2
      attr(meta_data, "normalized") <- TRUE
      rm(md1, md2)
    }

    ## alias_names
    alias_names <- c(this1$alias_names, this2$alias_names)
    alias_names <- alias_names[match(unique(names(alias_names)),
                                     names(alias_names))]

    ## ordered_call_names
    ordered_call_names <- unique(c(this1$ordered_call_names,
                                   this2$ordered_call_names))

    ## colnames_of_report
    colnames_of_report <- unique(c(this1$colnames_of_report,
                                   this2$colnames_of_report))

    #stopped_functions
    stopped_functions <- c(this1$stopped_functions, this2$stopped_functions)
    stopped_functions <-
      stopped_functions[match(unique(names(stopped_functions)),
                              names(stopped_functions))]

    #subtitle
    comp1 <- all.equal(this1$subtitle, this2$subtitle)
    if (length(comp1) == 1 &&
        comp1 == TRUE) {
      #TODO: check if it did not become too long
      subtitle <- this1$subtitle
    } else {
      subtitle <- paste0(this1$subtitle, " - ", this2$subtitle)
      attr(subtitle, "default") <- TRUE
    }

    #title
    comp1 <- all.equal(this1$title, this2$title)
    if (length(comp1) == 1 &&
        comp1 == TRUE) {
      #TODO: check if it did not become too long
      title <- this1$title
    } else {
      title <- paste0(this1$title, " - ", this2$title)
      attr(title, "default") <- TRUE
    }
    rm(comp1)

    #label_col
    label_col <- "new_label"
    attr(label_col, "var_att_required") <- "recommended"

    #filter_of
    ## Combine lists by element names. It works also if all list are identical
    l_all <- list(this1$filter_of, this2$filter_of)
    keys <- unique(unlist(lapply(l_all, names)))
    list_all <- as.data.frame(do.call(mapply, c(FUN = c,
                                                lapply(l_all, `[`, keys))))
    filter_of <- lapply(
      names(list_all),
      FUN = function(x) {
        paste0(unique(list_all[x]), collapse = "-")
      }
    )
    filter_of <- setNames(filter_of, names(list_all))

    ## labels
    #labels are exactly the same as filter_of
    labels <- filter_of

    rm(l_all, keys, list_all)

    #colors
    colors <- this1$colors
    comp1 <- all.equal(this1$colors, this2$colors)
    if (length(comp1) > 1 || comp1 != TRUE) {
      util_warning("Colors of first report in the list will be used")
    }

    #order_of is always the same 5 categories and NAs
    order_of <- this1$order_of

    summary_of_summaries <- NA
    class(summary_of_summaries) <- c("dataquieR_summary")

    this <- new.env(parent = emptyenv())
    for (prop in c(
      "result",
      "rowmaxes",
      "labels_of_var_names_in_report",
      "alias_names",
      "stopped_functions",
      "colors",
      "labels",
      "order_of",
      "filter_of",
      "ordered_call_names",
      "ordered_var_names",
      "rownames_of_report"
    )) {
      this[[prop]] <- get(prop)
    }

    # needed for the summary downlaod button
    this[["title"]] <- title
    this[["subtitle"]] <- subtitle
    # needed to pretty print
    this[["meta_data"]] <- meta_data
    # "label_col", # needed to pretty print
    this[["label_col"]] <- label_col
    attr(summary_of_summaries, "this") <- this

    return(summary_of_summaries)
  } else {
    hd <- head(to_combine, 2)
    tl <- tail(to_combine, -2)
    hd_repl <- util_combine_list_report_summaries(hd, type = type)
    new_to_combine <- c(list(hd_repl), tl)
    res1 <- util_combine_list_report_summaries(new_to_combine, type = type)
    return(res1)
  }

}
