#' Render a table summarizing dataquieR results
#'
#' @param x a report summary (`summary(r)`)
#' @param grouped_by define the columns of the resulting matrix. It can be either
#'                  "call_names", one column per function, or "indicator_metric",
#'                  one column per indicator or both
#'                  `c("call_names", "indicator_metric")`.
#'                  The last combination is the default
#' @param folder_of_report a named vector with the location of variable and call_names
#' @param var_uniquenames a data frame with the original variable names and the
#'                        unique names in case of reports created with dq_report_by
#'                        containing the same variable in several reports
#'                        (e.g., creation of reports by sex)
#' @inheritParams util_filter_repsum
#' @return something, `htmltools` can render
#'
#' @noRd
#'
util_render_table_dataquieR_summary <- function(x,
                                                grouped_by =
                                                  c("call_names",
                                                    "indicator_metric"),
                                                folder_of_report = NULL,
                                                var_uniquenames = NULL,
                                                vars_to_include = c("study")) { # FIXME: -> util_*, remove other table printer functions, make prep_extract_summary util_* and, maybe, remove ggplot2 alternative pre for pie charts
  # make check happy:
  ordered_call_names <- NULL
  ordered_var_names <- NULL
  html <- NULL

  #order grouped_by if more than one is present
  if (length(grouped_by) > 1) {
    grouped_by <- sort(grouped_by)
  }

  # start
  util_stop_if_not(inherits(x, "dataquieR_summary"))
  # this represent the current object
  this <- attr(x, "this")
  # define an environment to be attached
  withr::with_environment(this, {
    for (n in names(this)) {
      assign(n, get(n, envir = this), envir = environment())
    }
    result <- util_filter_repsum(result, vars_to_include, meta_data,
                                 rownames_of_report,
                                 label_col)
    rownames_of_report <- attr(result, "rownames_of_report")
    labels <- this$labels #obviously not attached by with_environment
    # extract the colors from the grading_format file
    gg_colors <- util_get_colors()
    # rename the categories to have "Ok" or "Critical" instead of cat1 or cat5
    names(gg_colors) <- util_get_labels_grading_class()[names(gg_colors)]

    # For robustness: in case none of the assessments work, create an empty result
    # if result is empty or contains only errors
    if (!prod(dim(result)) || all(
      startsWith(as.character(result$indicator_metric), "CAT_") |
      startsWith(as.character(result$indicator_metric), "MSG_"))) {
      # create a paragraph stating that there is no result
      res <- (htmltools::browsable(htmltools::tagList(htmltools::p(
        "Empty summary"))))
      # create an empty dataframe
      result2 <- data.frame()
      # use var_names as labels
      label_col <- VAR_NAMES
    } else {  # in the case of actual metrics and not only error messages
      #===== Table and html text preparation ===
      result_full <- result

      #create a named vector with names = call_names and values = function_name
      function_name_mapping <- setNames(result_full$function_name,
                                        nm = result_full$call_names)

      # create the function name that corresponds to the actual call_names,
      # e.g., with con_hard_limits instead of con_limit_deviations
      result_full$function_name2 <-
        apply(result_full[, "call_names",
                          drop = FALSE],
              1, FUN = function(x) {
                util_map_by_largest_prefix(x,
                                           haystack = util_all_ind_functions())
              })

      # add a column with suffixes
      result_full <-
        dplyr::mutate(result_full,
                      suffixes = ifelse(
                        startsWith(result_full[["call_names"]],
                                   result_full[["function_name2"]]),
                        substr(result_full[["call_names"]],
                               nchar(result_full[["function_name2"]]) +
                                 1 + 1,
                               nchar(result_full[["call_names"]])),
                        # name + "_" (first +1), start is the next character (second +1)
                        result_full[["call_names"]]
                      ))

      # create a new column with indicator_metric and suffixes
      # if present (excluding MSG and cAT indicators)
      result_full <-
        dplyr::mutate(result_full,
                      indicator_metric_suff =
                        ifelse(startsWith(as.character(result_full$indicator_metric),
                                          "MSG_") |
                                 startsWith(as.character(result_full$indicator_metric),
                                            "CAT_"),
                               result_full[["indicator_metric"]],
                               ifelse(!util_empty(result_full[["suffixes"]]),
                                      paste0(result_full[["indicator_metric"]],
                                             "_",
                                             result_full[["suffixes"]]),
                                      result_full[["indicator_metric"]])))

      # Create 3 different objects containing errors, category of errors, and
      # the actual results with the indicator metrics

      # Start selecting everything that starts with MSG (Error message)
      result_err_msg <-
        result_full[startsWith(as.character(result_full$indicator_metric),
                               "MSG_"),
                    c(VAR_NAMES, "call_names",
                      "indicator_metric_suff",
                      "class"),
                    drop = FALSE]
      # modify the class to be "err_msg"
      result_err_msg <- dplyr::rename(result_err_msg, err_msg = "class")

      # Select everything starting with CAT (Error category)
      result_err_cat <-
        result_full[startsWith(as.character(result_full$indicator_metric),
                               "CAT_"),
                    c(VAR_NAMES, "call_names",
                      "indicator_metric_suff",
                      "class"),
                    drop = FALSE]
      # Rename the class for CAT errors as "err_cat"
      result_err_cat <- dplyr::rename(result_err_cat, err_cat = "class")

      # Remove all error messages (CAT and MSG) and obtain an object with actual
      # results (metrics) only
      result_full <-
        result_full[!startsWith(as.character(result_full$indicator_metric_suff),
                                "CAT_"), , drop = FALSE]
      result_full <-
        result_full[!startsWith(as.character(result_full$indicator_metric_suff),
                                "MSG_"), , drop = FALSE]
      #=====  Add or modify columns in the 3 objects before merging them ======

      #===== prepare the object with the results =======

      # Add a column with the numeric value of class e.g., 1 to 5
      result_full$class_num <- result_full$class #save it first as numeric

      # Convert column "class" to factor (e.g., Critical, Ok, NA)
      class_categ <- util_get_labels_grading_class()
      result_full$class <- factor(result_full$class,
                                  # take the levels of the grading and the labels
                                  # from the grading_ruleset
                                  levels = names(class_categ),
                                  labels = class_categ,
                                  ordered = TRUE)
      rm(class_categ)

      #===== prepare the object with the error messages =======

      # Remove empty rows and leave only rows with actual error messages
      result_err_msg <- result_err_msg[!util_empty(result_err_msg$err_msg), ,
                                       drop = FALSE]

      #remove duplicated rows in case present
      result_err_msg<- dplyr::distinct(result_err_msg)

      # in case there are duplicated rows due to the message
      # "No results computed"
      # separate duplicates in a group and then remove the
      # "No results computed" duplicates before to add them back
      values_with_dupl <- subset(
        result_err_msg,
        stats::ave(seq_len(nrow(result_err_msg)),
                   interaction(result_err_msg[, c("indicator_metric_suff",
                                                  "VAR_NAMES", "call_names")]),
                   FUN = length
        ) > 1
      )

      result_err_msg_without_duplicate <-
        dplyr::anti_join(result_err_msg,
                         values_with_dupl,
                         by = c("indicator_metric_suff",
                                "VAR_NAMES",
                                "call_names",
                                "err_msg"))
      values_with_dupl <-
        values_with_dupl[order(values_with_dupl[,"indicator_metric_suff"],
                               values_with_dupl[,"VAR_NAMES"],
                               values_with_dupl[,"call_names"],
                               values_with_dupl[, "err_msg"]) ,]
      values_with_dupl <- values_with_dupl[!grepl("No results computed",
                                                  values_with_dupl$err_msg),]
      result_err_msg <- rbind(result_err_msg_without_duplicate,
                              values_with_dupl)

      # from long to wide format
      # result_err_msg <- tidyr::spread(result_err_msg, indicator_metric, err_msg)
      result_err_msg <- stats::reshape(result_err_msg,
                                       idvar = c("VAR_NAMES", "call_names"),
                                       timevar = "indicator_metric_suff",
                                       direction = "wide")

      names(result_err_msg)[startsWith(names(result_err_msg), "err_msg.")] <-
        substr(names(result_err_msg)[startsWith(names(result_err_msg),
                                                "err_msg.")],
               nchar("err_msg.") + 1,
               nchar(names(result_err_msg)[startsWith(names(result_err_msg),
                                                      "err_msg.")]))

      # Robustness: in case of empty cells replace them with NAs
      result_err_msg[util_empty(result_err_msg)] <- NA

      # add tag <li> to each error message (if the column exists in the report)
      if("MSG_anamat" %in% colnames(result_err_msg)) {
        result_err_msg <- dplyr::mutate(result_err_msg,
                                        MSG_anamat =
                                          ifelse(!util_empty(get("MSG_anamat")),
                                                 paste0("<li>",
                                                        get("MSG_anamat"),
                                                        "</li>"),
                                                 NA))
      }

      if("MSG_applicability" %in% colnames(result_err_msg)) {
        result_err_msg <-
          dplyr::mutate(result_err_msg,
                        MSG_applicability =
                          ifelse(!util_empty(get("MSG_applicability")),
                                 paste0("<li>",
                                        get("MSG_applicability"),
                                        "</li>"), NA))
      }

      if("MSG_error" %in% colnames(result_err_msg)) {
        result_err_msg <- dplyr::mutate(result_err_msg,
                                        MSG_error =
                                          ifelse(!util_empty(get("MSG_error")),
                                                 paste0("<li>",
                                                        get("MSG_error"),
                                                        "</li>"), NA))
      }

      # create a column with all error messages (if there are error messages)
      # FIXED: Here, error messages are copied to all variables.
      # We need to group by VAR_NAMES and call_names
      ky_cols <- c("VAR_NAMES", "call_names")
      dt_cols <- setdiff(colnames(result_err_msg), ky_cols)
      result_err_msg <- tapply(simplify = FALSE,
                               result_err_msg, as.list(result_err_msg[, ky_cols]),
                               function(x) {
                                 res <- unique(x[, ky_cols, FALSE])
                                 if (nrow(x) == 0) {
                                   return("")
                                 }
                                 paste(unique(
                                   na.omit(unlist(x[, dt_cols], recursive = TRUE))),
                                   collapse = "\n")
                               })
      result_err_msg <- array2DF(result_err_msg,
                                 responseName = "All_err_msg",
                                 simplify = FALSE,
                                 allowLong = TRUE)

      #===== prepare the object with the categories of errors =======

      # Remove empty rows and leave only rows with actual error messages
      result_err_cat <- result_err_cat[!util_empty(result_err_cat$err_cat), ,
                                       drop = FALSE]
      #remove duplicated rows in case present
      result_err_cat <- dplyr::distinct(result_err_cat)

      # in case there are duplicated rows separate duplicates in
      # a group and then order by decreasing values of err_cat
      # then keep only worst (higher values) of err_cat
      values_with_dupl <- subset(
        result_err_cat,
        stats::ave(seq_len(nrow(result_err_cat)),
                   interaction(result_err_cat[, c("indicator_metric_suff",
                                                  "VAR_NAMES", "call_names")]),
                   FUN = length) > 1)

      result_err_cat_without_duplicate <-
        dplyr::anti_join(result_err_cat,
                         values_with_dupl,
                         by = c("indicator_metric_suff",
                                "VAR_NAMES",
                                "call_names",
                                "err_cat"))
      values_with_dupl$err_cat <- as.numeric(values_with_dupl$err_cat)
      # order by decreasing values of err_cat
      values_with_dupl <-
        values_with_dupl[order(values_with_dupl[,"indicator_metric_suff"],
                               values_with_dupl[,"VAR_NAMES"],
                               values_with_dupl[,"call_names"],
                               -values_with_dupl[, "err_cat"]) ,]
      values_with_dupl <-    #keep first row of duplicated values
        values_with_dupl[!duplicated(values_with_dupl[,
                                                      c("indicator_metric_suff",
                                                        "VAR_NAMES",
                                                        "call_names")]),]
      result_err_cat <- rbind(result_err_cat_without_duplicate, values_with_dupl)

      #from long to wide format
      #result_err_cat <- tidyr::spread(result_err_cat, indicator_metric, err_cat)
      result_err_cat <- stats::reshape(result_err_cat,
                                       idvar = c("VAR_NAMES", "call_names"),
                                       timevar = "indicator_metric_suff",
                                       direction = "wide")

      names(result_err_cat)[startsWith(names(result_err_cat), "err_cat.")] <-
        substr(names(result_err_cat)[startsWith(names(result_err_cat),
                                                "err_cat.")],
               nchar("err_cat.") + 1,
               nchar(names(result_err_cat)[startsWith(names(result_err_cat),
                                                      "err_cat.")]))

      # Add column other_error_stop5 - logic T/F; if T unexpected error has occurred
      result_err_cat$other_error_stop5 <- result_err_cat$CAT_error == 5

      # Add column is_indicator - logic T/F; if F is not an indicator
      result_err_cat$is_indicator <-
        result_err_cat$CAT_indicator_or_descriptor == 1

      #===== Merge the 3 object in one data frame =======

      # Merge results and errors in one object (errors become 4 extra columns)
      if (!VAR_NAMES %in% colnames(result_err_msg))
        result_err_msg[[VAR_NAMES]] <- rep(NA_character_, nrow(result_err_msg))
      result_full <- merge(
        result_full,
        result_err_msg,
        by = c(VAR_NAMES, "call_names"), all = TRUE)

      if (!VAR_NAMES %in% colnames(result_err_cat))
        result_err_cat[[VAR_NAMES]] <- rep(NA_character_, nrow(result_err_cat))
      result_full <- merge(
        result_full,
        result_err_cat,
        by = c(VAR_NAMES, "call_names"), all = TRUE)


      #===== Modify the obtained dataframe =======

      # Sort result_full by indicator_metrics
      result_full <- result_full[order(result_full[["indicator_metric"]]), ,
                                 drop = FALSE]

      # For robustness: names in labels and colors should match, otherwise
      # there is inconsistency in the grading_ruleset.
      # Check if the names match
      if (!identical(names(labels), names(colors)) ||
          !all(paste0("cat", seq_len(5)) %in% names(colors))) {
        util_warning(
          c("Could not find enough categories in custom format file,",
            "falling back to default"), applicability_problem = TRUE,
          intrinsic_applicability_problem = FALSE)
        old_o <- options(
          dataquieR.grading_formats =
            system.file("grading_formats.xlsx",
                        package = "dataquieR"),
          dataquieR.grading_rulesets =
            system.file("grading_rulesets.xlsx",
                        package = "dataquieR"))
        on.exit(options(old_o))
        labels <- util_get_labels_grading_class()
        colors <- util_get_colors()
        names(labels) <- paste0("cat", names(labels))
        names(colors) <- paste0("cat", names(colors))
      }

      # Add a column T/F to check if the indicator metric has a value
      result_full$have_value <- !util_empty(result_full$indicator_metric)

      # Add a column label
      result_full$LABEL <-
        this$labels_of_var_names_in_report[result_full$VAR_NAMES]

      # Add a column islab - with the category (e.g., "Ok") or "Not classified"
      result_full <-
        result_full %>% dplyr::mutate(islab = ifelse(!util_empty(class),
                                                     as.character(class),
                                                     "Not classified"))

      # Add a logic vector checking if the result is reasonably possible
      if("CAT_anamat" %in% colnames(result_full)) {
        result_full$not_applicable <- result_full[["CAT_anamat"]] != 1
      }

      # Add a logic vector checking if there are all metadata to compute the
      # indicator, if the value is not 1 something is missing
      if("CAT_applicability" %in% colnames(result_full)) {
        result_full$meta_data_sufficient <-
          result_full[["CAT_applicability"]] == 1
      }
      # Add a column with the categories as. cat1, cat2 and so on
      result_full$class_cat <- util_as_cat(result_full$class_num)

      # Add a column "class_color" with the corresponding color of the categories and NAs
      result_full$class_color <-  NA
      result_full$class_color[is.na(result_full$class_color) &
                                !is.na(result_full$class_cat) &
                                result_full$is_indicator] <-
        colors[paste(result_full$class_cat[is.na(result_full$class_color) &
                                             !is.na(result_full$class_cat) &
                                             result_full$is_indicator])]

      if("not_applicable" %in% colnames(result_full)){
        result_full$class_color[is.na(result_full$class_color) &
                                  result_full$not_applicable &
                                  result_full$is_indicator] <-  "#ffffff"
      }


      if("meta_data_sufficient" %in% colnames(result_full)){
        result_full$class_color[is.na(result_full$class_color) &
                                  !result_full$meta_data_sufficient &
                                  result_full$is_indicator] <- "#444444"
      }

      result_full$class_color[is.na(result_full$class_color) &
                                (result_full$other_error_stop5 |
                                   result_full$is_indicator)] <-  "#aaaaaa"

      result_full$class_color[is.na(result_full$class_color)] <-  "#ffffff"

      # Add a column to determine the brightness of the text
      # (black/white depending on the background)
      result_full$fg_color <- vapply(result_full$class_color,
                                     FUN = util_get_fg_color,
                                     FUN.VALUE = character(1))

      # create a column title_text - containing the title for the hover text
      result_full$title <-
        vapply(unique(result_full$call_names),
               util_alias2caption,
               long = FALSE,
               FUN.VALUE= character(1))[result_full$call_names]

      result_full <- result_full %>%
        dplyr::mutate(title_text = paste0(get("title"), " of ", get("LABEL")))

      # create the text of the hover box. A tag for header of level 4 with nice caption
      # and a tag for classification (e.g. Critical)
      result_full$hover_text <- paste0("<h4>", result_full[["title_text"]],
                                       "</h4> <h5>", result_full[["islab"]],
                                       "</h5>")
      result_full <- result_full[, -which(names(result_full) == c("title_text")),
                                 drop = FALSE]


      #create an alternative title_text and hover_text in case only indicator metric is selected
      result_full$title_ind <- vapply(unique(result_full$indicator_metric),
                                      util_translate_indicator_metrics,
                                      FUN.VALUE=
                                        character(1))[result_full$indicator_metric]

      result_full <- result_full %>% dplyr::mutate(title_text_ind =
                                                     paste0(get("title_ind"),
                                                            " of ",
                                                            get("LABEL")))

      # create the text of the hover box. A tag for header of level 4 with nice caption
      # and a tag for classification (e.g. Critical)
      result_full$hover_text_ind <-
        paste0("<h4>", result_full[["title_text_ind"]],
               "</h4> <h5>", result_full[["islab"]],
               "</h5>")

      result_full <- result_full[, -which(names(result_full) ==
                                            c("title_text_ind")),
                                 drop = FALSE]

      # create a readable text for the indicator metric result
      # example: Unexpected location (Percentage (0 to 100)) = 0.00%: Ok
      result_full <-
        dplyr::mutate(result_full,
                      speaking_name_indic_metric = ifelse(
                        !util_empty(get("indicator_metric")),
                        paste0(util_translate_indicator_metrics(
                          result_full[["indicator_metric"]])),
                        NA))

      result_full <-
        dplyr::mutate(result_full,
                      speaking_name_indic_metric = ifelse(
                        !util_empty(result_full[["suffixes"]]),
                        paste0(result_full[["speaking_name_indic_metric"]],
                               " ", result_full[["suffixes"]]),
                        result_full[["speaking_name_indic_metric"]]
                      ))

      result_full <-
        dplyr::mutate(result_full,
                      values_readable = ifelse(
                        !util_empty(result_full[["value"]]),
                        paste0(result_full[["speaking_name_indic_metric"]],
                               " = ", result_full[["value"]],
                               ifelse(
                                 !is.na(result_full[["class"]]),
                                 paste0(": ",
                                        result_full[["class"]]),
                                 ""),
                               " - ", result_full[["title"]]),
                        "No results available"
                      ))


      result_full <- result_full[, -which(names(result_full) ==
                                            c("speaking_name_indic_metric")),
                                 drop = FALSE]

      # define which should be in bold (there is a metric and it has a class)
      result_full <- dplyr::mutate(result_full,
                                   values_readable =
                                     ifelse(get("have_value") &
                                              !util_empty(get("class")),
                                            paste("<strong>",
                                                  get("values_readable"),
                                                  "</strong>"),
                                            get("values_readable")))

       # create a link
      result_full$ln <-
        apply(result_full[, c(LABEL, "call_names",
                              "value", "fg_color"),
                          drop=FALSE], 1,
              FUN = function(x) {
                #in case it is a summary of a single report
                if(is.null(folder_of_report)) {
                  link <-
                    util_generate_anchor_link(x[[1]],
                                              x[["call_names"]],
                                              title =
                                                htmltools::HTML(as.character(
                                                  x[["value"]])))
                } else {
                  #in case it is a summary of a group of reports
                  anchor_name <- gsub(".*-(.*)", "\\1" , x[[1]])
                  search_name <- paste0(x[[1]], ".", x[["call_names"]])
                  link <-
                    util_generate_anchor_link(anchor_name,
                                              x[["call_names"]],
                                              title =
                                                htmltools::HTML(as.character(
                                                  x[["value"]])))
                  prefix <-
                    unique(folder_of_report[names(folder_of_report)[
                      names(folder_of_report) %in% search_name]])
                  link[["attribs"]]$href <- paste0(prefix, "/.report/",
                                                   link[["attribs"]]$href)
                }

                link$attribs$style <- c(link$attribs$style,
                                        "text-decoration:none;display:block;")
                link$attribs$style <- paste0(link$attribs$style,
                                             sprintf("color:%s;",
                                                     x[["fg_color"]]))
                as.character(link)
              })

      result_full <- dplyr::mutate(result_full, ln =
                                     ifelse(util_empty(get("value")),
                                            gsub("(<a.*?[^/>]*>)[^/<]*(.*)",
                                                 "\\1&nbsp;\\2",
                                                 x = get("ln"),
                                                 perl = TRUE),
                                            get("ln")))


      #extract href to a columns and remove href as an attribute in the link
      result_full$href <- gsub('<a.*? *href *= *"([^"]*)".*?>.*',
                               "\\1",
                               x = result_full$ln, perl = TRUE)

      result_full$ln <- gsub('(<a.*?) *href *= *"[^"]*"(.*?>)',
                             "\\1\\2",
                             x = result_full$ln, perl = TRUE)

      # for all rows is_indicator == FALSE append something to All_err_msg
      result_full <-
        dplyr::mutate(result_full,
                      All_err_msg = ifelse(!get("is_indicator"),
                                           paste0(get("All_err_msg"),
                                                  " ",
                                                  '<span class="dataquieR-error-message"> ',
                                                  get("function_name"),
                                                  " is a descriptor, only.",
                                                  "</span>"),
                                           get("All_err_msg")))


      # create hover addition containing error messages
      result_full <- dplyr::mutate(result_full,
                                   hover = ifelse(!util_empty(get("All_err_msg")),
                                                  paste0("<br>","Messages: ",
                                                         "<br>",
                                                         "\n<ul>\n",
                                                         get("All_err_msg"),
                                                         "\n</ul>\n"),
                                                  paste0("")))


      # initialize the text to display inside the cell - it should be never observed
      result_full$celltext <- paste(result_full$VAR_NAMES,
                                    result_full$call_names, collapse = "/")
      result_full$o <- NA  # Initialize the order
      result_full$f <- htmltools::HTML("&nbsp;") #initialize the filters


      # define order and filter
      result_full$o <- this$order_of[paste(result_full$class_cat)]
      result_full$f <- this$filter_of[paste(result_full$class_cat)]

      #Complete text for the hover box
      result_full$text_complete_hover <- paste0(result_full$hover_text, "<br/>",
                                                result_full$values_readable,
                                                "<br/>",
                                                result_full$hover)
      result_full$text_complete_hover <-
        htmltools::htmlEscape(result_full$text_complete_hover,
                              attribute = TRUE)
      if(is.null(folder_of_report)) {
        #in case of a summary of a single report
        # create the complete href sentence to add after in the html text
        result_full$href_sentence <-
          sprintf('"if (window.hasOwnProperty(&quot;dq_report2&quot;) &amp;&amp; window.dq_report2 &amp;&amp; window.location != &quot;%s&quot;) { if (all_ids.all_ids.includes(&quot;%s&quot;)) { window.location = &quot;%s&quot; } else { window.alert(&quot;No result available&quot;) } }"',
                  result_full$href,
                  result_full$href, result_full$href)

      } else {
        #in case of a summary of a group of reports
        # create the complete href sentence to add after in the html text
        result_full$href_sentence <-
          sprintf('"if (window.hasOwnProperty(&quot;dq_report_by_overview&quot;) &amp;&amp; window.dq_report_by_overview &amp;&amp; window.location != &quot;%s&quot;) { if (all_ids.all_ids_overall.includes(&quot;%s&quot;)) { window.location = &quot;%s&quot; } else { window.alert(&quot;No result available&quot;) } }"',
                  result_full$href,
                  result_full$href, result_full$href)

      }
      # create a column with the final html text
      try(result_full$html <-
            apply(result_full[, c("href_sentence", "class_color",
                                  "o", "f",
                                  "text_complete_hover", "ln"),
                              drop=FALSE], 1,
                  FUN = function(x) {
                    ln <- x[["ln"]]
                    filter <- x[["f"]]
                    sort_value <- x[["o"]]
                    #  x <- htmltools::htmlEscape(x, attribute = TRUE)
                    text_for_html <- paste0("<pre ",
                                            "onclick=",
                                            x[["href_sentence"]],
                                            " style=",
                                            sprintf('"height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;"',
                                                    x[["class_color"]]),
                                            " sort=\"",
                                            sort_value, "\"",
                                            " filter=\"",
                                            filter,"\"",
                                            " title=\"",
                                            x[["text_complete_hover"]],"\"> ",
                                            ln, "\n</pre>")
                    as.character(text_for_html)
                  }))

      #####==== In case the argument grouped_by is selected ========
      # if not selected the default is to have both grouped_by =  c("call_names", "indicator_metric")
      if(identical(sort(grouped_by), c("call_names", "indicator_metric"))){

        result <- result_full[, -which(names(result_full) %in%
                                         c("value_readable",
                                           "hover_text",
                                           "ln", "href",
                                           "hover",
                                           "text_complete_hover")),
                              drop = FALSE]
        rm(result_full)
      } else if (grouped_by == "call_names" | grouped_by == "indicator_metric") {

        result_full <- result_full[,c("VAR_NAMES","call_names",
                                      "class", "indicator_metric",
                                      "indicator_metric_suff", "suffixes",
                                      "value", "values_raw",
                                      "n_classes", "STUDY_SEGMENT",
                                      "function_name", "class_num",
                                      "All_err_msg", "LABEL",
                                      "class_cat", "class_color",
                                      "values_readable",
                                      "hover_text", "ln", "hover_text_ind",
                                      "href_sentence", "celltext", "o", "f"),
                                   drop = FALSE]

        # ==== Separate the case of call_names and indicator_metric ===

        if(grouped_by == "call_names") {
          #####==== In case the argument grouped_by = "call_names" is selected===

          # decreasing order of VAR_NAMES, call_names and class_num
          result_full <- result_full[order(result_full$VAR_NAMES,
                                           result_full$call_names,
                                           result_full$class_num,
                                           decreasing = TRUE), , drop = FALSE]

          #create a code with VAR_NAMES and call_names
          result_full$code <- paste(result_full$VAR_NAMES,
                                    result_full$call_names)
        } else if (grouped_by == "indicator_metric") {
          #####==== In case the argument grouped_by = "indicator_metric" is selected====

          # decreasing order of VAR_NAMES, call_names and class_num
          result_full <- result_full[order(result_full$VAR_NAMES,
                                           result_full$indicator_metric_suff,
                                           result_full$class_num,
                                           decreasing = TRUE), , drop = FALSE]

          #create a code with VAR_NAMES and indicator_metric
          result_full$code <- paste(result_full$VAR_NAMES,
                                    result_full$indicator_metric_suff)

        }

        #####==== The following part is the same for the 2 cases
        # grouped_by = "indicator_metric" or
        # grouped_by = "call_names"  =========

        # return only first occurrence in the compared vectors, to get only the first row with
        # the worst value
        result_call_name <- result_full[match(unique(result_full$code),
                                              result_full$code), ,drop = FALSE]

        # return rows of result_full that do not have a match in result_call_name
        # If NULL = all variables in common across the 2 dataframe will be used
        result_full <- suppressMessages(dplyr::anti_join(result_full,
                                                         result_call_name,
                                                         by = NULL))

        # create a temporary object tO remove strong tag from values_readable
        List_other_indicators <- result_full[ , c("VAR_NAMES",
                                                  "call_names",
                                                  "code",
                                                  "indicator_metric",
                                                  "indicator_metric_suff",
                                                  "suffixes",
                                                  "class",
                                                  "values_readable"),
                                              drop = FALSE]
        # TO remove strong tag : remove all rows without a result (class = NA)
        List_other_indicators <- dplyr::filter(List_other_indicators,
                                               get("values_readable") !=
                                                 "No results available")

        # TO remove strong tag: keep only a part of the string
        # First create a temporary data frame with the code and the worst class
        class_worst_result <- result_call_name[, c("code", "class"),
                                               drop = FALSE]
        class_worst_result <-
          class_worst_result[!util_empty(class_worst_result$class), ,
                             drop = FALSE]
        colnames(class_worst_result)[colnames(class_worst_result) == "class"] =
          "class_worst_result"
        # Then merge data frames to add a column containing the worst class in the
        # list of other indicators
        List_other_indicators <- merge(List_other_indicators,
                                       class_worst_result,
                                       by = "code",
                                       all.x = TRUE)
        rm(class_worst_result)

        # Finally remove the strong tag only if the class is different from
        # the worst class
        List_other_indicators <-
          dplyr::mutate(List_other_indicators,
                        values_readable =
                          ifelse(class ==
                                   get("class_worst_result") |
                                   is.na(get("class_worst_result")),
                                 get("values_readable"),
                                 gsub('<strong> *(.*)<.*',
                                      "\\1",
                                      x = List_other_indicators$values_readable,
                                      perl = TRUE)))

        # create a row with the content of multiple rows creating a bullet list -
        # if there are other rows
        # first add the tag li to each row in values_readable
        if(nrow(List_other_indicators) != 0) {
          List_other_indicators$values_readable <-
            paste0("<li>",
                   List_other_indicators$values_readable,
                   "</li>")
          # Then i merged the rows by code (VAR_NAMES call_names)
          List_other_indicators <- List_other_indicators %>%
            dplyr::group_by(get("code")) %>%
            dplyr::summarise(values_readable = paste(get("values_readable"),
                                                     collapse = " "))
          # renamed to have the first column "code" and not get("code")
          colnames(List_other_indicators) <- c("code", "values_readable")


          #create a bullet points list
          #      List_other_indicators$values_readable <- paste0("\n<ul>\n",
          #                                                     List_other_indicators$values_readable,
          #                                                     "\n</ul>\n")
          #      # I add now the list of other value readable to be add under the indicator
          # that is in bold and define the color of the cell
          result_call_name <- merge(result_call_name, List_other_indicators,
                                    by = "code", all.x = TRUE)


          # create a column with all value_readable
          result_call_name$values_readable_complete <-
            apply(result_call_name[, names(result_call_name) %in%
                                     c("values_readable.x",
                                       "values_readable.y")],
                  1, function(x) paste(na.omit(x), collapse = "\n"))
          # remove non useful columns
          result_call_name <-
            result_call_name[, -which(names(result_call_name) %in%
                                        c("values_readable.x",
                                          "values_readable.y")),
                             drop = FALSE]
          rm(List_other_indicators)
        }

        #Now create the new complete list of error messages
        List_other_errors <- result_full[ ,
                                          c("VAR_NAMES",
                                            "call_names",
                                            "code",
                                            "indicator_metric",
                                            "indicator_metric_suff",
                                            "suffixes",
                                            "class",
                                            "All_err_msg"), drop = FALSE]
        # remove rows without an error message
        List_other_errors <-
          List_other_errors[!is.na(List_other_errors$All_err_msg), ,
                            drop = FALSE]
        # keep only non-repeated error messages for VAR_NAMES and call_names

        List_other_errors <- List_other_errors[, c("code", "All_err_msg"),
                                               drop = FALSE]
        List_other_errors <- unique(List_other_errors[ , c("code",
                                                           "All_err_msg"),
                                                       drop = FALSE])

        # remove error messages matching the error message of the indicator with the
        # worst result
        # create a temporary dataframe
        errors_in_result_call_name <-
          result_call_name[ , c("code", "All_err_msg"), drop = FALSE]

        # return rows of List_other_errors that do not have a match in errors_in_result_call_name
        List_other_errors <-
          suppressMessages(dplyr::anti_join(List_other_errors,
                                            errors_in_result_call_name,
                                            by = NULL))
        rm(errors_in_result_call_name, result_full)

        # Then merge the rows by code (VAR_NAMES call_names)
        List_other_errors <- List_other_errors %>%
          dplyr::group_by(get("code")) %>%
          dplyr::summarise(All_err_msg = paste(get("All_err_msg"),
                                               collapse = " "))
        # renamed to have the first column "code" and not get("code")
        colnames(List_other_errors)<- c("code", "All_err_msg")

        #Then add the error messages to the dataframe
        result_call_name <- merge(result_call_name, List_other_errors,
                                  by = "code", all.x = TRUE)

        # create a column with all error messages
        result_call_name$All_err_msg_complete <-
          apply(result_call_name[, names(result_call_name) %in%
                                   c("All_err_msg.x", "All_err_msg.y")],
                1, function(x) paste(na.omit(x),
                                     collapse = "\n"))

        # create hover containing the bullet point list of error messages and the word
        # message is there is messages in All_err_msg_complete
        result_call_name <-
          dplyr::mutate(result_call_name,
                        hover =
                          ifelse(!util_empty(get("All_err_msg_complete")),
                                 paste0("<br>","Messages: ", "<br>",
                                        "\n<ul>\n",
                                        get("All_err_msg_complete"),
                                        "\n</ul>\n"),
                                 paste0("")))
        # remove non useful columns
        result_call_name <-
          result_call_name[,-which(names(result_call_name) %in%
                                     c("All_err_msg.x",
                                       "All_err_msg.y")),
                           drop = FALSE]
        rm(List_other_errors)



        # == modify the title of the hover text in case only indicator metric is selected

        if("call_names" %in% grouped_by) {
          # complete text for the hover box
          result_call_name$text_complete_hover <-
            paste0(result_call_name$hover_text, "<br/>",
                   result_call_name$values_readable, "<br/>",
                   result_call_name$hover)
          result_call_name$text_complete_hover <-
            htmltools::htmlEscape(result_call_name$text_complete_hover,
                                  attribute = TRUE)
        }  else if (grouped_by == "indicator_metric" ) {

          # complete text for the hover box
          result_call_name$text_complete_hover <-
            paste0(result_call_name$hover_text_ind, "<br/>",
                   result_call_name$values_readable, "<br/>",
                   result_call_name$hover)
          result_call_name$text_complete_hover <-
            htmltools::htmlEscape(result_call_name$text_complete_hover,
                                  attribute = TRUE)
        }

        # create the new html column
        try(result_call_name$html <-
              apply(
                result_call_name[, c("href_sentence",
                                     "class_color",
                                     "o",
                                     "f",
                                     "text_complete_hover",
                                     "ln"), drop =
                                   FALSE],
                1,
                FUN = function(x) {
                  ln <- x[["ln"]]
                  filter <- x[["f"]]
                  sort_value <- x[["o"]]
                  #  x <- htmltools::htmlEscape(x, attribute = TRUE)
                  text_for_html <-
                    paste0(
                      "<pre ",
                      "onclick=",
                      x[["href_sentence"]],
                      " style=",
                      sprintf(
                        '"height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;"',
                        x[["class_color"]]
                      ),
                      " sort=\"",
                      sort_value,
                      "\"",
                      " filter=\"",
                      filter,
                      "\"",
                      " title=\"",
                      x[["text_complete_hover"]],
                      "\"> ",
                      ln,
                      "\n</pre>"
                    )
                  as.character(text_for_html)
                }
              ))
        result<- result_call_name
      }

      # remove cases without indicator metric
      if("indicator_metric"%in% grouped_by){
        result<- result[!util_empty(result$indicator_metric), , drop = FALSE]
      }

      #rename value as category and html as value
      result1 <- dplyr::rename(result, category = value, value = html)

      # ==== create column of the final table in the case call_names is in the argument
      if("call_names" %in% grouped_by) {

        if (length(grouped_by) > 1) {
          result2 <- result1[, c("VAR_NAMES",
                                 "call_names",
                                 "indicator_metric",
                                 "value"),
                             drop = FALSE]
          result2$cl <- paste0(result2$call_names, "_",
                               result2$indicator_metric)

          result2 <- result2[, c("VAR_NAMES","cl", "value"), drop = FALSE]

          #remove duplicated rows in case present
          result2 <-  dplyr::distinct(result2)

          result2 <- stats::reshape(result2,
                                    idvar = c("VAR_NAMES"),
                                    timevar = "cl",
                                    direction = "wide")

          names(result2)[startsWith(names(result2), "value.")] <-
            substr(names(result2)[startsWith(names(result2),
                                             "value.")],
                   nchar("value.") + 1,
                   nchar(names(result2)[startsWith(names(result2),
                                                   "value.")]))
        } else {
          result2 <- result1[, c("VAR_NAMES",
                                 "call_names",
                                 "indicator_metric",
                                 "value"),
                             drop = FALSE]
          result2 <- dplyr::select(result2, "VAR_NAMES",
                                   paste0(grouped_by),
                                   "value")
          names(result2) <- c("VAR_NAMES", "cl", "value")

          #remove dupliates in case present
          result2 <- dplyr::distinct(result2)

          result2 <- stats::reshape(result2,
                                    idvar = c("VAR_NAMES"),
                                    timevar = "cl",
                                    direction = "wide")

          names(result2)[startsWith(names(result2), "value.")] <-
            substr(names(result2)[startsWith(names(result2),
                                             "value.")],
                   nchar("value.") + 1,
                   nchar(names(result2)[startsWith(names(result2),
                                                   "value.")]))

        }

        # create a dataframe with unique call_names in the first column and
        # indicator_metrics in the second column
        mapping_of_cns <-
          expand.grid(call_names = unique(result$call_names),
                      indicator_metric = unique(na.omit(result$indicator_metric)),
                      stringsAsFactors = FALSE)


        # append a new column with function names
        mapping_of_cns$fname <-
          function_name_mapping[mapping_of_cns[["call_names"]]];
        # create a new column called colname containing the combination
        # selected for grouped-by. Example:
        # if only indicator_metric was selected, the column will contains only indicator_metrics
        if (all(c("call_names", "indicator_metric") %in% grouped_by)) {
          mapping_of_cns$colname <- paste0(mapping_of_cns[["call_names"]], '_',
                                           mapping_of_cns[["indicator_metric"]])
        } else if (all(c("call_names") %in% grouped_by)) {
          mapping_of_cns$colname <- paste0(mapping_of_cns[["call_names"]])
          #     } else if(all(c("indicator_metric") %in% grouped_by)) {
          #       mapping_of_cns$colname <- paste0(mapping_of_cns[["indicator_metric"]])
        } else {
          util_error("this group by is not yet supported")
        }
        # create a list of suffixes for each call_name and function name
        # example for acc_loess_observer, the suffix is observer

        mapping_of_cns$fname2 <-
          apply(mapping_of_cns[, "call_names", drop = FALSE],
                1, FUN = function(x) {
                  util_map_by_largest_prefix(x,
                                             haystack =
                                               util_all_ind_functions())
                })

        suffixes <- mapply(SIMPLIFY = FALSE,
                           cn = mapping_of_cns[["call_names"]],
                           fn = mapping_of_cns$fname2,
                           FUN = function(cn, fn) {
                             if (startsWith(cn, fn)) {
                               substr(cn, nchar(fn) + 1 + 1, nchar(cn))
                               # name + "_" (first +1), start is the next character (second +1)
                             } else {
                               cn
                             }
                           })

        # get a table with for each function, the short-name, implementations and so on
        acronyms_tab <- util_get_concept_info("implementations")

        # === Define the title of the matrix columns =====
        # First select the short name of the function from "implementations" in
        # dq control file
        mapping_of_cns$acronyms <-
          util_map_labels(mapping_of_cns$fname2,
                          acronyms_tab,
                          # to = "dq_report2_short_title",
                          to = "matrix_column_title_report",
                          from = "function_R",
                          ifnotfound = util_abbreviate(mapping_of_cns$fname))
        # Prepare the suffixes
        suffixes <- gsub("_", " ", vapply(suffixes, as.character,
                                          FUN.VALUE = character(1)))
        suffixes[!util_empty(suffixes)] <-
          paste0(":",
                 abbreviate(
                   suffixes[!util_empty(suffixes)],
                   minlength = 3
                 ))

        # Prepare the indicator names
        mapping_of_cns$imtitle <-
          util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
                                           long = FALSE,
                                           short = FALSE)
        mapping_of_cns$imtitle[util_empty(mapping_of_cns$imtitle)] <-
          mapping_of_cns[["indicator_metric"]][util_empty(mapping_of_cns$imtitle)]

        mapping_of_cns$imtitle_short <-
          util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
                                           long = FALSE, short = TRUE)
        mapping_of_cns$imtitle_short[util_empty(mapping_of_cns$imtitle_short)] <-
          mapping_of_cns[["indicator_metric"]][util_empty(mapping_of_cns$imtitle_short)]

        # Create the actual column names for the matrix in column "coltitle"
        if (all(c("call_names", "indicator_metric") %in% grouped_by)) {
          mapping_of_cns$coltitle <- paste0(mapping_of_cns$acronyms, suffixes,
                                            ", ", mapping_of_cns$imtitle_short)
        } else if (all(c("call_names") %in% grouped_by)) {
          mapping_of_cns$coltitle <- paste0(mapping_of_cns$acronyms, suffixes)
          #      } else if(all(c("indicator_metric") %in% grouped_by)) {
          #        mapping_of_cns$coltitle <- paste0(mapping_of_cns$imtitle, suffixes)
        } else {
          util_error("this group by is not yet supported")
        }

        # Definition of the description of the hover text of column titles in the final matrix
        #      if("call_names" %in% grouped_by){
        # in all other cases of grouped_by
        # create description of the hover text of column titles in the final matrix
        mapping_of_cns$coldesc <- vapply(mapping_of_cns$fname,
                                         FUN.VALUE = character(1),
                                         util_col_description)

        # order the column based on the object ordered_call_names
        # First create an order based on call names e.g., 3
        mapping_of_cns$order_call <-
          setNames(seq(length(this$ordered_call_names)),
                   nm = this$ordered_call_names)[
                     mapping_of_cns[["call_names"]]]
        # FIXME: If called with grouped_by = c("call_names"), the columns imtitle and imtitle_short are NA at their second occurrence
        # Then create an order based on indicator metric e.g., 9
        mapping_of_cns$order_slot <- util_order_of_indicator_metrics(mapping_of_cns[["indicator_metric"]]) # was rank(mapping_of_cns[["indicator_metric"]])

        # Then combine the 2 values e.g., 3009
        mapping_of_cns$order <-
          (mapping_of_cns$order_call * 10 **
             ceiling(log(max(mapping_of_cns$order_slot, na.rm = TRUE),
                         base = 10))) + mapping_of_cns$order_slot

        # create an object with the order
        order_of_cols <- c(
          VAR_NAMES = 0,
          setNames(mapping_of_cns$order, nm = mapping_of_cns$colname)
        )

        # check if the columns of result2 match the values in order_of_cols
        util_stop_if_not(all(colnames(result2) %in% names(order_of_cols)))

        # reorder the column of the result based on the order of the object just created
        result2 <-
          result2[, order(order_of_cols[colnames(result2)]), FALSE]


        # create a list containing for each combination of the selected grouped_by
        # the correspondent description
        descs <- setNames(mapping_of_cns$coldesc,
                          nm = mapping_of_cns$colname)[colnames(result2)]
        descs[util_empty(descs)] <- ""

        # rename the columns of result2 with the user friendly name
        colnames_tech <- colnames(result2)
        colnames(result2)[colnames(result2) %in% mapping_of_cns$colname] <-
          setNames(mapping_of_cns$coltitle,
                   nm = mapping_of_cns$colname)[
                     colnames(result2)[colnames(result2) %in%
                                         mapping_of_cns$colname]]

        # creation of the final matrix in case indicator_metric is selected ===
      }    else if (grouped_by == "indicator_metric" ) {
        result2 <- result1[, c("VAR_NAMES",
                               "indicator_metric_suff",
                               "value"), drop = FALSE]

        #remove dupliates in case present
        result2 <- dplyr::distinct(result2)

        result2  <- stats::reshape(result2,
                                   idvar = c("VAR_NAMES"),
                                   timevar = "indicator_metric_suff",
                                   direction = "wide")

        names(result2)[startsWith(names(result2), "value.")] <-
          substr(names(result2)[startsWith(names(result2),
                                           "value.")],
                 nchar("value.") + 1,
                 nchar(names(result2)[startsWith(names(result2),
                                                 "value.")]))

        # create a dataframe with unique call_names in the first column and
        # indicator_metrics in the second column
        mapping_of_cns <-
          expand.grid(call_names = unique(result$call_names),
                      indicator_metric = unique(result$indicator_metric),
                      stringsAsFactors = FALSE)


        # add a new column with the function name
        mapping_of_cns$function_name2 <-
          apply(mapping_of_cns[, "call_names",
                               drop = FALSE],
                1, FUN = function(x) {
                  util_map_by_largest_prefix(x,
                                             haystack =
                                               util_all_ind_functions())
                })

        # add a column with suffixes
        mapping_of_cns <-
          dplyr::mutate(mapping_of_cns,
                        suffixes = ifelse(
                          startsWith(mapping_of_cns[["call_names"]],
                                     mapping_of_cns[["function_name2"]]),
                          substr(mapping_of_cns[["call_names"]],
                                 nchar(mapping_of_cns[["function_name2"]]) +
                                   1 + 1,
                                 nchar(mapping_of_cns[["call_names"]])),
                          # name + "_" (first +1), start is the next character (second +1)
                          mapping_of_cns[["call_names"]]
                        ))

        # create a new column with indicator_metric and suffixes if present
        mapping_of_cns <-
          dplyr::mutate(mapping_of_cns,
                        indicator_metric_suff =
                          ifelse(!util_empty(get("suffixes")),
                                 paste0(get("indicator_metric"),"_",
                                        get("suffixes")),
                                 get("indicator_metric")))

        # append a new column with function names
        mapping_of_cns$fname <-
          function_name_mapping[mapping_of_cns[["call_names"]]];
        # create a new column called colname containing the combination
        # selected for grouped-by. Example:
        # if only indicator_metric was selected, the column will contains only indicator_metrics
        mapping_of_cns$colname <-
          paste0(mapping_of_cns[["indicator_metric"]])
        mapping_of_cns$colname_suff <-
          paste0(mapping_of_cns[["indicator_metric_suff"]])

        # Prepare the indicator names
        mapping_of_cns$imtitle <-
          util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
                                           long = FALSE, short = FALSE)
        mapping_of_cns$imtitle[util_empty(mapping_of_cns$imtitle)] <-
          mapping_of_cns[["indicator_metric"]][util_empty(mapping_of_cns$imtitle)]

         # Create the actual column names for the matrix in column "coltitle"
        mapping_of_cns$coltitle <- paste0(mapping_of_cns$imtitle, " ",
                                          mapping_of_cns$suffixes)

        # Definition of the description of the hover text of column titles in the final matrix
        # remove prefix from indicator name
        mapping_of_cns$indicators_no_prefix <-
          gsub(".*?_(.*)","\\1",
               vapply(mapping_of_cns$colname,
                      as.character,
                      FUN.VALUE = character(1)))
        # import the dqi informations
        dqi_info <- util_get_concept_info("dqi") # order_nr. AbbreviationMetrics$order
        # use the Definition from dqi to create the description of the hover text
        mapping_of_cns$coldesc <-
          util_map_labels(mapping_of_cns$indicators_no_prefix,
                          dqi_info,
                          to = "Definition",
                          from = "abbreviation")


        # order the columns
        # create an order based on indicator metric e.g., 9
        mapping_of_cns$order <- util_order_of_indicator_metrics(mapping_of_cns[["indicator_metric_suff"]]) # was rank(mapping_of_cns[["indicator_metric_suff"]])

        # create an object with the order
        order_of_cols <- c(
          VAR_NAMES = 0,
          setNames(mapping_of_cns$order, nm = mapping_of_cns$colname_suff)
        )

        util_stop_if_not(all(colnames(result2) %in% names(order_of_cols)))

        # reorder the column of the result based on the order of the object just created
        result2 <-
          result2[, order(order_of_cols[colnames(result2)]), FALSE]

        # create a list containing for each combination of the selected grouped_by
        # the correspondent description
        descs <- setNames(mapping_of_cns$coldesc,
                          nm = mapping_of_cns$colname_suff)[colnames(result2)]
        descs[util_empty(descs)] <- ""

        # rename the columns of result2 with the user friendly name
        colnames_tech <- colnames(result2)
        colnames(result2)[colnames(result2) %in% mapping_of_cns$colname_suff] <-
          setNames(mapping_of_cns$coltitle,
                   nm = mapping_of_cns$colname_suff)[
                     colnames(result2)[colnames(result2) %in%
                                         mapping_of_cns$colname_suff]]

      }

      # replace var_names with labels in the first column of the matrix
      if (VAR_NAMES %in% colnames(result2)) {
        Variables <- util_map_labels(result2[[VAR_NAMES]],
                                     meta_data = meta_data,
                                     to = this$label_col,
                                     from = VAR_NAMES)
        # label_col <- attr(Variables, "label_col")
        result2[[VAR_NAMES]] <- Variables
        colnames(result2)[colnames(result2) == VAR_NAMES] <- "Variables"
        label_col <- this$label_col
      } else {
        label_col <- VAR_NAMES
      }

      # In case of summary for dq_report_by
      # Create the href for each cell "Total"
      if (!is.null(folder_of_report) ) {

        # In case there is more than one strata, and the name of the variables
        # are modified because repeated in several folders
        if (!is.null(var_uniquenames)) {
          # Prepare vars_folder_of_report: a named vector containing the new name
          # of the variable (e.g., "sd1-SEX_0_1-ITEM_5_0"). The names of this
          # vector contains the name of the folder of the report
          vars_folder_of_report <- setNames(names(folder_of_report),
                                            folder_of_report)
          vars_folder_of_report <- gsub("^(.*)\\.(.*)" , "\\1",
                                        vars_folder_of_report)
          vars_folder_of_report <-
            vars_folder_of_report[!duplicated(vars_folder_of_report)]
          #Prepare a data frame with the different names of the same variable
          var_uniquenames <- do.call("rbind", var_uniquenames)
          rownames(var_uniquenames) <- NULL

          #Prepare a named vector of the variables (e.g., SEX_0) and the folder
          name_and_folder <- vars_folder_of_report
          simple_var_names <-
            var_uniquenames[var_uniquenames$new_names_with_label %in%
                              name_and_folder, , drop = FALSE]
          name_and_folder <- setNames(simple_var_names$name_label,
                                     nm = names(name_and_folder))

          # ix the names of this vector so that include the complete path to a
          # variable inside the folder
          names(vars_folder_of_report) <-
            mapply(x = prep_link_escape(name_and_folder),
                   y = names(vars_folder_of_report),
                   SIMPLIFY = FALSE,
                   FUN = function(x, y){
                     z <- paste0(y, "/.report/VAR_", x, ".html#", x)
                     return(z)
                   })

          #rename content of vars_folder_of_report, so that now contain
          # the new name e.g.,  "sd1-SEX_0_0-v00000" instead of
          # previous "sd1-SEX_0_0-CENTER_0" (to match content of this$rowmaxes)
          var_uniquenames_fil <-
            var_uniquenames[var_uniquenames$new_names_with_label %in%
                              vars_folder_of_report, , drop = FALSE]
          vars_folder_of_report <-
            setNames(var_uniquenames_fil$new_names_with_varnames,
                     nm = names(vars_folder_of_report))

          # make unique the names a created named vector containing
          # this$rowmaxes$cell_text. The names comes now from the
          # column this$rowmaxes$VAR_NAMES, because
          # the names of this$rowmaxes$cell_text contain repeated variable names
          new_rowmaxes_cell_text <- this$rowmaxes$cell_text
          names(new_rowmaxes_cell_text) <- this$rowmaxes$VAR_NAMES

          # replace the href in rowmaxes used for links from var_names and total cells
          this$rowmaxes$cell_text <-
            mapply(needle = setNames(nm = names(new_rowmaxes_cell_text)),
                   MoreArgs = list(
                     haystack = new_rowmaxes_cell_text,
                     corresponding_value_for_needle = vars_folder_of_report),
                   SIMPLIFY = FALSE,
                   FUN = function(needle, corresponding_value_for_needle,
                                  haystack){
                     haystack <- haystack[[as.character(needle)]]
                     haystack <-
                       gsub('(?=VAR_)(.*)(?="\\sstyle)',
                            names(corresponding_value_for_needle[
                              corresponding_value_for_needle %in% needle]),
                            haystack,
                            perl = TRUE)
                     return(haystack)
                   })
        } else {
          #Create a named vector vars_folder_of_report containing the variable
          # name (e.g., "SEX_0"). The names are the folder of the variable
          vars_folder_of_report <- setNames(names(folder_of_report),
                                            folder_of_report)
          vars_folder_of_report <- gsub("^(.*)\\.(.*)" , "\\1",
                                        vars_folder_of_report)
          vars_folder_of_report <-
            vars_folder_of_report[!duplicated(vars_folder_of_report)]

          names(vars_folder_of_report) <-
            mapply(variable_nm = prep_link_escape(vars_folder_of_report),
                   folder_for_href = names(vars_folder_of_report),
                   SIMPLIFY = FALSE,
                   FUN = function(variable_nm, folder_for_href){
                     z <- paste0(folder_for_href, "/.report/VAR_",
                                 variable_nm, ".html#", variable_nm)
                     return(z)
                   })

          # replace the href in rowmaxes used for links from var_names and Total cells
          this$rowmaxes$cell_text <-
            mapply(x = setNames(nm = names(this$rowmaxes$cell_text)),
                   MoreArgs = list(
                     z = this$rowmaxes$cell_text,
                     y = vars_folder_of_report),
                   SIMPLIFY = FALSE,
                   FUN = function(x, y, z){
                     z <- z[[as.character(x)]]

                     z <- gsub('(?=VAR_)(.*)(?="\\sstyle)',
                               names(y[y %in% x]),
                               z,
                               perl = TRUE)
                     return(z)
                   })
        }
      }

      this$rowmaxes$VAR_NAMES_mapped <- prep_map_labels(this$rowmaxes$VAR_NAMES,
                                                        meta_data = meta_data,
                                                        to = label_col)

      ordered_var_names_mapped <- prep_map_labels(ordered_var_names,
                                                  meta_data = meta_data,
                                                  to = label_col)

      result2 <- result2[util_order_by_order(result2$Variables,
                                             ordered_var_names_mapped),
                         , drop = FALSE]

      result2$Total <- setNames(this$rowmaxes$cell_text,
                                nm = this$rowmaxes$VAR_NAMES_mapped)[result2$Variables]

      descs <- c(descs, "Total DQ for Items")


      colnames_speaking_from_tech <- setNames(colnames(result2),
                                              nm = colnames_tech)

      #In case of summary for dq_report_by
      if (!is.null(folder_of_report)) {
        # if more than 1 strata is present
        if (!is.null(var_uniquenames)) {
          # create a named vector with as variable name the new name that
          # will appear on the table (it has extra spaces)
          # (e.g.,  "sd1-SEX_0_1 - QUEST_DT_0" instead of
          # "sd1-SEX_0_1-QUEST_DT_0") and as name
          # the folder containing the variable
          new_var_name_in_table <- vars_folder_of_report
          simple_var_names <-
            var_uniquenames[var_uniquenames$new_names_with_varnames %in%
                              new_var_name_in_table, , drop = FALSE]

          new_var_name_in_table <-
            setNames(simple_var_names$result2_Variables_match,
                     nm = names(new_var_name_in_table))

          # create a new named vector href with the correct folder (as name) and
          # the new variable name to read in the table (with extra white spaces)
          href <-
            setNames(names(new_var_name_in_table[new_var_name_in_table %in%
                                                   result2$Variables]),
                     nm = new_var_name_in_table[new_var_name_in_table %in%
                                                  result2$Variables])
          filter <- setNames(result2$Variables, nm = result2$Variables)
          data <- setNames(result2$Variables, nm = result2$Variables)

        } else {
          # define the href, a named vector containing the folder of the variable.
          # The names contain the variable name (e.g., "SEX_0")
          href <-
            setNames(names(vars_folder_of_report[vars_folder_of_report %in%
                                                   result2$Variables]),
                     nm = vars_folder_of_report[vars_folder_of_report %in%
                                                  result2$Variables])
          filter <- setNames(result2$Variables, nm = result2$Variables)
          data <- setNames(result2$Variables, nm = result2$Variables)
        }

        result2$Variables <-
          mapply(x = setNames(nm = result2$Variables),
                 MoreArgs = (list(
                   href = #sprintf('javascript:console.log("%s")', href),
                     href,
                   filter = filter,
                   data =  data
                 )), SIMPLIFY = FALSE,
                 FUN = function(x, href, filter, data){
                   text_for_html <-
                     paste0("<pre ",
                            'onclick=\"\"',
                            ' style=',
                            '"height: 100%; min-height: 2em; font-size: 16px; font-family: serif; font-style: normal; margin: 0em; color: #0066CC; padding: 0em; background: #ffffff; cursor: pointer; text-align: right;"',
                            " filter=\"", filter[x],"\">\n",
                            "<a href=\"", href[names(href) %in% x], '"',
                            'style=\"text-decoration:none;display:block;',
                            "\"> ",
                            data[x], "</a>\n</pre>")
                   as.character(text_for_html)
                   return(text_for_html)
                 })

        tb <-
          util_suppress_warnings(util_html_table(
            result2,
            filter = "top", options = list(scrollCollapse = TRUE,
                                           scrollY = "75vh"),
            is_matrix_table = TRUE, rotate_headers =
              (ncol(result2) > 5),
            link_variables = FALSE,
            cols_are_indicatormetrics = !TRUE,
            colnames_aliases2acronyms = FALSE,
            descs = descs,
            meta_data = meta_data,
            label_col = label_col,
            dl_fn = prep_link_escape(paste0("Summary-",
                                            this[["title"]],
                                            "-",
                                            this[["subtitle"]]),
                                     html = TRUE),
            title = this[["title"]],
            messageTop = this[["subtitle"]],
            messageBottom = sprintf("generated by %s %s",
                                    paste(packageName()),
                                    paste(packageVersion(
                                      packageName()))
            ),
            output_format = "HTML",
            col_tags = list(`All columns` = colnames(result2),
                            `Integrity` = c("Variables",
                                            colnames_speaking_from_tech[grep(
                                              "^int_", value = TRUE,
                                              colnames_tech)], "Total"),
                            `Completeness` = c("Variables",
                                               colnames_speaking_from_tech[grep(
                                                 "^com_", value = TRUE,
                                                 colnames_tech)], "Total"),
                            `Consistency` = c("Variables",
                                              colnames_speaking_from_tech[grep(
                                                "^con_", value = TRUE,
                                                colnames_tech)], "Total"),
                            `Accuracy` = c("Variables",
                                           colnames_speaking_from_tech[grep(
                                             "^acc_", value = TRUE,
                                             colnames_tech)], "Total")
            )
          ), classes = LONG_LABEL_EXCEPTION)
      } else {
        tb <- util_suppress_warnings(
          util_html_table(
            result2,
            filter = "top", options = list(scrollCollapse = TRUE,
                                           scrollY = "75vh"),
            is_matrix_table = TRUE, rotate_headers =
              (ncol(result2) > 5),
            cols_are_indicatormetrics = !TRUE,
            colnames_aliases2acronyms = FALSE,
            descs = descs,
            meta_data = meta_data,
            label_col = label_col,
            dl_fn = prep_link_escape(paste0("Summary-",
                                            this[["title"]],
                                            "-",
                                            this[["subtitle"]]),
                                     html = TRUE),
            title = this[["title"]],
            messageTop = this[["subtitle"]],
            messageBottom = sprintf("generated by %s %s",
                                    paste(packageName()),
                                    paste(packageVersion(
                                      packageName()))
            ),
            output_format = "HTML",
            col_tags = list(`All columns` = colnames(result2),
                            `Integrity` = c("Variables",
                                            colnames_speaking_from_tech[grep(
                                              "^int_", value = TRUE,
                                              colnames_tech)], "Total"),
                            `Completeness` = c("Variables",
                                               colnames_speaking_from_tech[grep(
                                                 "^com_", value = TRUE,
                                                 colnames_tech)], "Total"),
                            `Consistency` = c("Variables",
                                              colnames_speaking_from_tech[grep(
                                                "^con_", value = TRUE,
                                                colnames_tech)], "Total"),
                            `Accuracy` = c("Variables",
                                           colnames_speaking_from_tech[grep(
                                             "^acc_", value = TRUE,
                                             colnames_tech)], "Total")
            )
          ), classes = LONG_LABEL_EXCEPTION)
      }

      jqui <- rmarkdown::html_dependency_jqueryui()
      jqui$stylesheet <- "jquery-ui.min.css"

      res <- htmltools::browsable(htmltools::tagList(
        rmarkdown::html_dependency_jquery(),
        jqui,
        htmltools::htmlDependency(
          name = "tippy",
          version = "6.7.3",
          src = system.file("tippy", package = "dataquieR"),
          script = c("core.js", "tippy.js")
        ),
        htmltools::htmlDependency(
          name = "clipboard",
          version = "2.0.11",
          src = system.file("clipboard", package = "dataquieR"),
          script = c("clipboard.min.js")
        ),
        html_dependency_dataquieR(),
        tb,
        htmltools::tags$script(paste0('$(function() {',
                                      'if (!window.hasOwnProperty("dq_report2") || !window.dq_report2) {',
                                      'if (!window.hasOwnProperty("dq_report_by_overview") || !window.dq_report_by_overview) {',
                                      ' $("a").attr("href", "javascript:alert(\\"links work in dq_report2 reports, only.\\")")',
                                      '} else {' ,
                                      ' ',
                                      '}',
                                      '}} )'))
      ))
    }

    util_attach_attr(res,
                     repsum_wide = util_attach_attr(result2,
                                                    label_col = label_col),
                     this = this)
  })}
