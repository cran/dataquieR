#' Render a table summarizing dataquieR results
#'
#' @param x a report summary (`summary(r)`)
#' @param grouped_by define the columns of the resulting matrix. It can be either
#' "call_names", one column per function,  or "indicator_metric", one column per indicator
#' or both c("call_names", "indicator_metric"). The last combination is the default
#' @return something, `htmltools` can render
#'
#'
#'
#' @keywords internal
#'
util_render_table_dataquieR_summary <- function(x,
                                                grouped_by =
                                                  c("call_names",
                                                    "indicator_metric")) { # FIXME: -> util_*, remove other table printer functions, make prep_extract_summary util_* and, maybe, remove ggplot2 alternative pre for pie charts
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
    } else
      {  # in the case of actual metrics and not only error messages
      # save the original long format in a new object before to continue
#      .result0 <- result


      #===== Table and html text preparation ===

      result_full <- result

      #create a named vector with names = call_names and values = function_name
      function_name_mapping <- setNames(result_full$function_name,
                                        nm = result_full$call_names)


      # create the function name that corresponds to the actual call_names,
      # e.g., with con_hard_limits instead of con_limit_deviations
      result_full$function_name2 <- apply(result_full[, "call_names",
                                                      drop = FALSE],
                                          1, FUN = function(x) {
                                            util_map_by_largest_prefix(x,
                                            haystack = util_all_ind_functions())
                                          })

      # add a column with suffixes
      result_full <- dplyr::mutate(result_full,
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

#      # reduce suffixes to an abbreviation
#      suffixes[!util_empty(suffixes)] <- paste0(":",
#                                                abbreviate(
#                                                  suffixes[!util_empty(suffixes)],
#                                                  minlength = 3
 #                                               )
 #     )


      # create a new column with indicator_metric and suffixes
      # if present (excluding MSG and cAT indicators)
      result_full <- dplyr::mutate(result_full,
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
      result_err_msg <- result_full[startsWith(as.character(result_full$indicator_metric),
                                          "MSG_"),
                           c(VAR_NAMES, "call_names",
                             "indicator_metric_suff",
                             "class"),
                           drop = FALSE]
      # modify the class to be "err_msg"
      result_err_msg <- dplyr::rename(result_err_msg, err_msg = "class")

      # Select everything starting with CAT (Error category)
      result_err_cat <- result_full[startsWith(as.character(result_full$indicator_metric),
                                          "CAT_"),
                                       c(VAR_NAMES, "call_names",
                                         "indicator_metric_suff",
                                          "class"),
                                       drop = FALSE]
      # Rename the class for CAT errors as "err_cat"
      result_err_cat <- dplyr::rename(result_err_cat, err_cat = "class")

      # Remove all error messages (CAT and MSG) and obtain an object with actual
      # results (metrics) only
      result_full <- result_full[!startsWith(as.character(result_full$indicator_metric_suff),
                                   "CAT_"), , drop = FALSE]
      result_full <- result_full[!startsWith(as.character(result_full$indicator_metric_suff),
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



      # Sort results by class (decreasing), so that critical output are on top
#      result_full <- result_full[order(
#        result_full$class, decreasing = TRUE), ,
#        FALSE]

      # TO IGNORE, sort by var_names
#      result_full <- result_full[util_order_by_order(result_full$VAR_NAMES,
#                                           ordered_var_names), ,
#        FALSE]
      # TO IGNORE, sort by call_names
#      result_full <- result_full[util_order_by_order(result_full$call_names,
#                                           ordered_var_names), ,
#                       FALSE]



      #===== prepare the object with the error messages =======

      # Remove empty rows and leave only rows with actual error messages
      result_err_msg <- result_err_msg[!util_empty(result_err_msg$err_msg), ,
                                       drop = FALSE]
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

#      result_err_msg <- util_cast(result_err_msg,
#                                         VAR_NAMES + call_names ~
#                                           indicator_metric_suff)





      # Robustness: in case of empty cells replace them with NAs
      result_err_msg[util_empty(result_err_msg)] <- NA

      # prepare text of error messages for html
#      result_err_msg$MSG_anamat <- htmltools::htmlEscape(result_err_msg$MSG_anamat, attribute = FALSE)
 #     result_err_msg$MSG_applicability <- htmltools::htmlEscape(result_err_msg$MSG_applicability, attribute = FALSE)
 #     result_err_msg$MSG_error <- htmltools::htmlEscape(result_err_msg$MSG_error, attribute = FALSE)
 #     result_err_msg$MSG_indicator_or_descriptor <- htmltools::htmlEscape(result_err_msg$MSG_indicator_or_descriptor, attribute = FALSE)

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
      result_err_msg <- dplyr::mutate(result_err_msg,
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
      if(length(colnames(result_err_msg[, !names(result_err_msg) %in%
                                        c("VAR_NAMES",
                                          "call_names"), drop = FALSE]))>1){
      result_err_msg$All_err_msg <-
        apply(result_err_msg[, !names(result_err_msg) %in%
                       c("VAR_NAMES", "call_names"), drop = FALSE],
                        1, function(x) paste(na.omit(x), collapse = "\n"))
      } else if (length(colnames(result_err_msg[, !names(result_err_msg) %in%
                                                c("VAR_NAMES", "call_names"),
                                                drop = FALSE]))==1) {
        result_err_msg$All_err_msg <-
          paste(result_err_msg[, !names(result_err_msg) %in%
                      c("VAR_NAMES", "call_names"), drop = FALSE])
      } else if (length(colnames(result_err_msg[, !names(result_err_msg) %in%
                                                c("VAR_NAMES", "call_names"),
                                                drop = FALSE]))==0) {
        result_err_msg$All_err_msg <- rep(paste(""), nrow(result_err_msg))
      }
      # Delete 4 single original columns of error messages
#      result_err_msg <- result_err_msg[, -which(names(result_err_msg) %in%
#                                                  c("MSG_anamat",
#                                                    "MSG_applicability",
#                                                    "MSG_error")),
#                                           drop = FALSE]




      #===== prepare the object with the categories of errors =======

      # Remove empty rows and leave only rows with actual error messages
      result_err_cat <- result_err_cat[!util_empty(result_err_cat$err_cat), ,
                                       drop = FALSE]
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

#      result_err_cat <- util_cast(result_err_cat,
#                                      VAR_NAMES + call_names ~
#                                        indicator_metric_suff)



      # Robustness: in case of empty cells replace them with NAs
#      result_err_cat[util_empty(result_err_cat)] <- NA

      # Add column other_error_stop5 - logic T/F; if T unexpected error has occurred
      result_err_cat$other_error_stop5 <- result_err_cat$CAT_error == 5

      # Add column is_indicator - logic T/F; if F is not an indicator
      result_err_cat$is_indicator <-
        result_err_cat$CAT_indicator_or_descriptor == 1

      # Add column expected_result - logic T/F; if T we expect a result (all metadata
      # are there and there are no applicability problems)
 #     if("CAT_anamat" %in% colnames(result_err_cat) & "CAT_applicability" %in% colnames(result_err_cat)) {
 #     result_err_cat$expected_result <- result_err_cat$CAT_applicability != 5 &
 #                                       result_err_cat$CAT_anamat != 5
 #     }


      #===== Merge the 3 object in one dataframe =======

      # Merge results and errors in one object (errors become 4 extra columns)
      result_full <- merge(
        result_full,
        result_err_msg,
        by = c(VAR_NAMES, "call_names"), all = TRUE)

      result_full <- merge(
        result_full,
        result_err_cat,
        by = c(VAR_NAMES, "call_names"), all = TRUE)


      #===== Modify the obtained dataframe =======

      # Sort result_full by indicator_metrics
      result_full <- result_full[order(result_full[["indicator_metric"]]), ,
                                 drop = FALSE]

      # In preparation for later (for wide format), create a new tables with names
#      labels_of_var_names_in_report <- setNames(
#        meta_data[[label_col]],
 #       nm = meta_data[[VAR_NAMES]]
#      )

      # prepare order_of --maybe not necessary?
#      order_of <- suppressWarnings(setNames(as.integer(gsub("^cat", "",
#                  util_as_cat(names(colors)))), nm = names(colors)))
      # prepare labels and colors containing the classes information from the
      # grading_formats files --maybe not necessary?
#      labels <- util_get_labels_grading_class()
#      colors <- util_get_colors()
#      names(labels) <- paste0("cat", names(labels))
#     names(colors) <- paste0("cat", names(colors))

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

      # make labels and colors as lists (html tool compatibility)
#      labels <- as.list(labels)
#      colors <- as.list(colors)

      # specify colours in the case where I do not have a category or
      # there is no classification available
#      labels[["catNA"]] <- htmltools::HTML("\u00A0") # this is nbsp, but it reads " ", even in the HTML source code
#      labels[["NA"]] <- htmltools::HTML("\u00A0")
#      colors[["catNA"]] <- "#ffffff00" # transparent / white
#      colors[["NA"]] <- "#ffffff00" # transparent / white
      # create a new empty column for html content
#      result_full$html <- htmltools::HTML("")


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

      #
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
   #   result_full$class_color <- colors[paste(result_full$class_cat)]

    #  result_full <- within(result_full, class_color[is_indicator & not_applicable] <- "#ffffff")
   #  result_full <- within(result_full, class_color[is_indicator & !meta_data_sufficient] <- "#444444")
    #  result_full <- within(result_full, class_color[other_error_stop5] <- "#aaaaaa")
    #  result_full <- within(result_full, class_color[!is_indicator] <- "#ffffff")

      # Add a column to determine the brightness of the text
      # (black/white depending on the background)
      result_full$fg_color <- vapply(result_full$class_color,
                                     FUN = util_get_fg_color,
                                     FUN.VALUE = character(1))




      # create a column title_text - containing the title for the hover text
      result_full$title <-
        vapply(unique(result_full$call_names),
               util_alias2caption,
               FUN.VALUE= character(1))[result_full$call_names]

      result_full <- result_full %>%
        dplyr::mutate(title_text = paste0(get("title"), " of ", get("LABEL")))

#      result_full <- result_full[, -which(names(result_full) == c("title")),
#                                 drop = FALSE]
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
                      paste0(util_translate_indicator_metrics(result_full[["indicator_metric"]])),
                      NA))

      result_full <- dplyr::mutate(result_full,
                                   speaking_name_indic_metric = ifelse(
                                     !util_empty(result_full[["suffixes"]]),
                                     paste0(result_full[["speaking_name_indic_metric"]],
                                            " ", result_full[["suffixes"]]),
                                     result_full[["speaking_name_indic_metric"]]
                                   ))

      result_full <- dplyr::mutate(result_full,
                                   values_readable = ifelse(
                                     !util_empty(result_full[["class"]]),
                                     paste0(result_full[["speaking_name_indic_metric"]],
                                            " = ", result_full[["value"]],
                                            ": ", result_full[["class"]],
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




      # create the link_text --- removed and used directly value instead
#      result_full$link_text <- result_full$value

#      result_full<- dplyr::mutate(result_full,
#                                  link_text = ifelse(islab != "Not classified",
#                                                paste0("<h5>",  islab , "</h5>",value),
#                                                paste0("&nbsp;"))) #HTML entity


#      result_full$ln <- util_generate_anchor_link(result_full$LABEL,  #NOT WORKING
 #                                                 result_full$call_names,
 #                                                 title =
 #                                                   htmltools::HTML(as.character(result_full$link_text)))


      # create a link
      result_full$ln <- apply(result_full[, c(LABEL, "call_names",
                                              "value", "fg_color"),
                                          drop=FALSE], 1, FUN = function(x) {
        link <- util_generate_anchor_link(x[[1]],
                                          x[["call_names"]],
                                          title =
                                            htmltools::HTML(as.character(x[["value"]])))
        link$attribs$style <- c(link$attribs$style,
                                "text-decoration:none;display:block;")
        link$attribs$style <- paste0(link$attribs$style,
                                     sprintf("color:%s;", x[["fg_color"]]))
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

#      result_full$href <- htmltools::tagGetAttribute(result_full$ln,
#                                         attr = "href")
#
#      ln$attribs$href <- NULL



      # for all rows is_indicator == FALSE append something to All_err_msg
      result_full <- dplyr::mutate(result_full,
                                  All_err_msg = ifelse(!get("is_indicator"),
                                  paste0(get("All_err_msg"), " ",
                                         '<span class="dataquieR-error-message"> ',
                                         get("function_name"),
                                         " is a descriptor, only.",
                                         "</span>"),
                                  get("All_err_msg")))

#      if (!result_full$is_indicator) {  #TODO: to fix  #not sure how to do this
#        result_full$ms <- c(result_full$All_err_msg, paste(sQuote(result_full$function_name), "is a descriptor, only."))
#      }


      # create hover addition containing error messages
###      result_full$All_err_msg <- htmltools::htmlEscape(result_full$All_err_msg, attribute = FALSE)
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

      # create the complete href sentence to add after in the html text
      result_full$href_sentence <- sprintf('"if (window.hasOwnProperty(&quot;dq_report2&quot;) &amp;&amp; window.dq_report2 &amp;&amp; window.location != &quot;%s&quot;) { if (all_ids.all_ids.includes(&quot;%s&quot;)) { window.location = &quot;%s&quot; } else { window.alert(&quot;No result avaialable&quot;) } }"',
                                          result_full$href,
                                          result_full$href, result_full$href)
#     result_full$href_sentence <-  htmltools::htmlEscape(result_full$href_sentence,
#                                                         attribute = TRUE)

      # create a column with the final html text
      try(result_full$html <- apply(result_full[, c("href_sentence", "class_color",
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
                               " sort=\"", sort_value, "\"",
                               " filter=\"", filter,"\"",
                               " title=\"", x[["text_complete_hover"]],"\"> ",
                               ln, "\n</pre>")
        as.character(text_for_html)
      }))



#      try(result_full$celltext <- paste0(as.character(  # computing text to be displayed
#        htmltools::tagList(
          # htmltools::p(cn),
          # htmltools::p(vn)
#          htmltools::pre(
#            onclick = sprintf('if (window.hasOwnProperty("dq_report2") && window.dq_report2 && window.location != "%s") { if (all_ids.all_ids.includes("%s")) { window.location = "%s" } else { window.alert("No result avaialable") } }', result_full$href, result_full$href, result_full$href),
#            style = sprintf("height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;", result_full$class_color),
#            sort = result_full$o,
#            filter = result_full$f,
#            title = result_full$text_complete_hover,
#            title = htmltools::tagList(result_full$hover_text,
#                                       htmltools::tags$br(),
#                                       htmltools::HTML(result_full$hover)),
#            result_full$ln))
#      ), collapse = "\n"))
#      result_full$celltext
#    }   # TODO: where this was opened??




      # group by indicator_metric or not
      # For the inner loop, define im_list as a  vector will all metrics
      # (in case it is grouped by indicator_metric, so the loop iterated over
      # each metric) or as a list with just one
      # element represented by a vector of all metrics, in case it is not
      #  grouped (the loops only iterates once)
#      if ("indicator_metric" %in% grouped_by) {
#        im_list <- unique(result$indicator_metric)
#      } else {
#       im_list <- list(unique(result$indicator_metric))
#      }

      # Loop iterating over all cells of the finale matrix:
      # over call_names, var_names and inner loop over indicator_metrics
#      for (cn in unique(result$call_names)) {
#        for (vn in unique(result$VAR_NAMES)) {
#          for (im in im_list) {
            # obtain a subtable that corresponds to a cell of the matrix (.res)
#            .res <- .result0[.result0$call_names == cn &  #for each cell it
#                                            # extract all results: call_names,
#                               .result0$VAR_NAMES == vn &  # var_names,
#                               ((.result0$indicator_metric %in% im) | # and metrics.
#                                  startsWith(as.character(  # omit error messages
#                                    .result0$indicator_metric),
#                                             "CAT_") |
#                                  startsWith(as.character(
#                                    .result0$indicator_metric),
#                                             "MSG_")
#                               ), , FALSE]
            # create the entry for the current cell of the matrix in html
#            result[result$VAR_NAMES == vn &
#                     result$call_names == cn &
#                     result$indicator_metric %in% im, "html"] <-
#              util_get_html_cell_for_result(
#                vn = vn,
#                cn = cn,
#                result = .res,
#                labels_of_var_names_in_report = labels_of_var_names_in_report,
#                colors = colors,
#                order_of = order_of,
#                filter_of = labels,
#                labels = labels
#              )
#          }
#        }
#      }
      ####



      # result2 <- reshape::cast(result,
      #                          rlang::new_formula(
      #                            rlang::sym(VAR_NAMES),
      #                            do.call(rlang::call2, c(list("+"),
      #                                                    rlang::syms(grouped_by)),
      #                                    quote = TRUE)
      #                          ),
      #                          value = "class",
      #                          fun.aggregate = max)



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
          result_full$code <- paste(result_full$VAR_NAMES, result_full$call_names)


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
      List_other_indicators <- result_full[ , c("VAR_NAMES", "call_names", "code",
                                                "indicator_metric",
                                                "indicator_metric_suff",
                                                "suffixes", "class",
                                                "values_readable"), drop = FALSE]
      # TO remove strong tag : remove all rows without a result (class = NA)
      List_other_indicators <- dplyr::filter(List_other_indicators,
                                             get("values_readable") !=
                                               "No results available")

      # TO remove strong tag: keep only a part of the string
        # First create a temporary data frame with the code and the worst class
      class_worst_result <- result_call_name[, c("code", "class"), drop = FALSE]
      class_worst_result <-
        class_worst_result[!util_empty(class_worst_result$class), ,drop = FALSE]
      colnames(class_worst_result)[colnames(class_worst_result) == "class"] =
        "class_worst_result"
       # Then merge data frames to add a column containing the worst class in the
       # list of other indicators
      List_other_indicators <- merge(List_other_indicators, class_worst_result,
                                     by = "code",
                                     all.x = TRUE)
      rm(class_worst_result)

       # Finally remove the strong tag only if the class is different from
       # the worst class
      List_other_indicators <-
        dplyr::mutate(List_other_indicators,
               values_readable = ifelse(class ==
                                        get("class_worst_result") |
                                          is.na(get("class_worst_result")),
                                        get("values_readable"),
                                        gsub('<strong> *(.*)<.*',
                                             "\\1",
                                             x = List_other_indicators$values_readable,
                                             perl = TRUE)))
      # translated call_names to something speaking
#      List_other_indicators$call_names_friendly <- vapply(unique(List_other_indicators$call_names),
#                                  util_alias2caption,FUN.VALUE= character(1))[List_other_indicators$call_names]

      #append the call name -- translated to something speaking
#      List_other_indicators$values_readable <- paste0 (List_other_indicators$values_readable, " - ", List_other_indicators$call_names_friendly)

      # create a row with the content of multiple rows creating a bullet list - if there are other rows
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
      List_other_errors <- result_full[ , c("VAR_NAMES", "call_names", "code",
                                                "indicator_metric", "indicator_metric_suff",
                                            "suffixes",  "class",
                                                "All_err_msg"), drop = FALSE]
      # remove rows without an error message
      List_other_errors <-
        List_other_errors[!is.na(List_other_errors$All_err_msg), , drop = FALSE]
      # keep only non-repeated error messages for VAR_NAMES and call_names

      List_other_errors <- List_other_errors[, c("code", "All_err_msg"),
                                             drop = FALSE]
      List_other_errors <- unique(List_other_errors[ , c("code", "All_err_msg"),
                                                     drop = FALSE])

      # remove error messages matching the error message of the indicator with the
      # worst result
      # create a temporary dataframe
      errors_in_result_call_name <-
        result_call_name[ , c("code", "All_err_msg"), drop = FALSE]

      # return rows of List_other_errors that do not have a match in errors_in_result_call_name
      List_other_errors <- suppressMessages(dplyr::anti_join(List_other_errors,
                                            errors_in_result_call_name, by = NULL))
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
      result_call_name <- dplyr::mutate(result_call_name,
                                   hover =
                                     ifelse(!util_empty(get("All_err_msg_complete")),
                                             paste0("<br>","Messages: ", "<br>",
                                                    "\n<ul>\n",
                                                    get("All_err_msg_complete"),
                                                   "\n</ul>\n"),
                                             paste0("")))
      # remove non useful columns
      result_call_name <-
        result_call_name[,-which(names(result_call_name) %in% c("All_err_msg.x",
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
      }  else if (grouped_by == "indicator_metric" ){

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
      try(result_call_name$html <- apply(result_call_name[, c("href_sentence",
                                                              "class_color",
                                                              "o", "f",
                                                              "text_complete_hover",
                                                              "ln"),
                                                drop=FALSE], 1,
                                    FUN = function(x) {
                                      ln <- x[["ln"]]
                                      filter <- x[["f"]]
                                      sort_value <- x[["o"]]
                                      #  x <- htmltools::htmlEscape(x, attribute = TRUE)
                                      text_for_html <-
                                        paste0("<pre ",
                                               "onclick=",
                                               x[["href_sentence"]],
                                               " style=",
                                               sprintf('"height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;"', x[["class_color"]]),
                                               " sort=\"", sort_value, "\"",
                                               " filter=\"", filter,"\"",
                                               " title=\"", x[["text_complete_hover"]],"\"> ",
                                               ln, "\n</pre>")
                                      as.character(text_for_html)
                                    }))


      result<- result_call_name
      }


      # remove cases without indicator metric
      if("indicator_metric"%in% grouped_by){
        result<- result[!util_empty(result$indicator_metric), , drop = FALSE]
      }


      #rename value as category and html as value
      result1 <- dplyr::rename(result, category = value, value = html)




      # to have the column of the resulting matrix separate by observer/device
      # use the indicator_metric plus the suffix
 #     if(.cl == "indicator_metric"){
#        .cl <- "indicator_metric_suff"
#      }


      # ==== create column of the final table in the case call_names is in the argument
      if("call_names" %in% grouped_by) {

        if (length(grouped_by) > 1) {
          # put in .cl:  call_names + indicator_metric
#          .cl <- do.call(rlang::call2, c(list("+"),
#                                         rlang::syms(grouped_by)),
#                         quote = TRUE)
          # to wide format (create a table with a column for each call_names + indicator_metric or
          # only the selected option for grouped_by)
          #      result2 <- util_cast(result1,
          #                             rlang::new_formula(
          #                              rlang::sym(VAR_NAMES),
          #                              .cl
          #                              ))
          result2 <- result1[, c("VAR_NAMES", "call_names", "indicator_metric", "value"), drop = FALSE]
          result2$cl <- paste0(result2$call_names, "_", result2$indicator_metric)

          result2 <- result2[, c("VAR_NAMES","cl", "value"), drop = FALSE]
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
          # put in .cl the option selected in the argument grouped_by
 #         .cl <- rlang::sym(grouped_by)
          # to wide format (create a table with a column for each call_names + indicator_metric or
          # only the selected option for grouped_by)
          #      result2 <- util_cast(result1,
          #                             rlang::new_formula(
          #                              rlang::sym(VAR_NAMES),
          #                              .cl
          #                              ))
          result2 <- result1[, c("VAR_NAMES", "call_names", "indicator_metric", "value"), drop = FALSE]
          result2 <- dplyr::select(result2, "VAR_NAMES", paste0(grouped_by), "value")
          names(result2) <- c("VAR_NAMES", "cl", "value")
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
                    indicator_metric = unique(result$indicator_metric),
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


      mapping_of_cns$fname2 <- apply(mapping_of_cns[, "call_names", drop = FALSE],
                                     1, FUN = function(x) {
                                        util_map_by_largest_prefix(x,
                                        haystack = util_all_ind_functions())
                                      })

        suffixes <- mapply(SIMPLIFY = FALSE, cn = mapping_of_cns[["call_names"]],
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
                        to = "dq_report2_short_title",
                        from = "function_R",
                        ifnotfound = util_abbreviate(mapping_of_cns$fname))
      # Prepare the suffixes
      suffixes <- gsub("_", " ", vapply(suffixes, as.character,
                                        FUN.VALUE = character(1)))
      suffixes[!util_empty(suffixes)] <- paste0(":",
                                                abbreviate(
                                                 suffixes[!util_empty(suffixes)],
                                                  minlength = 3
                                                ))

      # Prepare the indicator names
      mapping_of_cns$imtitle <-
        util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
                                         long = FALSE, short = FALSE)
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

 #     } else {

      # remove prefix from indicator name
 #     mapping_of_cns$indicators_no_prefix <-  gsub(".*?_(.*)","\\1",
 #                                                  vapply(mapping_of_cns$colname,
#                                                          as.character,
 #                                                         FUN.VALUE = character(1)))
      # import the dqi informations
#      dqi_info <- util_get_concept_info("dqi")
      # use the Definition from dqi to create the description of the hover text
#      mapping_of_cns$coldesc <- util_map_labels(mapping_of_cns$indicators_no_prefix,
#                                                dqi_info,
#                                                to = "Definition",
#                                                from = "abbreviation")
#      }




      # order the column based on the object ordered_call_names
        # First create an order based on call names e.g., 3
      mapping_of_cns$order_call <- setNames(seq(length(this$ordered_call_names)),
                                       nm = this$ordered_call_names)[
                                         mapping_of_cns[["call_names"]]]
        # Then create an order based on indicator metric e.g., 9
      mapping_of_cns$order_slot <- rank(mapping_of_cns[["indicator_metric"]])
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
      colnames(result2)[colnames(result2) %in% mapping_of_cns$colname] <-
        setNames(mapping_of_cns$coltitle,
                                    nm = mapping_of_cns$colname)[
                                      colnames(result2)[colnames(result2) %in%
                                                          mapping_of_cns$colname]]

      # result2[, setdiff(colnames(result2), grouped_by)] <-
      #   lapply(result2[, setdiff(colnames(result2), grouped_by)], as.logical)
      #
      # result2$class <- colnames(result2)[-1][apply(
      #   result2[, setdiff(colnames(result2), grouped_by)], 1, which.max)]
      #
      # result2 <- result2[, c(grouped_by, "class"), FALSE]





      # creation of the final matrix in case indicator_metric is selected ===
       }    else if (grouped_by == "indicator_metric" ){

         # to wide format (create a table with a column for each call_names + indicator_metric or
         # only the selected option for grouped_by)
#         result2 <- util_cast(result1,
#                                  rlang::new_formula(
#                                    rlang::sym(VAR_NAMES),
#                                    rlang::sym("indicator_metric_suff")
#                                  ))

         result2 <- result1[, c("VAR_NAMES",
                                "indicator_metric_suff",
                                "value"), drop = FALSE]
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
         mapping_of_cns$function_name2 <- apply(mapping_of_cns[, "call_names",
                                                               drop = FALSE],
                                             1, FUN = function(x) {
                                               util_map_by_largest_prefix(x,
                                                haystack = util_all_ind_functions())
                                             })

         # add a column with suffixes
         mapping_of_cns <- dplyr::mutate(mapping_of_cns,
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
#         mapping_of_cns$suffixes[!util_empty(mapping_of_cns$suffixes)] <- paste0(":",
#                                                   abbreviate(
#                                                     mapping_of_cns$suffixes[!util_empty(mapping_of_cns$suffixes)],
#                                                     minlength = 3
#                                                   ))

         # create a new column with indicator_metric and suffixes if present
         mapping_of_cns <- dplyr::mutate(mapping_of_cns,
                                     indicator_metric_suff =
                                       ifelse(!util_empty(get("suffixes")),
                                              paste0(get("indicator_metric"),"_",
                                                     get("suffixes")),
                                              get("indicator_metric")))

         # append a new column with function names
         mapping_of_cns$fname <- function_name_mapping[mapping_of_cns[["call_names"]]];
         # create a new column called colname containing the combination
         # selected for grouped-by. Example:
         # if only indicator_metric was selected, the column will contains only indicator_metrics
         mapping_of_cns$colname <-
           paste0(mapping_of_cns[["indicator_metric"]])
         mapping_of_cns$colname_suff <-
           paste0(mapping_of_cns[["indicator_metric_suff"]])



         # get a table with for each function, the short-name, implementations and so on
#         acronyms_tab <- util_get_concept_info("implementations")


         # === Define the title of the matrix columns
         # First select the short name of the function from "implementations" in
         # dq control file
#        mapping_of_cns$acronyms <-
#           util_map_labels(mapping_of_cns$fname2,
#                           acronyms_tab,
#                           to = "dq_report2_short_title",
 #                          from = "function_R",
 #                          ifnotfound = util_abbreviate(mapping_of_cns$fname))


         # Prepare the indicator names
         mapping_of_cns$imtitle <-
           util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
                                 long = FALSE, short = FALSE)
         mapping_of_cns$imtitle[util_empty(mapping_of_cns$imtitle)] <-
           mapping_of_cns[["indicator_metric"]][util_empty(mapping_of_cns$imtitle)]

#         mapping_of_cns$imtitle_short <- util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]], long = FALSE, short = TRUE)
 #        mapping_of_cns$imtitle_short[util_empty(mapping_of_cns$imtitle_short)] <- mapping_of_cns[["indicator_metric"]][util_empty(mapping_of_cns$imtitle_short)]

         # Create the actual column names for the matrix in column "coltitle"
         mapping_of_cns$coltitle <- paste0(mapping_of_cns$imtitle, " ",
                                           mapping_of_cns$suffixes)





         # Definition of the description of the hover text of column titles in the final matrix
           # remove prefix from indicator name
           mapping_of_cns$indicators_no_prefix <- gsub(".*?_(.*)","\\1",
                                                        vapply(mapping_of_cns$colname,
                                                               as.character,
                                                               FUN.VALUE = character(1)))
           # import the dqi informations
           dqi_info <- util_get_concept_info("dqi")
           # use the Definition from dqi to create the description of the hover text
           mapping_of_cns$coldesc <- util_map_labels(mapping_of_cns$indicators_no_prefix,
                                                     dqi_info,
                                                     to = "Definition",
                                                     from = "abbreviation")


         # order the columns
         # create an order based on indicator metric e.g., 9
         mapping_of_cns$order <- rank(mapping_of_cns[["indicator_metric_suff"]])

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

      # xx util_order_by_order(colnames(result2), this$ordered_call_names)

      # result2$class <- paste0(
      #   "<span style=\"background-color: ",
      #   gg_colors[result2$class],
      #   "dd;", # a little bit transparent
      #   "display:block;overflow:auto;", # https://stackoverflow.com/a/16961177
      #   "\">",
      #   result2$class,
      #   "</span>"
      # )

      # xx <- strsplit(colnames(result2), "_", fixed = TRUE)


      tb <- util_html_table(result2,
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
                            output_format = "HTML"
      )

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
        htmltools::tags$script('$(function() { if (!window.hasOwnProperty("dq_report2") || !window.dq_report2) { $("a").attr("href", "javascript:alert(\\"links work in dq_report2 reports, only.\\")") }} )')
      ))
    }

  util_attach_attr(res,
                   repsum_wide = util_attach_attr(result2,
                                                  label_col = label_col),
                   this = this)
})}
