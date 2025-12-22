#' Create a `plotly` pie chart
#'
#' @param data data as returned by `prep_summary_to_classes` but
#'             summarized by one column (currently, we support
#'             `indicator_metric`, `call_names`,
#'              `STUDY_SEGMENT`, and `VAR_NAMES`)
#' @param meta_data [meta_data]
#'
#' @return a `htmltools` compatible object
#'
#' @family summary_functions
#' @export
prep_render_pie_chart_from_summaryclasses_plotly <- function(data, # FIXME: If use_plotly is FALSE?
                                                             meta_data = "item_level") {
  # FIXME: Amend prep_render_pie_chart_from_summaryclasses_ggplot2 to handle this also.
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft("2.1.0.9007",
                              "prep_render_pie_chart_from_summaryclasses_plotly()",
                              "plot.dataquieR_summary()")
  }

  if (!util_ensure_suggested(c("plotly", "htmltools"), err = FALSE)) {
    return(NULL)
  }

  if (nrow(data) == 0) {
    return(htmltools::browsable(htmltools::HTML("")))
  }

  grouped_by <- setdiff(colnames(data), c("class", "value", "percent", "note"))

  util_stop_if_not(length(grouped_by) == 1)

  groups <- unique(data[[grouped_by]])

  util_stop_if_not(length(groups) > 0)

  if (length(groups) > 1) {
    all_pys <- lapply(setNames(nm = groups), function(g) {
      prep_render_pie_chart_from_summaryclasses_plotly(
        data[data[[grouped_by]] == g, , FALSE], meta_data = meta_data)
    })
    # pys <-
    #   do.call(plotly::subplot, lapply(all_pys, plotly::subplot))
    # py <- plotly::layout(pys,
    #                # title = list(text =
    #                #                res$SummaryPlot$patches$annotation$title),
    #                margin = 0.01)
    ncols <- min(2, ceiling(sqrt(length(all_pys))))
    nrows <- ceiling(length(all_pys) / ncols)

    pys_matrix <- htmltools::tags$table(lapply(
      seq_len(nrows),
      function(rw) {
        if ((rw - 1) * ncols + 1 <= length(all_pys)) { # current row needed?
          cur_tr_as_list <- lapply(seq_len(ncols), function(rw, cl) {
            wch <- (rw - 1) * ncols + cl
            if (wch <= length(all_pys)) {
              o <- htmltools::tags$td(all_pys[wch])
            } else {
              o <- htmltools::tags$td()
            }
            o
          }, rw = rw)
          do.call(htmltools::tags$tr, cur_tr_as_list)
        } else { # should never be reached
          NULL
        }
      }
    ))

    py <- pys_matrix

    py <- htmltools::tags$table(py)
    py <- htmltools::browsable(py)
    return(py)
  }

  py_colors <- util_get_colors()
  py_colors["NA"] <- "lightgrey"
  labs <- util_get_labels_grading_class()
  labs["NA"] <- "Not classified"

  # wrap the text of the labs if too long. Max no. characters = 20
  no_char_labs<- vapply(labs, FUN = function(x){
    no_char <- nchar(x)
  }, FUN.VALUE = numeric(1))

  if (any(no_char_labs > 10)) {
    labs<- vapply(labs, FUN = function(x){
      #remove white spaces at beginning or end
      x <- trimws(x)

      #If labs >20 characters, cut it at 20
      if (nchar(x) > 20) {
        x <- substr(x, start = 1, stop = 20)
      }

      sst <- strsplit(x, '')[[1]]  #from https://stackoverflow.com/questions/11619616/how-to-split-a-string-into-substrings-of-a-given-length
      m <- matrix('', nrow=10,
                  ncol=(length(sst)+10-1)%/%10)
      m[seq_along(sst)] <- sst
      x <- apply(m, 2, paste, collapse='')
      rm(sst, m)
      #remove white spaces at beginning or end
      x <- trimws(x)
      x <- paste0(x, collapse = "<br>")
    }, FUN.VALUE = character(1))
  }


  if (all(is.na(data$class))) {
    return(htmltools::browsable(htmltools::HTML("")))
  }

  if (is.factor(data$class)) {
    data$class <- as.integer(gsub("^cat", "", data$class))
  }
  data$class <- factor(data$class,
                       levels = names(py_colors),
                       ordered = TRUE
  )

  data <- data[order(data$class, -data$value, decreasing = TRUE), , FALSE]
  hoverinfo <- "label+percent+value"

  if ("note" %in% colnames(data)) {
    hoverinfo <- paste0(hoverinfo, "+text")
  } else {
    data$note <- ""
  }

  if (length(groups) > 1) { # IDEA: sunburst would maybe work here, if we have a hiearchy in groups, currently, we do not have that.
    # Sunburst:
    # py <- plotly::plot_ly(
    #   ids = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
    #   labels = paste(c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"), 42),
    #   parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
    #   values = c(10, 14, 12, 10, 2, 6, 6, 4, 4),
    #   type = 'sunburst'
    # )
  } else {
    rotation_value <- 90
    #order data frame "data"
    data <- data[order(data$class),]


    py <- plotly::add_pie(plotly::plot_ly(data,
                                          height = 400,
                                          width = 400),
                          sort = FALSE,
                          direction = "clockwise",
                          rotation = rotation_value,
                          labels = labs[
                            paste(data$class)],
                          values = data$value,
                          hovertext = data$note,
                          #                name = "Segment",
                          #                hoverinfo = "label+percent+name",
                          hoverinfo = hoverinfo,
                          textinfo = 'label+percent+value',
                          showlegend = FALSE,
                          marker = list(
                            colors = py_colors[paste(data$class)]))
  }

  if (identical(grouped_by, "indicator_metric")) { # TODO: Hiearchical structure support
    title <- util_translate_indicator_metrics(groups, short = FALSE,
                                              long = FALSE)
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, "call_names")) { # TODO: Hiearchical structure support
    fnms <- util_cll_nm2fkt_nm(groups)
    if (nchar(fnms) < nchar(groups)) {
      suff <- gsub("^.*_", ": ", groups)
      substr(suff, 3, 3) <- toupper(substr(suff, 3, 3))
    } else {
      suff <- ""
    }
    title <- paste0(util_map_labels(fnms,
                                    util_get_concept_info("implementations"),
                                    to = "dq_report2_short_title",
                                    from = "function_R",
                                    ifnotfound = NA_character_), suff)
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character(STUDY_SEGMENT))) { # TODO: Hiearchical structure support
    title <- groups
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character(VAR_NAMES))) {
    util_expect_data_frame(meta_data)
    title <- prep_get_labels(groups,
                             meta_data = meta_data,
                             label_class = "LONG")
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character("function_name"))) {
    title <- vapply(groups, util_alias2caption, long = TRUE,
                    FUN.VALUE = character(1))
    subtitle <- groups
  } else {
    title <- groups
    subtitle <- "percentage of QA classes"
    #subtitle <- NULL
    #    util_error("Unkown grouping by %s", sQuote(grouped_by))
  }

  subtitle <- paste(subtitle, sprintf(" -- %d of %d variables classified",
                                      sum(data$value, na.rm = TRUE),
                                      nrow(meta_data)
  )) # TODO: Maybe, we should not compute this here, but earlier.


  #Define the space on top among the title and the plot conditionally
  angles <- data$value / sum(data$value) * 360
  cs1 <- cumsum(data$value / sum(data$value) * 360) #final limits in case started on top
  cs1 <- cs1 + rotation_value # final limits in case started at rotation_value

  lower_bound <- 345
  upper_bound <- 360
  values_in_range1 <- (cs1 >= lower_bound & cs1 <= upper_bound)
  count_in_range1 <- sum(values_in_range1)

  lower_bound <- 360
  upper_bound <- 375
  values_in_range2 <- (cs1 >= lower_bound & cs1 <= upper_bound)
  count_in_range2 <- sum(values_in_range2)


  #for bottom margin
  lower_bound <- 150
  upper_bound <- 180
  values_in_range3 <- (cs1 >= lower_bound & cs1 <= upper_bound)
  count_in_range3 <- sum(values_in_range3)

  lower_bound <- 180
  upper_bound <- 210
  values_in_range3 <- (cs1 >= lower_bound & cs1 <= upper_bound)
  count_in_range4 <- sum(values_in_range3)


  #Define the values for white spaces around the plot
  value_bottom_conditional <- 50
  value_on_top_conditional <- 50

  if(all(angles >= 60)) {
    #All the text is inside the plot, no need for extra space at top or bottom
    value_on_top_conditional <- 0
    value_bottom_conditional <- 0
  } else {
    if (count_in_range1 >= 2 || count_in_range2 >= 2) {
      value_on_top_conditional <- 170
    } else {
      value_on_top_conditional <- 60
    }

    if (count_in_range3 >= 2 || count_in_range4 >= 2) {
      value_bottom_conditional <- 170
    } else {
      value_bottom_conditional <- 60
    }
  }

  py <- plotly::layout(py,
                       title =
                         list(
                           text =
                             as.character(htmltools::tagList(
                               title,
                               htmltools::tags$sup(subtitle)
                             )),
                           y = 0.95,
                           yref = "container"),
                       autosize = FALSE,  #maybe can cause trouble
                       margin = list(
                         t = value_on_top_conditional,
                         r = 100,
                         l = 100,
                         b = value_bottom_conditional)
  )

  py <- plotly::config(py, displaylogo = FALSE)

  py <- htmltools::span(
    title = groups,
    `data-tippy-always-on` = "true",
    py
  )

  py <- htmltools::browsable(py)
  return(py)


#  py <- plotly::layout(py,
                       #title =
                       #  list(
                       #    text =
                       #      as.character(htmltools::tagList(
                       #        title,
                       #        htmltools::tags$sup(subtitle)
                        #     )),
                        #   y = 0.95,
                        #   yref = "container"),
#                       autosize = FALSE,  #maybe can cause trouble
#                       margin = list(
#                         t = value_on_top_conditional,
#                         r = 100,
#                         l = 100,
#                         b = value_bottom_conditional)
#  )

#  py <-
#    htmltools::div(htmltools::h4(title,
#                                 style = "font-family: sans-serif; margin-bottom: 0; margin-top: 2;"),
#                   htmltools::h5(subtitle,
#                                 style = "font-family: sans-serif; margin-top: 0; margin-bottom: 0; "),
#                   htmltools::div(py,
#                                  style = "align-self: center;margin-top: 0; "),
#                   style = "text-align: center; display: flex; flex-direction: column; align-item: center;",
#                   title = groups,
#                   `data-tippy-always-on` = "true")

#  py <- htmltools::browsable(py)

#  return(py)
}
