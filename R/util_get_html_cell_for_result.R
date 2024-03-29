#' Return a html summary table cell for a result
#'
#' @param result [data.frame] long format table with report results
#' @param vn row name for links inside a `dq_report2` report
#' @param cn column name for links inside a `dq_report2` report
#' @param ... formatting objects prepared outside.
#'
#' @return [character]`(1)` `html` result for a `DT` cell#'
#' @family summary_functions
#' @concept reporting
#' @keywords internal
util_get_html_cell_for_result <- function(vn, cn, result, ...) {
  ellipsis_arguments <- list(...)
  labels_of_var_names_in_report <- NULL
  colors <- NULL
  order_of <- NULL
  filter_of <- NULL
  call_names <- NULL
  indicator_metric <- NULL

  for (n in names(ellipsis_arguments)) {
    assign(n, ellipsis_arguments[[n]])
  }


  # Sort result by indicator_metrics
  result <- result[order(result[["indicator_metric"]]), , FALSE]

  # extract the error messages from result
  ms <- setNames(result[
    result$VAR_NAMES == vn &
      result$call_names == cn &
      startsWith(as.character(result$indicator_metric), "MSG_"),
    c("class"), TRUE], nm =
      result[
        result$VAR_NAMES == vn &
          result$call_names == cn &
          startsWith(as.character(result$indicator_metric), "MSG_"),
        c("indicator_metric"), TRUE])
  # extract the results that are not error messages (remove metrics starting
  # with MSG or CAT)
  # Create a vector with T = result F = error message
  issue_res <-
    result$VAR_NAMES == vn &
    result$call_names == cn &
    !startsWith(as.character(result$indicator_metric), "MSG_") &
    !startsWith(as.character(result$indicator_metric), "CAT_")

  # Extract the class of the metric and translate it in category
  iscats <- util_as_cat(result[issue_res, "class", drop = TRUE]) # this may be a problem, if there are != 5 classes, but now, we have util_standardise_ordinal()
  # IGNORE: we always have 5 classes, it looks for the no. classes
  iscats_n_classes <- result[issue_res, "n_classes", drop = TRUE]
  # Extract the values of the metric
  isvalues <- result[issue_res, "value", drop = TRUE]
  # check if the indicator metric has a value
  have_value <- !is.na(result[issue_res, "value", drop = TRUE])
  # check which metric we have for the current result
  ismettrics <- result[issue_res, "indicator_metric", drop = TRUE]
  # find the function name
  function_name <- unique(result[result$call_names == cn,
                                 "function_name",
                                 drop = TRUE])
  # there should be only one function name, if not stop and give an error
  util_stop_if_not("Internal error: More than one function for one call?" =
                     length(function_name) <= 1)
  util_stop_if_not(
"Internal error: No result one call should be prevented by summary, already?" =
                     length(function_name) == 1)
  # extract the class for the result (maybe more than one)
  cl <- result[
    result$VAR_NAMES == vn &
      result$call_names == cn &
      !startsWith(as.character(result$indicator_metric), "MSG_"),
    c("class"), TRUE]
  # IGNORE: it is always 5. Compute the maximum no. categories
  max_cat <- suppressWarnings(max(result[
    result$VAR_NAMES == vn &
    result$call_names == cn &
      !startsWith(as.character(result$indicator_metric), "MSG_"),
      "n_classes", TRUE]))

  # IGNORE: always 5 categories. This code it is used for normalization in case
  # of different number of categories
  cl_cmp_vals <- vapply(mapply(SIMPLIFY = FALSE,
                        i = as.integer(iscats),
                        n = iscats_n_classes,
                        FUN =
                          function(i, n) {
                            if (!is.finite(i) ||
                                !is.finite(n) ||
                                !is.finite(max_cat)) {
                              return(-Inf)
                            }
                            util_standardise_ordinal_codes(i, n, max_cat)
                          }), identity, FUN.VALUE = numeric(1))

  # cl <- suppressWarnings(max(as.numeric(cl), na.rm = TRUE))

   cl <- suppressWarnings(
     # Obtain the maximum value for category
    head(util_as_cat(c(max(cl_cmp_vals, na.rm = TRUE), max_cat)), 1))
  if (!is.finite(cl)) {
    # cl <- util_as_cat(max_cat)
    cl <- util_as_cat(NA)  # use NA if no category is available
    for_classes <- rep(FALSE, length(iscats))
  } else {
    for_classes <- as.numeric(cl_cmp_vals) == as.numeric(sub("^cat", "", cl))

  }
  cl <- as.character(cl)
  iscat <- cl # suppressWarnings(max(iscats, na.rm = TRUE))

  # check if there is a result, and it is not NA
  have_result <- length(result[issue_res, "value", drop = TRUE]) > 0 &&
    any(!is.na(result[issue_res, "value", drop = TRUE]))

  # if a result is present
  if (have_result) {
    values_short <- isvalues[have_value]
    #make it more readable
    values <- paste(util_translate_indicator_metrics(ismettrics[have_value]),
          "=", isvalues[have_value])
    # values_short <- paste0(values_short,
    #                  vapply(iscats[have_value],
    #                         function(v) {
    #                           if (is.finite(v)) {
    #                             paste0(": ", labels[as.character(v)])
    #                           } else {
    #                             ""
    #                           }
    #                         },
    #                         FUN.VALUE = character(1))
    # )

    # Append categories for each value (spelled out)
    values <- paste0(values,
                     vapply(iscats[have_value],
           function(v) {
             if (is.finite(v)) {
               paste0(": ", labels[as.character(v)])
             } else {
               ""
             }
           },
           FUN.VALUE = character(1))
    )
    # define which should be in bold
    make_bold <- which(for_classes[have_value])
    # Only variables responsible for classification are in bold
    values[make_bold] <-
      paste("<strong>", values[make_bold], "</strong>")

    # If there is anything in for_classes then we have an overall classification
    # and we store it inside islab, otherwise we use "Not classified"
    if (any(for_classes, na.rm = TRUE)) {
      islab <- labels[[iscat]]
    } else {
      islab <- "Not classified"
    }

    #Here start the hover box, it started creating a taglist
    hover_text <- htmltools::tagList(
        # create a tag for header of level 4 with nice caption
        htmltools::tags$h4(util_alias2caption(cn, long = TRUE), "of", labels_of_var_names_in_report[[vn]]),
        # create a tag for classification
        htmltools::tags$h5(islab),
        # create an unordered list (bullet points) with the different values
        # computed before
        htmltools::tags$ul(htmltools::HTML(vapply(lapply(lapply(values, htmltools::HTML), htmltools::tags$li), as.character,
                               FUN.VALUE = character(1))))
      )
    # provide the worst classification values (the first), or value-short if
    # there is no classifcation
    link_text <- htmltools::tagList(
      #htmltools::tags$h5(islab),
      htmltools::HTML(head(c(values_short[make_bold], values_short), 1))
    )
  } else { #In case we do not have any classification, no results, no metrics
    islab <- "Not available"
    iscat <- NA
    hover_text <- htmltools::tagList(
      htmltools::tags$h4(util_alias2caption(cn, long = TRUE), "of", labels_of_var_names_in_report[[vn]]),
      htmltools::tags$h5("No results available")
    )
    link_text <- htmltools::tagList(
      htmltools::HTML("&nbsp;"))
  }


  # Extracting error messages

  ms <- c(
#    issue = islab,
    ms
  )

  classes <-
    setNames(
      util_as_cat(result[
        result$VAR_NAMES == vn &
          result$call_names == cn &
          !startsWith(as.character(result$indicator_metric), "MSG_"),
        "class", TRUE]),
      nm = result[
        result$VAR_NAMES == vn &
          result$call_names == cn &
          !startsWith(as.character(result$indicator_metric), "MSG_"),
        "indicator_metric", TRUE]
    )
  # if equal 5 an unexpected error has occurred and cause the function to stop
  other_error_stop5 <- all(classes[c("CAT_error")] == cat5)
  # If the value is equal to 1 it is an indicator, otherwise is a descriptor
  is_indicator <- all(classes[c("CAT_indicator_or_descriptor")] == cat1)
  # checks if all metadata are there and there is no sever problem and can not run
  expected_result <- all(classes[c("CAT_applicability",
                                   "CAT_anamat")] != cat5)

  ln <- util_generate_anchor_link(labels_of_var_names_in_report[[vn]],
                                  cn,
                                  title =
                                    htmltools::HTML(as.character(link_text))) # icons[[cl]])

  ln$attribs$style <- c(ln$attribs$style, "text-decoration:none;display:block;")

  href <- htmltools::tagGetAttribute(ln,
                                     attr = "href")

  ln$attribs$href <- NULL

  if (!is_indicator) {
    ms <- c(ms, paste(sQuote(function_name), "is a descriptor, only."))
  }

  if (any(!util_empty(ms))) {
    hover <- paste0("\n<ul>\n",
                    paste0("<li>", ms[!util_empty(ms)], "</li>",
                           collapse = "\n"),
                    "\n</ul>\n")
  } else {
    hover <- ""
  }
  # logic vector checking if the result is reasonably possible
  not_applicable <- classes["CAT_anamat"] != cat1
  # logic vector checking if there are all metadata to compute the indicator, if not 1 something is missing
  meta_data_sufficient <- classes["CAT_applicability"] == cat1
  # if (labels_of_var_names_in_report[[vn]] == "GLOBAL_HEALTH_VAS_0" && cn == "acc_distributions_loc") browser()
  # if (labels_of_var_names_in_report[[vn]] == "SBP_0" && cn == "acc_varcomp_device") browser()
  # if (labels_of_var_names_in_report[[vn]] == "SBP_0" && cn == "acc_varcomp_observer") browser()
  if (!is.na(iscat) && is_indicator) {   #in case it is an indicator with a class
#    if (!(iscat %in% names(colors))) browser()
    color <- colors[[iscat]] # the color is the corresponding color of the category
  } else if (not_applicable && is_indicator) { # in case it make no sense to compute or is not chosen by the used (e.g. end-digits)
    color <- "#ffffff"
  } else if (!meta_data_sufficient && is_indicator) { # there are no enought metadata and it is an indicator
    color <- "#444444"
  } else { # any other error blocks
    if (other_error_stop5 || is_indicator) { # TODO: test: did we crash or is it just a descriptor?
      color <- "#aaaaaa"   # t is an indicator but still no classificaton or no rules for it
    } else {
      color <- "#ffffff" # descriptor
    }
  }
  # determine the brightness of the text (black/white depending on the background)
  fg_color <- util_get_fg_color(color)
  # Additions to the style
  ln$attribs$style <- c(ln$attribs$style, sprintf("color:%s;", fg_color))
  # Initialize the text to display inside the cell - it should be never observed
  celltext <- paste(vn, cn, collapse = "/")
  o <- NA  # Initialize the order
  f <- htmltools::HTML("&nbsp;") #initialize the filters
  try({  # check if there is a color
    if (is.na(cl)) {
      o <- order_of[["catNA"]]
    } else { # if there is a class use the order of the class
      o <- order_of[[cl]]
    }
  }, silent = TRUE)
  try({
    if (is.na(cl)) {
      f <- filter_of[["catNA"]]
    } else {
      f <- filter_of[[cl]]
    }
  }, silent = TRUE)
  try(celltext <- paste0(as.character(  # computing text to be displayed
    htmltools::tagList(
      # htmltools::p(cn),
      # htmltools::p(vn)
      htmltools::pre(
        onclick = sprintf('if (window.hasOwnProperty("dq_report2") && window.dq_report2 && window.location != "%s") { if (all_ids.all_ids.includes("%s")) { window.location = "%s" } else { window.alert("No result avaialable") } }', href, href, href),
        style = sprintf("height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;", color),
        sort = o,
        filter = f,
        title = htmltools::tagList(hover_text,
                                   htmltools::tags$br(),
                                   htmltools::HTML(hover)),
        ln))
  ), collapse = "\n"))
  celltext
}
# function() {
#   util_html_table(summary.dataquieR_resultset2(r))
#   xx <- summary(r)
#   xxx <- util_html_table(xx,
#                   filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
#                   is_matrix_table = TRUE, rotate_headers = TRUE,
#                   meta_data = attr(r, "meta_data"),
#                   label_col = attr(r, "label_col"),
#                   dl_fn = "DQ Report Summary",
#                   output_format = "HTML"
#   )
#   htmltools::browsable(xxx)
# }
