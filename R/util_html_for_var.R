#' Create a dynamic single variable page for the report
#'
#' @param report [dataquieR_resultset2] a `dq_report2` report
#' @param cur_var [character] variable name for single variable pages
#' @param use_plot_ly [logical] use `plotly`, if available.
#' @param template [character] template to use for the `dq_report2` report.
#' @param note_meta [character] notes on the metadata for a single variable
#'                              (if needed)
#'
#' @return list of arguments for `append_single_page()` defined locally in
#'   [util_generate_pages_from_report()].
util_html_for_var <- function(report, cur_var, use_plot_ly, template,
                              note_meta = c()) {

  meta_data <- attr(report, "meta_data")
  label_col <- attr(report, "label_col")

  util_stop_if_not(inherits(report, "dataquieR_resultset2"))
  util_stop_if_not(!missing("cur_var") && length(cur_var) == 1 &&
                     !is.na(cur_var) &&
                      is.character(cur_var))

  # find which dimensions are included in the report
  dims_in_rep <-
    unique(vapply(strsplit(colnames(report), "_"), `[[`, 1,
                  FUN.VALUE = character(1)))

  dims_in_rep <- util_sort_by_order(dims_in_rep, names(dims))

  # match the functions to the dimensions
  clls_in_rp <- lapply(setNames(nm = dims_in_rep),
                             function(prefix)
                               colnames(report)[startsWith(colnames(report),
                                                           prefix)])


  svp <- list()

  for (cur_dm in dims_in_rep) {
    # util_message("Dimension %s", cur_dm)
    drop_down <- dims[[cur_dm]] # fetches the name of the content for the first-level drop-down menu
    j <- 0
    m <- length(clls_in_rp[[cur_dm]])
    for (cll in clls_in_rp[[cur_dm]]) { # loop over the functions in each dimension
      fkt <- util_cll_nm2fkt_nm(cll, report)
      all_of_f <- report[cur_var, cll] # extract the single result

     # use results as they are, not combined by util_combine_res. see else branch below for comments on mapply
      output <- do.call(htmltools::tagList, unname(mapply(dqr = all_of_f,
                                                          nm =
                                                            names(all_of_f),
                                                          FUN =
                                                            util_pretty_print,
                                                          MoreArgs =
                                                            list(
                                                              meta_data =
                                                                meta_data,
                                                              label_col =
                                                                label_col,
                                                              use_plot_ly =
                                                                use_plot_ly,
                                                              is_single_var =
                                                                TRUE),
                                                          SIMPLIFY = FALSE)))
      # if (cll == "con_inadmissible_categorical" && cur_var == "SBP_0") browser()
      # if we have some result, then store it in the svp list for the current single variable page
      if (length(output) && !all(vapply(output, is.null, FUN.VALUE = logical(1)))) {
        # title <- .manual$titles[[fkt]]
        title <- util_alias2caption(cll, long = TRUE)
        description <- util_function_description(fkt)

        svp[[cll]] <- htmltools::tagList(
          htmltools::span( # span is requird to make [[2]][[1]]$link on page navi menu creation below work
            htmltools::h4(style = "margin-top: 3em", # add function name as a ~subtitle, add space to prevent plot.lys to overlap
                          htmltools::a(
                            target = "_blank",
                            href = util_online_ref(fkt),
                            title)),
            htmltools::HTML("<!-- "),
            htmltools::h5(paste(unique(c(cll, fkt)),
                                collapse = ": ")),
            htmltools::HTML(" -->"),
            htmltools::p(htmltools::HTML(description))),
          output)
      } else {
        svp[[cll]] <- NULL
      }

    }

  }



  title <- htmltools::h1(cur_var)

  cur_md <- meta_data[meta_data[[label_col]] == cur_var,
                      , FALSE] # subset the metadata for the current variable to show it in the report

  md_info <- htmltools::tagList(
    anchor = util_generate_anchor_tag(cur_var, "meta_data", "variable"),
    link = util_generate_anchor_link(cur_var, "meta_data", "variable"), # these links are moved later
    htmltools::h1("Item-level Metadata"),
    htmltools::div(note_meta),
    util_html_table(data.frame(Name = names(cur_md),
                               Value = unname(t(cur_md))),
                    meta_data = meta_data,
                    label_col = label_col,
                    copy_row_names_to_column = FALSE,
                    dl_fn = paste("Item-level_Metadata_", cur_var),
                    output_format = "HTML") # ignore row names (created by data.frame)
  )

  if (util_ensure_suggested("summarytools",
                            goal =
                            "descriptive summary statistics in the report",
                            err = FALSE) &&
      inherits(attr(report, "summary_stat"), "summarytools") &&
      inherits(attr(report, "summary_descr"), "summarytools")) {

    summary_stat <- attr(report, "summary_stat")

    # The current variable might be missing in summary_stat, if it was only
    # contained in the metadata and could not be found in the study data.
    if (cur_var %in% summary_stat$Variables) {
      summary_stat <- summary_stat[summary_stat$Variables == cur_var, , FALSE]
      summary_stat <- util_html_table(summary_stat, rotate_for_one_row = TRUE,
                                      meta_data = meta_data,
                                      label_col = label_col,
                                      dl_fn = "summary_stat",
                                      output_format = "HTML")

      summary_stat <- htmltools::tagList(
        anchor = util_generate_anchor_tag(cur_var, "Distribution Overview", "variable"),
        link = util_generate_anchor_link(cur_var, "Distribution Overview", "variable"), # these links are moved later
        htmltools::h1("Distribution Overview"),
        summary_stat
      )
    } else {
      summary_stat <- htmltools::p("")
    }

    summary_descr <- attr(report, "summary_descr")

    # descriptive statistics only available for numeric variables
    if (cur_var %in% rownames(summary_descr)) {
      summary_descr <- summary_descr[cur_var, , FALSE]
      summary_descr <- util_html_table(summary_descr, rotate_for_one_row = TRUE,
                                       meta_data = meta_data,
                                       label_col = label_col,
                                       dl_fn = "summary_descr",
                                       output_format = "HTML")
      summary_descr <- htmltools::tagList(
        anchor = util_generate_anchor_tag(cur_var, "Descriptive statistics", "variable"),
        link = util_generate_anchor_link(cur_var, "Descriptive statistics", "variable"), # these links are moved later
        htmltools::h1("Descriptive statistics"),
        summary_descr
      )

    } else {
      summary_descr <- htmltools::p("")
    }

    svp <- c(list(summary_stat = list("", list(summary_stat))), # add the metadata info to the svp keeping the same structure of svp
             svp)
    svp <- c(list(summary_descr = list("", list(summary_descr))), # add the metadata info to the svp keeping the same structure of svp
             svp)

  }

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

  return(list(list("Single Variables",
                     cur_var,
                     sprintf("VAR_%s.html", prep_link_escape(cur_var, #remove special characters from variable name
                                                             html = TRUE)),
                     page)))


}
