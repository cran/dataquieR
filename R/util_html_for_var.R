#' Create a dynamic single variable page for the report
#'
#' @param report [dataquieR_resultset2] a `dq_report2` report
#' @param cur_var [character] variable name for single variable pages
#' @param use_plot_ly [logical] use `plotly`, if available.
#' @param template [character] template to use for the `dq_report2` report.
#' @param note_meta [character] notes on the metadata for a single variable
#'                              (if needed)
#' @param rendered_repsum the `dataquieR` summary, see [summary()],
#'   [dq_report2()] and [print.dataquieR_summary()]
#' @param dir [character] output directory for potential `iframes`.
#'
#'
#' @return list of arguments for `append_single_page()` defined locally in
#'   [util_generate_pages_from_report()].
#'
#' @keywords internal
util_html_for_var <- function(report, cur_var, use_plot_ly, template,
                              note_meta = c(), rendered_repsum, dir) {

  meta_data <- attr(report, "meta_data")
  label_col <- attr(report, "label_col")

  util_stop_if_not(inherits(report, "dataquieR_resultset2"))
  util_stop_if_not(!missing("cur_var") && length(cur_var) == 1 &&
                     !is.na(cur_var) &&
                      is.character(cur_var))


  rep_sum_el <- htmltools::HTML("")
  this <- attr(rendered_repsum, "this")
  repsum_wide <- attr(rendered_repsum, "repsum_wide")
  Variables <- util_map_labels(repsum_wide$Variables, meta_data, label_col,
                  attr(repsum_wide, "label_col"))
  if (any(Variables == cur_var)) {
    rs <- repsum_wide[Variables == cur_var,
                 setdiff(colnames(repsum_wide), "Variables"), FALSE]
    rs <- data.frame(x = colnames(rs), v = unname(unlist((rs[1, , TRUE]))))
    rs <- rbind.data.frame(rs[rs$x == "Total", , drop = FALSE],
                           rs[rs$x != "Total", , drop = FALSE])
    rs <-
      rs[!grepl("background: #ffffff", rs[, 2], fixed = TRUE), , drop = FALSE]
    rs <-
      rs[!is.na(rs[, 2]), , drop = FALSE]

    texts <- vapply(rs[, 1, drop = TRUE],
                    util_alias2caption, long = TRUE, FUN.VALUE = character(1))
    links <- vapply(mapply(href =
                             gsub("^.*window.location = &quot;(.*?)&quot;.*$",
                                  "\\1",
                                  gsub("^.*href=\"(.*?)\".*$", "\\1",
                                       rs[, 2, drop = TRUE])),
                           # rs[, 1, drop = TRUE],
                           text = texts,
                    SIMPLIFY = FALSE, FUN = function(href, text) {
                      # if (text == "Total") {
                      #   return(text)
                      # }
                      # as.character(util_generate_anchor_link(cur_var,
                      #                           callname = href,
                      #                           order_context = "variable",
                      #                           title = text))
                      paste0("<a href=\"", href, "\">", text, "</a>")
                    }), FUN = paste0, collapse = "", FUN.VALUE = character(1))
    rs[, 1] <- links
    if (!!prod(dim(rs))) {
      rep_sum_el <- htmltools::browsable(util_html_table(rs, filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                                           is_matrix_table = TRUE, rotate_headers = TRUE,
                                           meta_data = this$meta_data,
                                           label_col = this$label_col,
                                           dl_fn = "DQ Report Summary",
                                           output_format = "HTML", link_variables = FALSE, colnames = c(" ", " ")))
    }
  }

  rep_sum_el <- htmltools::tagList(
    anchor = util_generate_anchor_tag(cur_var, "Summary", "variable"),
    link = util_generate_anchor_link(cur_var, "Summary", "variable"), # these links are moved later
    rep_sum_el
  )
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
                                                                TRUE,
                                                              dir = dir),
                                                          SIMPLIFY = FALSE)))
      # if (cll == "con_inadmissible_categorical" && cur_var == "SBP_0") browser()
      # if we have some result, then store it in the svp list for the current single variable page
      if (length(output) && !all(vapply(output, is.null, FUN.VALUE = logical(1)))) {
        # title <- .manual$titles[[fkt]]

        title <- util_alias2caption(cll, long = TRUE)
        description <- util_function_description(fkt)

        anchor <- output[[1]]$anchor # extract anchor tag and move it above the caption of the current result
        output[[1]]$anchor <- htmltools::span() # delete anchor tag from its orignal position

        svp[[cll]] <- htmltools::tagList(
          htmltools::span( # span is requird to make [[2]][[1]]$link on page navi menu creation below work
            anchor, # put the anchor tag on top
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



  title <- htmltools::tagList(
    htmltools::h1(prep_get_labels(cur_var,
                                  max_len = 9999999,
                                  meta_data = meta_data,
                                  label_col = label_col,
                                  resp_vars_match_label_col_only = TRUE,
                                  label_class = "LONG")),
     htmltools::h2(paste0(
       prep_map_labels(cur_var,
                       meta_data = meta_data,
                       to = VAR_NAMES,
                       from = label_col,
                       ifnotfound = "",
                       warn_ambiguous = FALSE),
      " ",
      prep_get_labels(cur_var,
                      meta_data = meta_data,
                      label_col = label_col,
                      resp_vars_match_label_col_only = TRUE,
                      label_class = "SHORT")
       ))
  )

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

  svp <- c(list(rep_sum_el = list("", list(rep_sum_el))), # add the rep_sum_el info to the svp keeping the same structure of svp
           svp,
           list(md_info = list("", list(md_info))) # add the metadata info to the svp keeping the same structure of svp
           )

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

  add_anchor <- util_generate_anchor_tag(cur_var, "", "variable")

  page <- htmltools::tagList(add_anchor, title, body)

  title <- prep_get_labels(cur_var,
                           meta_data = meta_data,
                           label_col = label_col,
                           max_len = 9999999,
                           resp_vars_match_label_col_only = TRUE,
                           label_class = "LONG")
  title <- sprintf("%s: %s", names(title), title)

  return(list(list("Single Variables",
                     title,
                     sprintf("VAR_%s.html", prep_link_escape(cur_var, #remove special characters from variable name
                                                             html = TRUE)),
                     page)))


}
