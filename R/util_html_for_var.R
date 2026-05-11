#' Create a dynamic single variable page for the report
#'
#' @param results [list] a list of subsets of the report matching `cur_var`
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
#'   `util_generate_pages_from_report()`.
#'
#' @keywords internal
util_html_for_var <- function(results, cur_var, use_plot_ly, template,
                              note_meta = c(), rendered_repsum, dir,
                              meta_data, label_col,
                              dims_in_rep,
                              clls_in_rep,
                              function_alias_map) {

  fname <-
    sprintf("VAR_%s.html", prep_link_escape(cur_var, #remove special characters from variable name
                                          html = TRUE))

  if (getOption("dataquieR.resume_print", dataquieR.resume_print_default)) {
    # TODO: [html_files_should_also_work_alone] Check, if we have the html file, already, see util_create_page_file
    # do not re-compute, then
  }
  .fn <- file.path(dir, sub("\\.html$", ".RDS", fname))
  if (getOption("dataquieR.resume_print", dataquieR.resume_print_default)
      && file.exists(.fn)) {
    cached <- try(readRDS(.fn), silent = TRUE)
    if (!util_is_try_error(cached)) {
      return(cached)
    }
  }

  util_stop_if_not(is.list(results))
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

    x <- rs[, 2, drop = TRUE]

    # 1) href="..."
    href1 <- sub('^.*href="([^"]+)".*$', "\\1", x)

    # nur echte Treffer behalten
    href1[href1 == x] <- NA_character_

    # 2) window.location = &quot;...&quot;
    href2 <- sub(
      '^.*window\\.location\\s*=\\s*&quot;([^&]+)&quot;.*$',
      "\\1",
      x
    )
    href2[href2 == x] <- NA_character_

    # 3) showDataquieRResult(&quot;...&quot;, &quot;TARGET&quot;, &quot;...&quot;)
    href3 <- sub(
      '^.*showDataquieRResult\\(&quot;[^&]*&quot;\\s*,\\s*&quot;([^&]+)&quot;.*$',
      "\\1",
      x
    )
    href3[href3 == x] <- NA_character_

    # Fallback-Kaskade pro Zeile
    href_final <- ifelse(
      !is.na(href1), href1,
      ifelse(!is.na(href2), href2, href3)
    )

    links <- vapply(
      mapply(
        href = href_final,
        text = texts,
        no_res = grepl("No results available", rs[, 2]),
        SIMPLIFY = FALSE,
        FUN = function(href, text, no_res) {
          if (any(grepl(".", href, fixed = TRUE)) && !no_res) {
            paste0("<a href=\"", href, "\">", text, "</a>")
          } else {
            paste0("<a style=\"color:black;cursor:default;text-decoration:none;\">", text, "</a>")
          }
        }
      ),
      FUN = paste0, collapse = "",
      FUN.VALUE = character(1)
    )

    rs[, 1] <- links

    # Remove onclick from a elements
    rs[, 2] <- gsub("(<a\\b[^>]*?)\\s+onclick\\s*=\\s*(['\"]).*?\\2", "\\1", rs[, 2], perl = TRUE, ignore.case = TRUE)
    rs <- rs[!is.na(href3), , FALSE]

    if (!!prod(dim(rs))) {
      rep_sum_el <- htmltools::browsable(util_html_table(rs, filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
                                           is_matrix_table = TRUE, rotate_headers = TRUE,
                                           meta_data = this$meta_data,
                                           label_col = this$label_col,
                                           dl_fn = "DQ Report Summary",
                                           link_variables = FALSE, colnames = c(" ", " ")))
    }
  }

  rep_sum_el <- htmltools::tagList(
    anchor = util_generate_anchor_tag(cur_var, "Summary", "variable"),
    link = util_generate_anchor_link(cur_var, "Summary", "variable"), # these links are moved later
    rep_sum_el
  )

  svp <- list()

  for (cur_dm in dims_in_rep) {
    # util_message("Dimension %s", cur_dm)
    drop_down <- dims[[cur_dm]] # fetches the name of the content for the first-level drop-down menu
    j <- 0
    m <- length(clls_in_rep[[cur_dm]])
    for (cll in clls_in_rep[[cur_dm]]) { # loop over the functions in each dimension
      fkt <- util_cll_nm2fkt_nm(cll, function_alias_map = function_alias_map)
      all_of_f <- results[paste0(cll, '.', cur_var)] # extract the single result
      all_of_f <- # remove non-existing results
        all_of_f[!vapply(all_of_f, is.null, FUN.VALUE = logical(1))]

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
            htmltools::div(class="infobutton", htmltools::tagList(
                           htmltools::HTML(description),
                           htmltools::h6("Concept relations:"),
                           htmltools::p(util_get_concept_links(fkt))
                           ))),
          output)
      } else {
        svp[[cll]] <- NULL
      }

    }

  }


  ..vn <- prep_map_labels(cur_var,
                          meta_data = meta_data,
                          to = VAR_NAMES,
                          from = label_col,
                          ifnotfound = "",
                          warn_ambiguous = FALSE)
  ..lbs <- prep_get_labels(cur_var,
                           meta_data = meta_data,
                           label_col = label_col,
                           resp_vars_match_label_col_only = TRUE,
                           label_class = "SHORT")
  if (startsWith(..lbs, ..vn)) {
    ..vn <- ""
  }
  title <- htmltools::tagList(
    htmltools::h1(prep_get_labels(cur_var,
                                  max_len = 9999999,
                                  meta_data = meta_data,
                                  label_col = label_col,
                                  resp_vars_match_label_col_only = TRUE,
                                  label_class = "LONG")),
     htmltools::h2(paste(
       ..vn,
      ..lbs
       ))
  )

  cur_md <- meta_data[meta_data[[label_col]] == cur_var,
                      , FALSE] # subset the metadata for the current variable to show it in the report

  { # link tables to their representation in this report-site
    cur_md[, !grepl("_TABLE$", colnames(cur_md))] <-
      util_df_escape(cur_md[, !grepl("_TABLE$", colnames(cur_md)),
                            drop = FALSE])

    for (cn in grep("_TABLE$", colnames(cur_md), value = TRUE)) {

      cur_md[!util_empty(cur_md[[cn]]), cn] <- vapply(FUN.VALUE = character(1),
                                                      cur_md[!util_empty(cur_md[[cn]]), cn],
                                                      FUN = function(tn) {
                                                        paste(as.character(htmltools::a(href = paste0(
                                                          prep_link_escape(tn, html = TRUE), ".html"), tn)),
                                                          collapse = "")
                                                      }
      )

    }

  }

  md_info <- htmltools::tagList(## link tables
    anchor = util_generate_anchor_tag(cur_var, "meta_data", "variable"),
    link = util_generate_anchor_link(cur_var, "meta_data", "variable"), # these links are moved later
    htmltools::h1("Item-level Metadata"),
    htmltools::div(note_meta),
    util_html_table(data.frame(Name = names(cur_md),
                               Value = unname(t(cur_md))),
                    meta_data = meta_data,
                    label_col = label_col,
                    copy_row_names_to_column = FALSE,
                    dl_fn = paste("Item-level_Metadata_", cur_var))
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

  is_var_page_js <- htmltools::tags$script(type = "text/javascript",
                                           htmltools::HTML("window.is_svp = true;"))

  page <- htmltools::tagList(is_var_page_js, add_anchor, title, body)

  title <- prep_get_labels(cur_var,
                           meta_data = meta_data,
                           label_col = label_col,
                           max_len = 9999999,
                           resp_vars_match_label_col_only = TRUE,
                           label_class = "LONG")
  if (startsWith(x = title, prefix = sprintf("%s:", names(title)))) {
    title <- sprintf("%s", title)
  } else {
    title <- sprintf("%s: %s", names(title), title)
  }


  r <- list(list("Single Variables",
                 util_attach_attr(title,
                                  alternative_names =
                                    as.vector(unlist(
                                      cur_md[, startsWith(colnames(cur_md), LABEL) |
                                               startsWith(colnames(cur_md), LONG_LABEL) |
                                               colnames(cur_md) == VAR_NAMES,
                                             drop = TRUE]))),
                 fname,
                 page))

  if (getOption("dataquieR.resume_print", dataquieR.resume_print_default)) {
    saveRDS(r, file = .fn, compress = "xz")
  }

  return(r)

}
