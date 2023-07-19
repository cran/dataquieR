#' Create a dynamic dimension related page for the report
#'
#' @param report [dataquieR_resultset2] a `dq_report2` report
#' @param use_plot_ly [logical] use `plotly`, if available.
#' @param template [character] template to use for the `dq_report2` report.
#' @param block_load_factor [numeric] multiply size of parallel compute blocks
#'                                    by this factor.
#'
#' @return list of arguments for `append_single_page()` defined locally in
#'   [util_generate_pages_from_report()].
util_html_for_dims <- function(report, use_plot_ly, template,
                               block_load_factor) {

  util_setup_rstudio_job("Page generation: Indicator View")

  meta_data <- attr(report, "meta_data")
  label_col <- attr(report, "label_col")

  util_stop_if_not(inherits(report, "dataquieR_resultset2"))

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


  clls_in_rep_2 <-
    factor(vapply(strsplit(colnames(report), '_', fixed = TRUE), `[[`, 1,
                  FUN.VALUE = character(1)), names(dims), ordered = TRUE)
  sorted_clls <- colnames(report)[order(order(clls_in_rep_2))]

  i <- 0
  n <- ncol(report)

  # already done, before in util_generate_pages_from_report
  # suppressWarnings(
  #   parallelMap::parallelExport("report", "use_plot_ly", "template"))
  # parallelMap::parallelLibrary(utils::packageName())
  cores <- getOption("parallelMap.cpus", 1)
  if (!is.numeric(cores) || !util_is_integer(cores) || !is.finite(cores)) {
    cores <- 1
  }
  block_size <- block_load_factor * cores
  all_pages <- list()
  nblocks <- ceiling(ncol(report) / block_size)
  for (cur_block in seq_len(nblocks) - 1) { # create the single variable pages
    block_indices <- seq(1 + cur_block * block_size,
                         min(cur_block * block_size + block_size,
                             ncol(report)))
    clls_in_chunk <- colnames(report)[block_indices]
    clls_in_chunk <- clls_in_chunk[!is.na(clls_in_chunk)]
    progress(i/n * 100)
    progress_msg("Page generation", sprintf("Single Indicators %s",
                                            paste(sQuote(clls_in_chunk),
                                                  collapse = ", ")))
    chunk_of_pages <- parallelMap::parallelLapply(
      clls_in_chunk,
      function(cll) {
        cur_dm <- sub("_.*$", "", cll)
        drop_down <- dims[[cur_dm]] # fetches the name of the content for the first-level drop-down menu
        fkt <- util_cll_nm2fkt_nm(cll, report)
        is_single_var <- FALSE
        # indicator overview page
        # take all results for the current function
        all_of_f <- util_combine_res(report[, cll])

        # try to also combine preferred_summary_slots
        resn <- intersect(resnames.dataquieR_resultset2(report[, cll]),
                  preferred_summary_slots)
        resn <- setdiff(resn, resnames.dataquieR_resultset2(all_of_f))
        if (length(resn) > 0) { # we have additional summaries to append
          resn <- head(resn, 1) # take the top-prio slot from the remainng ones
          all_of_f1 <- util_combine_res(report[, cll, res = resn])
          all_of_f[[cll]][[resn]] <- all_of_f1[[cll]][[resn]]
        }

        # create a page by iterating over all_of_f entries and their respective names
        # for each element the util_pretty_print function is called, converting each dataquieR result to an htmltools compatible object
        # the do.call(htmltools::tagList) converts the output as an htmltools compatible tag list ("as.tagList.list")
        # we have to unname the list of tags to prevent taglists from generating named html attributes
        page <- do.call(htmltools::tagList, unname(mapply(dqr = all_of_f,
                                                          nm = names(all_of_f),
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
                                                                FALSE),
                                                          SIMPLIFY = FALSE
        )))
        # check that the length of the page is positive and that it does not only contain null
        if ((length(page) && !all(vapply(page, is.null, FUN.VALUE = logical(1))))) {
          if ("link" %in% unique(unlist(lapply(page, names)))) { # if we have page navigation links
            # extract link information for the page navigation menu (created by util_pretty_print)
            all_links <- lapply(page, `[[`, "link")
            # add links to the page navigation menu
            all_links <- all_links[vapply(all_links, length, # remove empty links
                                          FUN.VALUE = integer(1)) != 0]
            all_links <- lapply(all_links, htmltools::tags$li) # add li to each tag to create the items
            all_links <- do.call(htmltools::tagList, all_links) # convert the list to a tag list, which is htmltools compatible
            page_menu <- util_float_index_menu(object = all_links) # create the menu
            # remove the original links (as created by util_pretty_print in the wrong places)
            page <- lapply(page, function(p) {
              p$link <- NULL
              p
            })
            # generates pages with navigation menu
            # load concept to get current indicator links in reports
            concept <- util_get_concept_info("dqi")
            concept <- concept[c("PublicID", "Indicator", "dataquieR function")]
            fkt2concept <- subset(concept,
                                  get("dataquieR function") ==
                                    fkt)
            # create link tags
            get_links <- function(indicator_id, indicator_name) {
              htmltools::tags$a(target="_blank",
                                href= paste("https://dataquality.qihs.uni-greifswald.de/id/#", indicator_id, sep = ""),
                                indicator_name)
            }
            # create un-ordered item list for each indicator
            links <- mapply(get_links, indicator_id = fkt2concept$PublicID,
                            indicator_name = fkt2concept$Indicator, SIMPLIFY = FALSE)
            names(links) <- NULL
            items <- lapply(links, htmltools::tags$li)
            items_indicators <- htmltools::tags$ul(items)
            # call template
            page <- htmltools::htmlTemplate(
              system.file("templates", template, "single_indicator_with_menu.html",
                          package = utils::packageName()),
              page_menu = page_menu,
              cll = cll,
              fkt = fkt,
              title = util_alias2caption(fkt, long = TRUE),
              description = htmltools::HTML(util_function_description(fkt)),
              page = page,
              online_ref = util_online_ref(fkt),
              items_indicators = items_indicators
            )
            if (cur_dm %in% c("acc")) {  # acc is in separate files
              fname <- paste0("dim_", cur_dm, "_", cll, ".html") # add the function name for accuracy
            } else {
              fname <- paste0("dim_", cur_dm, ".html") # define the file name, one file per dimension if not in accuracy
            }
            # the "dim_" prefix is required because otherwise windows will ignore a file called con.html confusing it with a special device con:
          } else { # for pages without navigation menu, not used
            page <- htmltools::htmlTemplate( #
              system.file("templates", template, "single_indicator.html",
                          package = utils::packageName()),
              cll = cll,
              fkt = fkt,
              title = util_alias2caption(fkt, long = TRUE),
              description = util_function_description(fkt),
              page = page,
              online_ref = util_online_ref(fkt)
            )
            fname <- paste0("dim_", cur_dm, ".html") # define the file name, one file per dimension if not in accuracy
          }
          list( drop_down,
                util_alias2caption(cll),
                fname,
                page)
        }

      }
    )
    all_pages <- c(all_pages, chunk_of_pages)
    i <- i + length(clls_in_chunk)
    progress(i/n * 100)
  }
  all_pages <- all_pages[vapply(all_pages, length,
                                          FUN.VALUE = integer(1)) > 0]
  all_pages
}
