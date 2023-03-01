#' Generate a full DQ report, v2
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for [parallelMap::parallelStart] or NULL,
#'                        if parallel has already been started by the caller.
#' @param debug_parallel [logical] print blocks currently evaluated in parallel
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_dataframe [data.frame] -- optional: Data frame level
#'                                                                 metadata
#' @param all_calls [list] a list of calls
#' @param meta_data_cross_item [data.frame] -- optional: cross-item level
#'                                             metadata
#' @param resp_vars [variable list] the name of the measurement variables
#'                                  for the report.
#' @param filter_result_slots [character] regular expressions, only
#'                                               if an indicator function's
#'                                               result's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#'
#' @return a [dataquieR_resultset2]. Can be printed creating a RMarkdown-report.
util_evaluate_calls <-
  function(all_calls,
           study_data,
           meta_data,
           label_col,
           meta_data_segment,
           meta_data_dataframe,
           meta_data_cross_item,
           resp_vars,
           filter_result_slots,
           cores,
           debug_parallel) {
    # TODO: add all the objects, that Square2 writes for being full compatible
    r <- NULL
    util_ensure_suggested("parallel")
    if (length(all_calls) > 0) {

      if (!is.null(cores)) {
        if (inherits(cores, "list")) {
          suppressMessages(do.call(parallelMap::parallelStart, cores))
        } else {
          suppressMessages(parallelMap::parallelStart("socket", cpus = cores,
                                                      logging = FALSE,
                                                      load.balancing = TRUE))
        }
        on.exit(suppressMessages(parallelMap::parallelStop()))
        cores <- NULL
      }

      util_setup_rstudio_job(
        "Computing dq_report2, parallel computation")

      oldO <- options(parallelMap.show.info = FALSE)
      on.exit(options(oldO), add = TRUE)
      parallelMap::parallelLibrary(utils::packageName())
      parallelMap::parallelExport("study_data", "meta_data", "label_col", "meta_data_segment", "meta_data_dataframe", "meta_data_cross_item")
      parallelMap::parallelExport(".dataframe_environment")
      if (!is.null(parallel::getDefaultCluster())) {
        parallel::clusterEvalQ(cl = NULL, dataquieR::prep_add_data_frames(
          data_frame_list = as.list(.dataframe_environment)))
      } else {
        dataquieR::prep_add_data_frames(
          data_frame_list = as.list(.dataframe_environment))
      }
      # parallelMap::parallelLapply(1:10, function(x) {dataquieR::prep_list_dataframes()})

      # this exports the static data to the cluster, this is always done, even if
      # already avail. functions don't touch the exported data.
      currentCpus <- parallelMap::parallelGetOptions()$settings$cpus

      .worker <- util_eval_to_dataquieR_result

      if (parallelMap::parallelGetOptions()$settings$mode == "local") {
        environment(.worker) <- new.env(parent = asNamespace(utils::packageName()))
      } else if (parallelMap::parallelGetOptions()$settings$mode == "multicore") {
        environment(.worker) <- environment()
      } else {
        environment(.worker) <- asNamespace(utils::packageName())
      }

      n_nodes <- max(1, as.integer(currentCpus[[1]]), na.rm = TRUE)
      # was length(parallel::getDefaultCluster()), but parallelMap doesn't use
      # defaultcluster any more!!
      tasks_per_node <- ceiling(length(all_calls) / n_nodes)
      indices <- 1:length(all_calls)
      length(indices) <- n_nodes * tasks_per_node # make equal length
      task_matrix <- matrix(indices, ncol = n_nodes, nrow = tasks_per_node,
                            byrow = TRUE)
      r <- unlist(lapply(
        1:nrow(task_matrix),
        function(row) {
          slices <- task_matrix[row, ]
          slices <- slices[!is.na(slices)]
          if (length(all_calls))
            progress(100 * row / nrow(task_matrix))
          util_message(
            sprintf("%s [%s] %d of %d, %s", Sys.time(), "INFO",
                    row, nrow(task_matrix), "DQ")
          ) # TODO: Use RStudio job if available
          if (debug_parallel) {
            # TODO: log something about the current chunk
          }
          R.devices::suppressGraphics(
            # don't use any auto graphics device (needed for certain
            # parallelization methods)
            parallelMap::parallelLapply(impute.error = identity,
                                        all_calls[slices], .worker,
                                        filter_result_slots =
                                          filter_result_slots,
                                        env = environment())
          )
        }
      ), recursive = FALSE)

      util_message(
        sprintf("%s [%s], %s", Sys.time(), "INFO",
                "DQ -- done")
      ) # TODO: Use RStudio job if available
    }

    function_names <- vapply(lapply(all_calls, `[[`, 1), as.character,
                             FUN.VALUE = character(1))

    r[] <- mapply(function_names, all_calls, r, FUN = function(nm, cl, r) {
      attr(r, "function_name") <- nm
      attr(r, "call") <- cl
      r
    })

    function_names <- setNames(nm = function_names)

    aliases <- gsub("\\..*$", "", names(r))

    matrix_list <- lapply(setNames(nm = setdiff(aliases,
                                                attr(all_calls,
                                                     "multivariatcol"))),
                          function(alias) {
                            lapply(setNames(nm = resp_vars),
                                   function(vn) {
                                     rlang::call_args(
                                       all_calls[[paste0(alias, ".", vn)]]
                                     )
                                   })
                          })

    function_names <- function_names[!duplicated(aliases)]
    aliases <- aliases[!duplicated(aliases)]

    descriptions = .manual$descriptions[function_names]
    if (length(descriptions) == 0) {
      descriptions <- rep(NA_character_, length(function_names))
    }
    descriptions[vapply(descriptions, is.null, FUN.VALUE = logical(1))] <-
      NA_character_
    descriptions = unlist(descriptions, recursive = FALSE)

    function_alias_map <- data.frame(
      fk_report = rep(NA_integer_, length(aliases)),
      alias = aliases,
      acronym = util_abbreviate(aliases),
      description = descriptions,
      fk_function = rep(NA_integer_, length(aliases)),
      # TODO: check if .manual actually exists
      function_description = descriptions,
      name = function_names,
      stringsAsFactors = FALSE)

    attr(matrix_list, "function_alias_map") <- function_alias_map

    fn <- unique(function_names)

    function2category <- setNames(c(
      int = "Integrity",
      com = "Completeness",
      con = "Consistency",
      acc = "Accuracy"
    )[gsub(
      "_.*$",
      "",
      fn)],
    nm = fn)

    attr(matrix_list, "function2category") <- function2category

    dim_ranks <- setNames(c(
      int = 0,
      com = 1,
      con = 2,
      acc = 3
    )[gsub(
      "_.*$",
      "",
      aliases)],
    nm = aliases)


    drr <- rank(dim_ranks)
    fnr <- rank(function_names)
    alr <- rank(aliases)

    base <- max(c(drr, fnr, alr), na.rm = TRUE)

    attr(matrix_list, "col_indices") <- setNames(
      (drr * base * base + fnr * base + alr) * 10,
      nm = aliases)

    vo <- meta_data[[VARIABLE_ORDER]]
    if (0 == length(vo)) {
      vo <- seq_len(nrow(meta_data)) * 10
    }
    attr(matrix_list, "row_indices") <- setNames(vo,
                                                 nm = meta_data[[label_col]])
    if (!length(r)) {
      r <- list()
    }
    attr(r, "matrix_list") <- matrix_list
    attr(r, "label_col") <- label_col
    attr(r, "meta_data") <- meta_data # one discapency from SQ2 reults, here: We have *always* v2.0 metadata, here. Will not yet write a back-converter, as long as this is not really needed.

    attr(r, "meta_data_segment") <- meta_data_segment
    attr(r, "meta_data_dataframe") <- meta_data_dataframe
    attr(r, "meta_data_cross_item") <- meta_data_cross_item

    # Include all tables referred to by the standard metadata ----
    meta_data_frames <-
      grep("^meta_data", names(attributes(r)), value = TRUE)

    refs <- unname(unlist(lapply(meta_data_frames,
                       function(mdf_name) {
                         mdf <- attr(r, mdf_name)
                         cls <- grep("_TABLE$", colnames(mdf), value = TRUE)
                         if (length(cls)) {
                           unlist(mdf[, cls], recursive = TRUE)
                         } else {
                           character(0)
                         }
                       }), recursive = FALSE))

    refs <- refs[!util_empty(refs)]
    refs <- gsub(sprintf("\\s*\\%s\\s*", SPLIT_CHAR),
                 SPLIT_CHAR, refs)
    refs <- gsub(sprintf("(\\%s.*?)\\%s.*$", SPLIT_CHAR, SPLIT_CHAR),
                 "\\1", refs)
    refs <- unique(sort(refs))

    refs <- lapply(setNames(nm = refs), function(dfn) {
      r <- NULL
      try(
        r <- prep_get_data_frame(dfn),
        silent = TRUE
      )
      r
    })

    attr(r, "referred_tables") <- refs

    # add information on the dimension names ----

    attr(r, "study_data_dimnames") <- dimnames(study_data)
    attr(r, "cn") <- attr(all_calls, "cn")
    attr(r, "rn") <- attr(all_calls, "rn")

    # class is to be compatible with Square2 -----

    class(r) <- union(dataquieR_resultset_class2,
                      "square_results")

    # Return report ----
    r
  }
