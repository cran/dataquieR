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
#'                        Can also be a cluster.
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
#' @inheritParams dq_report2
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
           debug_parallel,
           mode = c("default", "futures", "queue", "parallel"),
           mode_args) {

    if (length(mode_args) > 0) {
      if (!is.list(mode_args) || is.null(names(mode_args)) ||
          any(util_empty(names(mode_args)))) {
        util_message("%s needs to be a named list",
                     sQuote("mode_args"))
        mode_args <- list()
      }
    } else {
      mode_args <- list()
    }


    mode <- util_match_arg(mode)

    invisible(force(parallelMap::parallelGetOptions()$settings$mode)) # ensure, that this package is loaded here: it sets onLoad some options that
                                                                      # we want to overwrite below. doing so w/o having th package loaded will
                                                                      # overwrite the options, if parallelMap is acutally loaded after the option
                                                                      # setting code below.

    # maybe also https://cran.r-project.org/web/packages/parabar/readme/README.html
    # TODO: add all the objects, that Square2 writes for being full compatible
    r <- NULL
    util_ensure_suggested("parallel")
    util_setup_rstudio_job(
      "Computing dq_report2, parallel computation")

    if (mode == "queue") {
      if ((eval.parent(call("missing", as.symbol("cores"))) &&
          identical(cores,
                    eval.parent(formals(rlang::caller_fn())$cores))) ||
          missing(cores)) {
        cores <- util_detect_cores()
      }
      if (length(cores) != 1 ||
          !util_is_integer(cores) ||
          is.na(cores) ||
          cores > util_detect_cores()) {
        cores <- util_detect_cores()
        util_message(c("For mode %s, %s can only be an integer(1) <= %d,",
                       "it to its maximum"),
                     dQuote(mode),
                     sQuote("cores"),
                     cores)
      }
      q <- util_queue_cluster_setup(n_nodes = cores,
                                    progress = progress,
                                    debug_parallel = debug_parallel)
    } else {
      q <- NULL
    }

    if (length(all_calls) > 0) {
      if (is.null(q)) {
        if (!is.null(cores)) {
          if (inherits(cores, "cluster")) {
            old_def_cl <- parallel::getDefaultCluster()
            parallel::setDefaultCluster(cores)
            on.exit(parallel::setDefaultCluster(old_def_cl), add = TRUE)
            old_o_def_cl <- options(
              parallelMap.cpus = length(parallel::getDefaultCluster()),
              parallelMap.load.balancing = TRUE,
              parallelMap.mode = "socket"
            )
            on.exit(options(old_o_def_cl), add = TRUE)
          } else if (inherits(cores, "list")) {
            suppressMessages(do.call(parallelMap::parallelStart, cores))
            on.exit(suppressMessages(parallelMap::parallelStop()), add = TRUE)
          } else {
            suppressMessages(parallelMap::parallelStart("socket", cpus = cores,
                                                        logging = FALSE,
                                                        load.balancing = TRUE))
            on.exit(suppressMessages(parallelMap::parallelStop()), add = TRUE)
          }
          cores <- NULL
        } else if
        (getOption("parallelMap.mode") != "BatchJobs" &&
         getOption("parallelMap.mode") != "batchtools" &&
         !is.null(parallel::getDefaultCluster())) {
          old_o_def_cl <- options(
            parallelMap.cpus = length(parallel::getDefaultCluster()),
            parallelMap.load.balancing = TRUE,
            parallelMap.mode = "socket"
          )
          on.exit(options(old_o_def_cl), add = TRUE)
        }

        oldO <- options(parallelMap.show.info = FALSE)
        on.exit(options(oldO), add = TRUE)

        parlib <- parallelMap::parallelLibrary
        parexp <- parallelMap::parallelExport
        par_eval_q <- function(expr) {
          do.call(
            parallel::clusterEvalQ,
            list(cl = NULL, expr)
          )
        }

      } else {
        parlib <- function(lib) {
          q$workerEval(function(lib) {
            require(lib, character.only = TRUE)
          }, list(lib = lib))
        }
        parexp <- function(...) {
          q$export(...)
        }
        par_eval_q <- function(expr) { # may not work as expected
          q$workerEval(function(expr) {
            eval(expr)
            }, list(expr = substitute(expr)))
        }
      }

      parlib(utils::packageName())

      suppressWarnings(parexp("study_data", "meta_data", "label_col", "meta_data_segment", "meta_data_dataframe", "meta_data_cross_item"))

      suppressWarnings(parexp(".dataframe_environment"))

      if (!is.null(q) || !is.null(parallel::getDefaultCluster())) {
        par_eval_q(dataquieR::prep_add_data_frames(
          data_frame_list = as.list(.dataframe_environment)))
        u8 <- par_eval_q(l10n_info()[["UTF-8"]])
        u8 <- vapply(u8, identity, FUN.VALUE = logical(1))
        if (!all(u8)) {
          util_warning(c("%d of the %d cluster nodes do not support",
                         "UTF-8, this may cause trouble with the encoding.",
                         "For Windows nodes, you should use R > 4.2.0",
                         "on all nodes. Also, all nodes should use a UTF-8",
                         "character set by default (see Sys.setlocale())"),
                       sum(!u8), length(u8))
        }
      } else {
        dataquieR::prep_add_data_frames(
          data_frame_list = as.list(.dataframe_environment))
      }
      # parallelMap::parallelLapply(1:10, function(x) {dataquieR::prep_list_dataframes()})

      # this exports the static data to the cluster, this is always done, even if
      # already avail. functions don't touch the exported data.
      currentCpus <- parallelMap::parallelGetOptions()$settings$cpus

      worker <- util_eval_to_dataquieR_result
      formals(worker)$filter_result_slots = filter_result_slots
      force(formals(worker)$filter_result_slots)

      if (parallelMap::parallelGetOptions()$settings$mode == "local") {
        environment(worker) <- new.env(parent = asNamespace(utils::packageName()))
      } else if (parallelMap::parallelGetOptions()$settings$mode == "multicore") {
        environment(worker) <- environment()
      } else {
        environment(worker) <- asNamespace(utils::packageName())
      }

      n_nodes <- max(1, as.integer(currentCpus[[1]]), na.rm = TRUE)

      if (mode == "futures") { # have/use futures
        r <- util_parallel_futures(
          all_calls = all_calls,
          n_nodes = n_nodes,
          progress = progress,
          worker = worker,
          debug_parallel = debug_parallel
        )
      } else if (mode == "queue") {
        step <- 15
        if ("step" %in% names(mode_args)) {
          step <- mode_args[["step"]]
          if (length(step) != 1 || !is.numeric(step) ||
              !is.vector(step) || !is.finite(step) || !util_is_integer(step) ||
            step <= 0 || step > 10000) {
            util_message(
              c("%s needs to be a positive scalar integer value <= %d, falling",
                "back to default %d"),
              dQuote("step"),
              10000,
              15)
            step <- 15
          }
        }
        r <- q$compute_report(all_calls = all_calls,
                              worker = worker,
                              step = step)
      } else {
        r <- util_parallel_classic(
          all_calls = all_calls,
          n_nodes = n_nodes,
          progress = progress,
          worker = worker,
          debug_parallel = debug_parallel
        )
      }

      util_message(
        sprintf("%s [%s], %s", Sys.time(), "INFO",
                "DQ -- done")
      ) # TODO: Use RStudio job if available
    }

    function_names <- vapply(lapply(all_calls, `[[`, 1), as.character,
                             FUN.VALUE = character(1))

    r[] <- mapply(function_names, all_calls, r, SIMPLIFY = FALSE,
                  FUN = function(nm, cl, r) {
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
                                     if (is.call(
                                       all_calls[[paste0(alias, ".", vn)]])) {
                                       rlang::call_args(
                                         all_calls[[paste0(alias, ".", vn)]]
                                       )
                                     } else {
                                       setNames(list(), nm = character(0))
                                     }
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

    base <- max(c(drr, fnr, alr, 10), na.rm = TRUE)

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
