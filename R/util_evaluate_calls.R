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
#' @param checkpoint_resumed [logical] if using a `storr_factory` and the back-
#'                                     end there is already filled
#'                                     compute all missing result and add them
#'                                     to the back-end.
#' @inheritParams dq_report2
#'
#' @return a [dataquieR_resultset2]. Can be printed creating a RMarkdown-report.
#'
#' @family reporting_functions
#' @concept process
#' @noRd
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
           mode_args,
           my_storr_object = NULL,
           checkpoint_resumed,
           dt_adjust = as.logical(getOption("dataquieR.dt_adjust",
                                               dataquieR.dt_adjust_default))) {

    conds <- NULL # integrity issues outside the pipeline are collected here

    my_storr_object <- util_fix_storr_object(my_storr_object)

    function_names <- vapply(lapply(all_calls, `[[`, 1), as.character,
                             FUN.VALUE = character(1))

    if (!is.null(my_storr_object) && checkpoint_resumed) {
      all_calls_cp <- all_calls[setdiff(names(all_calls),
                                        my_storr_object$list(namespace =
                                                               util_get_storr_stat_namespace(my_storr_object)))]
    } else {
      all_calls_cp <- all_calls
    }

    # if (!is.null(my_storr_object)) {
    #   # if (inherits(my_storr_object$driver, "driver_thor")) {
    #   #   util_error( # IDEA: if we would write whenever we collect results and increase progress, we would not write concurrently to the storr.
    #   #     c("CAVEAT: LMDB databases (as used by the package thor) are",
    #   #       "not supported because of known issues with file-locking"))
    #   # }
    #   my_storr_object$set(NO_SHARED_STORR, TRUE) # will be deleted during computation
    #   # https://stackoverflow.com/a/74509506 -- be as robust as possible for local database backends
    #   invisible(lapply(names(all_calls_cp), function(slot) {
    #     invisible(util_eval_to_dataquieR_result(init = TRUE,
    #       quote({util_error(paste("No result available for unkown reasons",
    #                               "(out of memory? try to reduce the number",
    #                               "of parallel running jobs using the",
    #                               "`cores` argument)"))}),
    #       filter_result_slots = ".*", nm = slot,
    #       my_storr_object = my_storr_object,
    #       function_name = function_names[[slot]],
    #       my_call = all_calls_cp[[slot]]))
    #   }))
    #   my_storr_object$flush_cache()
    #
    # }

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
    r <- list()
    util_ensure_suggested("parallel")
    util_setup_rstudio_job(
      "Computing dq_report2, parallel computation",
      n = length(all_calls))

    progress_msg("Cluster setup", "initializing parallel mode, if applicable")

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
                                    debug_parallel = debug_parallel,
                                    my_storr_object = my_storr_object)
    } else {
      q <- NULL
    }

    if (length(all_calls_cp) > 0) {
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

        parlib <- function(lib) {
          suppressMessages(suppressPackageStartupMessages(
            parallelMap::parallelLibrary(lib, show.info = FALSE)))
        }
        parloadNS <- function(lib) {
          .exp <- substitute({
            suppressMessages(suppressPackageStartupMessages(
              loadNamespace(lib)))
            invisible(NULL)
          })
          parexp(".exp")
          par_eval_q(eval(.exp))
        }
        parexp <- parallelMap::parallelExport
        par_eval_q <- function(expr) {
          if (is.null(parallel::getDefaultCluster())) {
            eval(expr)
          } else {
            do.call(
              parallel::clusterEvalQ,
              list(cl = NULL, substitute(expr))
            )
          }
        }

      } else {
        parloadNS <- function(lib) {
          q$workerEval(function(lib) {
            suppressMessages(suppressPackageStartupMessages(
              loadNamespace(lib)))
          }, list(lib = lib))
        }
        parlib <- function(lib) {
          q$workerEval(function(lib) {
            suppressMessages(suppressPackageStartupMessages(
              require(lib, quietly = TRUE, character.only = TRUE)))
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

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "loading library")

      if (suppressWarnings(util_ensure_suggested("pkgload", err = FALSE,
                                                 goal =
                                                 "not really needed"))) {
        dev_package <- pkgload::is_dev_package(utils::packageName())
      } else {
        dev_package <- FALSE
      }

      if (dev_package && !is.null(parallel::getDefaultCluster()) &&
          !isTRUE(getOption("dataquieR.tmp_no_load_all"))) {
        .d <- system.file(package = utils::packageName())
        .exp <- substitute({
          pkgload::load_all(path = .d)
          invisible(NULL)
        })
        parexp(".exp")
        par_eval_q(eval(.exp))
      } else {
        suppressWarnings(suppressMessages(try({
          parlib(utils::packageName())
        }, silent = TRUE)))
      }
      parloadNS("hms")

      ..e <- environment()
      conds <- list()

      suppressWarnings(suppressMessages(withCallingHandlers({

        if (dt_adjust) {
          progress_msg("Cluster setup: initializing parallel mode, if applicable",
                       "consolidating data types 1...")

          ## Adjust data type and compute int_data_type_matrix before

          withr::with_options(list(dataquieR.testdebug = TRUE), # TODO: Find an internal solution to suppress convenience argument fillers and printers
                              int_datatype_matrix_res <- int_datatype_matrix(
                                study_data = study_data,
                                meta_data = meta_data,
                                label_col = label_col
                              )
          )
        } else {
          withr::with_options(list(dataquieR.testdebug = TRUE), # TODO: Find an internal solution to suppress convenience argument fillers and printers
            int_datatype_matrix_res <- int_datatype_matrix(
              study_data = study_data[1, , FALSE],
              meta_data = meta_data,
              label_col = label_col
            )
          )
        }

      },
      condition = function(cnd) {
        ..e$conds <- c(..e$conds, list(cnd))
      })))

      conds <- unique(conds[vapply(lapply(conds, attr, "integrity_indicator"),
                                   length, FUN.VALUE = integer(1)) == 1]) # IDEA: Don't discard "other" conditions, except such with non-matching var_name-attribute, see also my_conds below.


      old_.dq2_globs.called_in_pipeline <- .dq2_globs$.called_in_pipeline
      .dq2_globs$.called_in_pipeline <- TRUE
      on.exit(.dq2_globs$.called_in_pipeline <-
                old_.dq2_globs.called_in_pipeline, add = TRUE)

      if (dt_adjust) {
        progress_msg("Cluster setup: initializing parallel mode, if applicable",
                     "consolidating data types 2...")
        study_data <- util_adjust_data_type(
          study_data = study_data,
          meta_data = meta_data,
          relevant_vars_for_warnings = NULL)
      }

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting data")

      suppressWarnings(parexp("study_data", "meta_data", "label_col", "meta_data_segment", "meta_data_dataframe", "meta_data_cross_item",
                              "my_storr_object"))
      .options <- options() #options to be copied to the children (child process)
      .options <- .options[startsWith(names(.options), "dataquieR.")] #only dataquieR options selected

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting options")

      suppressWarnings(parexp(".options"))

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting data frame cache")

      dataframes_list <- as.list(.dataframe_environment())
      suppressWarnings(parexp("dataframes_list"))

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting other caches")

      if (getOption("dataquieR.precomputeStudyData",
                    default = dataquieR.precomputeStudyData_default)) {
        cache_as_list <- as.list(.cache[[".cache"]])
        study_data_cache <- as.list(.study_data_cache)
      } else {
        cache_as_list <- list()
        study_data_cache <- list()
      }

      suppressWarnings(parexp("cache_as_list"))
      suppressWarnings(parexp("study_data_cache"))

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "finalizing setup of compute nodes")

      if (!is.null(q) || !is.null(parallel::getDefaultCluster())) {
        par_eval_q({
          ..glbs <- get(".dq2_globs", envir = asNamespace("dataquieR"))
          ..glbs$.called_in_pipeline <- TRUE
        })
        par_eval_q(options(.options))
        par_eval_q(dataquieR::prep_add_data_frames(
          data_frame_list = dataframes_list))
        par_eval_q({
          assign(
            x = ".cache",
            envir = get(".cache", envir = asNamespace("dataquieR")),
            value = as.environment(cache_as_list))
          list2env(study_data_cache, get(".study_data_cache",
                                         envir = asNamespace("dataquieR")))
        })
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
          data_frame_list = dataframes_list)
        assign(
          x = ".cache",
          envir = get(".cache", envir = asNamespace("dataquieR")),
          value = as.environment(cache_as_list))
      }
      # parallelMap::parallelLapply(1:10, function(x) {dataquieR::prep_list_dataframes()})

      # this exports the static data to the cluster, this is always done, even if
      # already avail. functions don't touch the exported data.
      currentCpus <- parallelMap::parallelGetOptions()$settings$cpus

      worker <- util_eval_to_dataquieR_result
      formals(worker)$filter_result_slots <- filter_result_slots
      force(formals(worker)$filter_result_slots)
      formals(worker)$checkpoint_resumed <- checkpoint_resumed
      force(formals(worker)$checkpoint_resumed)

      if (parallelMap::parallelGetOptions()$settings$mode == "local") {
        environment(worker) <- new.env(parent = asNamespace(utils::packageName()))
      } else if (parallelMap::parallelGetOptions()$settings$mode == "multicore") {
        environment(worker) <- environment()
      } else {
        environment(worker) <- asNamespace(utils::packageName())
      }

      progress_msg("Computation", "computing report")

      n_nodes <- max(1, as.integer(currentCpus[[1]]), na.rm = TRUE)

      if (mode == "futures") { # have/use futures
        r <- util_parallel_futures(
          all_calls = all_calls_cp,
          n_nodes = n_nodes,
          progress = progress,
          worker = worker,
          debug_parallel = debug_parallel,
          my_storr_object = my_storr_object
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
        r <- q$compute_report(all_calls = all_calls_cp,
                              worker = worker,
                              step = step)
      } else {
        r <- util_parallel_classic(
          all_calls = all_calls_cp,
          n_nodes = n_nodes,
          progress = progress,
          worker = worker,
          debug_parallel = debug_parallel,
          my_storr_object = my_storr_object
        )
      }

      if (!dynGet(".is_testing", ifnotfound = FALSE)) {
        util_message(
          sprintf("%s [%s], %s", Sys.time(), "INFO",
                  "DQ -- done")
        ) # TODO: Use RStudio job if available
      }
    }

    r_with_already_computed_r <- setNames(r[names(all_calls)], names(all_calls))
    r_with_already_computed_r[setdiff(names(all_calls), names(r))] <- NA

    r <- r_with_already_computed_r

    my_storr_object <- util_fix_storr_object(my_storr_object)

    if (!is.null(my_storr_object) && length(unclass(r)) > 0 &&
        length(all_calls_cp) > 0 &&
        my_storr_object$exists(NO_SHARED_STORR) &&
        identical(my_storr_object$get(NO_SHARED_STORR), TRUE)
        ) {
      my_storr_object$del(NO_SHARED_STORR)
      util_error("Your storr backend must be shared amongst the compute nodes.")
    }

    progress_msg("Computation", "finalizing report")

    # old <- class(r)
    attr(r, "all_calls") <- all_calls

    class(r) <- union(dataquieR_resultset_class2,
                      "square_results")
    attr(r, "my_storr_object") <- my_storr_object
    # class(r) <- old need?
    if (!is.null(my_storr_object) &&
        inherits(my_storr_object, "storr") &&
        !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
      # to make summary work
      atts_r <- attributes(r)
      atts_r[["my_storr_object"]] <- NULL # dont save this ever
      my_storr_object$mset(key = names(atts_r), value = atts_r, namespace =
                             util_get_storr_att_namespace(my_storr_object))
    }

    if (!is.null(my_storr_object)) {
      my_storr_object$flush_cache()
      # missing_objects <- setdiff(names(r), my_storr_object$list())
      # invisible(lapply(missing_objects, function(mo) {
      #   invisible(util_eval_to_dataquieR_result(
      #       quote({util_error(paste("No result available for unkown reasons",
      #                        "(out of memory? try to reduce the number",
      #                        "of parallel running jobs using the",
      #                        "`cores` argument)"))}),
      #       filter_result_slots = ".*", nm = mo,
      #       my_storr_object = my_storr_object,
      #       function_name = function_names[[mo]],
      #       my_call = all_calls[[mo]], checkpoint_resumed = FALSE))
      # }))
    } else {
      # r[] <- mapply(function_names, all_calls, r, attr(all_calls, "cn"),
      #               i = seq_len(length(function_names)),
      #               i_n = length(function_names),
      #               SIMPLIFY = FALSE,
      #               FUN = function(nm, cl, r, cn, i, i_n) {
      #   # util_message("Fixing %s -- %d of %d", sQuote(nm), i, i_n) # FIXME: SLOW
      #   # added to fix bug in unexpected crashing single jobs.
      #   # if (is.null(r)) {
      #   #    r <-
      #   #      util_eval_to_dataquieR_result(
      #   #        quote({util_error(paste("No result available for unkown reasons",
      #   #                         "(out of memory? try to reduce the number",
      #   #                         "of parallel running jobs using the",
      #   #                         "`cores` argument)"))}),
      #   #        filter_result_slots = ".*", nm = nm,
      #   #        function_name = nm, checkpoint_resumed = FALSE,
      #   #        my_call = cl)
      #   # }
      #
      #   # now done directly, when result is computed:
      #   # attr(r, "function_name") <- nm --> util_eval_to_dataquieR_result
      #   # attr(r, "cn") <- cn
      #   # attr(r, "call") <- cl
      #   ########################################
      #   r
      # })
    }

    # do after itdm, see below:
    # if (!is.null(my_storr_object)) {
    #   namespace = util_get_storr_summ_namespace(my_storr_object)
    #   all_sums <- my_storr_object$mget(my_storr_object$list(
    #     namespace = namespace),
    #     namespace = namespace)
    # } else {
    #   all_sums <- lapply(r, attr, "r_summary")
    # }
    #
    # # c("SummaryData", "SummaryTable", "ReportSummaryTable"), # this is always included (integrity), but later
    # rsn <- sort(unique(unname(unlist(lapply(all_sums, attr, "resnames")))))
    # attr(r, "resnames") <- rsn

    if (!is.null(my_storr_object) &&
        inherits(my_storr_object, "storr") &&
        !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
      # to make summary work
      atts_r <- attributes(r)
      atts_r[["my_storr_object"]] <- NULL # dont save this ever
      my_storr_object$mset(key = names(atts_r), value = atts_r, namespace =
                             util_get_storr_att_namespace(my_storr_object))
    }


    if (any(is.na(names(r)))) {

      nms <- paste(
        vapply(r, attr, "cn", FUN.VALUE = character(1)),
        vapply(lapply(r, attr, "call"), function(cl) {
          attr(cl, "entity_name")
        }, FUN.VALUE = character(1)), sep = ".")

      names(r)[is.na(names(r))] <-
        nms[is.na(names(r))]
      util_stop_if_not(
        `Internal error, sorry, please report: report name inconsistency` =
          all(nms == names(r))
      )

    }

    # overwrite the result for int_datatype_matrix_res
#    if (!is.null(r)) { # TODO: could that happen?

    # add left-out integrity results to the report -----
    if (length(r)) {
      idtm_variable_labels <-
        vapply(lapply(.access_dq_rs2(r, startsWith(names(r), "int_datatype_matrix.")),
                      attr, "call"), attr, "entity_name",
               FUN.VALUE = character(1))

      int_datatype_matrix. <-
        lapply(idtm_variable_labels, function(lab) {
          res <- r[[paste0("int_datatype_matrix.", lab)]]
          attr(res, "error") <- NULL
          attr(res, "warning") <- NULL
          attr(res, "message") <- NULL
          if (!dt_adjust) {
            attr(res, "error") <- list(attr(try(util_error(
              c("data type check was disabled",
                "(argument dt_adjust or option dataquieR.dt_adjust)"),
              applicability_problem = TRUE),
              silent = TRUE), "condition"))
            attr(res, "error")[[1]]$trace <- NULL
            res$SummaryTable <- NULL
            res$SummaryData <- NULL
            res$ReportSummaryTable <- NULL
            class(res) <- union("dataquieR_NULL", class(res))
          } else {
            res$SummaryTable <-
              int_datatype_matrix_res$SummaryTable[
                int_datatype_matrix_res$SummaryTable$Variables == lab,
                , FALSE
              ]

            res$SummaryData <-
              int_datatype_matrix_res$SummaryData[
                int_datatype_matrix_res$SummaryData$Variables == lab,
                , FALSE
              ]

            res$ReportSummaryTable <- NULL
            res$ReportSummaryTable <-
              int_datatype_matrix_res$ReportSummaryTable[
                int_datatype_matrix_res$ReportSummaryTable$Variables == lab,
                , FALSE
              ]
          }
          my_conds <- conds[vapply(conds, function(cnd) {
            (identical(attr(cnd, "varname"), lab)) ||
              (is.null(cnd))
          }, FUN.VALUE = logical(1))]

          if (length(my_conds) > 0) {
            for (cnd in my_conds) {
              if (inherits(cnd, "error")) {
                attr(res, "error") <- c(attr(res, "error"),
                                        list(cnd))
              }
              if (inherits(cnd, "warning")) {
                attr(res, "warning") <- c(attr(res, "warning"),
                                          list(cnd))
              }
              if (inherits(cnd, "message")) {
                attr(res, "message") <- c(attr(res, "message"),
                                          list(cnd))
              }
            }
          }

          res
        })
      #    }

      int_datatype_matrix. <- mapply(SIMPLIFY = FALSE,
                                     r = int_datatype_matrix.,
                                     nm = names(int_datatype_matrix.),
                                     function(r, nm) {
                                       s <- prep_extract_summary(r)
                                       r_summary1 <-
                                         suppressWarnings(
                                           prep_summary_to_classes(s)) # FIXME: split this function, we do not want classes, here

                                       CAT_ <-
                                         vapply(setNames(nm = c("applicability", "error", "anamat", "indicator_or_descriptor")), function(aspect) {
                                           as.character(as.numeric(util_as_cat(util_get_category_for_result(r, aspect = aspect))))
                                         }, FUN.VALUE = character(1))

                                       MSG_ <-
                                         vapply(setNames(nm = c("applicability", "error", "anamat", "indicator_or_descriptor")), function(aspect) {
                                           util_get_message_for_result(r, aspect = aspect)
                                         }, FUN.VALUE = character(1))

                                       names(CAT_) <- paste0("CAT_", names(CAT_))
                                       names(MSG_) <- paste0("MSG_", names(MSG_))

                                       # rownames(CAT_) <- NULL
                                       # rownames(MSG_) <- NULL
                                       # rownames(r) <- NULL

                                       # if (is.data.frame(r_summary) && nrow(r_summary) > 0) {
                                       #   r_summary <- cbind.data.frame(r_summary, t(CAT_), t(MSG_))
                                       # } else {
                                       r_summary <- cbind.data.frame(t(CAT_), t(MSG_))
                                       # }
                                       my_call <- attr(r, "call")
                                       r_summary <- data.frame(
                                         VAR_NAMES = unname(attr(my_call, VAR_NAMES)),
                                         STUDY_SEGMENT = unname(attr(my_call, STUDY_SEGMENT)),
                                         call_names = unname(attr(r, "cn")),
                                         value = as.character(r_summary),
                                         values_raw = as.character(r_summary),
                                         function_name = "int_datatype_matrix",
                                         indicator_metric = names(r_summary)
                                       )
                                       r_summary <- util_rbind(r_summary, r_summary1)

                                       r_summary$function_name <- "int_datatype_matrix"

                                       attr(r_summary, "resnames") <- names(r)
                                       attr(r, "r_summary") <- r_summary
                                       if (!is.null(my_storr_object) &&
                                           inherits(my_storr_object, "storr") &&
                                           !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
                                         my_storr_object$set(key = nm, value = r)
                                         my_storr_object$set(
                                           key = nm,
                                           value = r_summary,
                                           namespace = util_get_storr_summ_namespace(my_storr_object))
                                         my_storr_object$flush_cache()
                                         try(my_storr_object$driver$disconnect(), silent = TRUE)
                                         try(my_storr_object$driver$env$sync(force = TRUE), silent = TRUE)

                                         r <- NA
                                       } else if (!is.null(my_storr_object)) {
                                         r <- "Invalid storr object"
                                       }
                                       r
                                     })

      if (is.null(my_storr_object)) { # only the RAM based version needs this line, otherwise, we overwrite the results also in the back-end with NAs, but we won't write the r_summary, then
        .access_dq_rs2(r, startsWith(names(r), "int_datatype_matrix.")) <-
          int_datatype_matrix. # TODO: make the access function aware of potential summary attributes and write in such cases also the summary to the back-end to make the condtions here unneeded
      }

      # do after itdm, see above
      if (!is.null(my_storr_object)) {
        namespace = util_get_storr_summ_namespace(my_storr_object)
        all_sums <- my_storr_object$mget(my_storr_object$list(
          namespace = namespace),
          namespace = namespace)
      } else {
        all_sums <- lapply(r, attr, "r_summary")
      }

      rsn <- sort(unique(unname(unlist(lapply(all_sums, attr, "resnames")))))
      attr(r, "resnames") <- rsn


      # overwrite the result for int_datatype_matrix_res / DONE
    }

    # make report compatible with old Square2 reports -----

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
      des = "Descriptors",
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
      des = 0,
      int = 1,
      com = 2,
      con = 3,
      acc = 4
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

    vo <- as.numeric(meta_data[[VARIABLE_ORDER]])
    if (0 == length(vo)) {
      vo <- seq_len(nrow(meta_data)) * 10
    }
    offset <- max(vo, na.rm = TRUE) + 1
    if (is.infinite(offset))
      offset <- 1
    vo[util_empty(vo)] <- offset + seq_len(sum(util_empty(vo)))
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

    # FIXME: ensure, that no study data are exposed, here. Flag all meta data frames in the cache as exportable.

    refs <- refs[!util_empty(refs)]
    refs <- gsub(sprintf("\\s*\\%s\\s*", SPLIT_CHAR),
                 SPLIT_CHAR, refs)
    refs <- gsub(sprintf("(\\%s.*?)\\%s.*$", SPLIT_CHAR, SPLIT_CHAR),
                 "\\1", refs)
    refs <- unique(sort(refs))

    refs <- lapply(setNames(nm = refs), function(dfn) {
      r <- data.frame(`NA` = paste(dQuote(dfn), "is not available."),
                      check.names = FALSE)
      if (!getOption("dataquieR.non_disclosure",
                     dataquieR.non_disclosure_default)) { # this is anyway removed below, if the non-disclosure option is TRUE, but we do not needto add it first.
        try(
          r <- prep_get_data_frame(dfn),
          silent = TRUE
        )
      }
      r
    })

    attr(r, "referred_tables") <- refs

    # add information on the dimension names ----

    attr(r, "study_data_dimnames") <- dimnames(study_data)
    attr(r, "cn") <- attr(all_calls, "cn")
    attr(r, "rn") <- attr(all_calls, "rn")
    attr(r, "integrity_issues_before_pipeline") <- conds

    attr(r, "dt_adjust") <- dt_adjust

    # class is to be compatible with Square2 -----

    class(r) <- union(dataquieR_resultset_class2,
                      "square_results")

    if (getOption("dataquieR.non_disclosure",
                  dataquieR.non_disclosure_default)) {
      progress_msg("Computation", "undisclosing report")
      r <- util_undisclose(r)
    }

    progress_msg("Computation", "finished")

    # Return report ----
    r
}

NO_SHARED_STORR <- "NO_SHARED_STORR"
