#' Generate a full DQ report, v2
#'
#' @inheritParams .template_function_indicator
#'
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for [parallelMap::parallelStart] or NULL,
#'                        if parallel has already been started by the caller.
#'                        Can also be a cluster.
#' @param ignore_empty_vars [enum] TRUE | FALSE | auto. See
#'                             [dataquieR.ignore_empty_vars].
#' @param specific_args [list] named list of arguments specifically for one of
#'                             the called functions, the of the list elements
#'                             correspond to the indicator functions whose calls
#'                             should be modified. The elements are lists of
#'                             arguments.
#' @param dimensions [dimensions] Vector of dimensions to address in the report.
#'                   Allowed values in the vector are Completeness, Consistency,
#'                   and Accuracy. The generated report will only cover the
#'                   listed data quality dimensions. Accuracy is computational
#'                   expensive, so this dimension is not enabled by default.
#'                   Completeness should be included, if Consistency is
#'                   included, and Consistency should be included, if Accuracy
#'                   is included to avoid misleading detections of e.g. missing
#'                   codes as outliers, please refer to the data quality concept
#'                   for more details. Integrity is always included.
#'                   If dimensions is equal to NULL or "all", all dimensions
#'                   will be covered.
#' @param author [character] author for the report documents.
#' @param debug_parallel [logical] print blocks currently evaluated in parallel
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_dataframe [data.frame] -- optional: Data frame level
#'                                                                 metadata
#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level
#'                                                                 metadata
#' @param user_info [list] additional info stored with the report, e.g.,
#'                         comments, title, ...
#' @param resp_vars [variable list] the name of the measurement variables
#'                                  for the report. If missing, all variables
#'                                  will be used. Only item level indicator
#'                                  functions are filtered, so far.
#' @param filter_indicator_functions [character] regular expressions, only
#'                                               if an indicator function's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param filter_result_slots [character] regular expressions, only
#'                                               if an indicator function's
#'                                               result's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param mode [character] work mode for parallel execution. default is
#'              "default", the values mean:
#'              - default: use `queue` except `cores` has been set explicitly
#'              - futures: use the `future` package
#'              - queue: use a queue as described in the examples
#'                from the `callr` package by Csárdi and Chang and start
#'                sub-processes as workers that evaluate the queue.
#'              - parallel: use the cluster from `cores` to evaluate all
#'                         calls of indicator functions using the classic
#'                         R `parallel` back-ends
#'
#' @param mode_args [list] of arguments for the selected `mode`. As of writing
#'                         this manual, only for the mode `queue` the argument
#'                         `step` is supported, which gives the number of
#'                         function calls that are run by one worker at a time.
#'                         the default is 15, which gives on most of the tested
#'                         systems a good balance between synchronization
#'                         overhead and idling workers.
#' @param notes_from_wrapper [list] a list containing notes about changed labels
#'                                  by `dq_report_by` (otherwise NULL)
#' @param title [character] optional argument to specify the title for
#'                          the data quality report
#' @param subtitle [character] optional argument to specify a subtitle for
#'                             the data quality report
#' @param advanced_options [list] options to set during report computation,
#'                                see [options()]
#' @param meta_data_item_computation [data.frame] optional. computation rules
#'                                              for computed variables.
#' @param storr_factory [function] `NULL`, or
#'                        a function returning a `storr` object as
#'                        back-end for the report's results. If used with
#'                        `cores > 1`, the storage must be accessible from all
#'                        cores and capable of concurrent writing according
#'                        to `storr`. Hint: `dataquieR` currently only supports
#'                        `storr::storr_rds()`, officially, while other back-
#'                        ends may nevertheless work, yet, they are not tested.
#' @param amend [logical] if there is already data in.`storr_factory`,
#'                        use it anyways -- unsupported, so far!
#' @param checkpoint_resumed [logical] if using a `storr_factory` and the back-
#'                                     end there is already filled, and if
#'                                     `amend` is missing or set to `TRUE`,
#'                                     compute all missing result and add them
#'                                     to the back-end.
#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`
#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`
#' @param segment_level [data.frame] alias for `meta_data_segment`
#' @param dataframe_level [data.frame] alias for `meta_data_dataframe`
#' @param item_computation_level [data.frame] alias for
#'                               `meta_data_item_computation`
#' @param .internal [logical] internal use, only.
#' @param name_of_study_data [character] name for study data inside the report,
#'                                       internal use.
#' @param dt_adjust [logical] whether to trust data types in the study data. if
#'                            `TRUE`, data types are checked based on the
#'                            metadata and later casted to the declared type.
#'                            if your data source is already typed, this can
#'                            be turned off to speed up computations.
#'                            see [dataquieR.dt_adjust]
#'
#' @return a [dataquieR_resultset2] that can be
#' [printed][print.dataquieR_resultset2] creating a `HTML`-report.
#'
#' @details
#' See [dq_report_by] for a way to generate stratified or splitted reports
#' easily.
#'
#' @seealso
#' `r paste0(" * [", methods(class="dataquieR_resultset"), "]", collapse="\n")`
#' * [dq_report_by]
#' @export
#' @importFrom stats alias
#' @importFrom utils osVersion packageName packageVersion
#' @importFrom stats setNames
dq_report2 <- function(study_data, # TODO: make meta_data_segment, ... optional
                       item_level = "item_level",
                       label_col = LABEL,
                       meta_data_segment = "segment_level",
                       meta_data_dataframe = "dataframe_level",
                       meta_data_cross_item = "cross-item_level",
                       meta_data_item_computation =
                         "item_computation_level",
                       meta_data = item_level,
                       meta_data_v2,
                       ...,
                       dimensions = c("Completeness", "Consistency"),
                       cores = list(mode = "socket",
                                    logging = FALSE,
                                    cpus = util_detect_cores(),
                                    load.balancing = TRUE),
                       ignore_empty_vars =
                         getOption("dataquieR.ignore_empty_vars",
                                   dataquieR.ignore_empty_vars_default),
                       specific_args = list(), # TODO: check if list of lists, or support not only function but also call names w/o var-suffix in this list: specific_args = list(acc_margins_observer = list()) or specific_args = list(acc_margins_observer.SBP_0 = list())
                       advanced_options =  list(),
                       author = prep_get_user_name(),
                       title = "Data quality report",
                       subtitle = as.character(Sys.Date()),
                       user_info = NULL,
                       debug_parallel = FALSE,
                       resp_vars = character(0),
                       filter_indicator_functions = character(0),
                       filter_result_slots = c(
                         "^Summary",
                         "^Segment",
                         "^DataTypePlotList",
                         "^ReportSummaryTable",
                         "^Dataframe",
                         "^Result",
                         "^VariableGroup"
                       ),
                       mode = c("default", "futures", "queue", "parallel"),
                       mode_args = list(),
                       notes_from_wrapper = list(),
                       storr_factory = NULL,
                       amend = FALSE, # TODO: reduced output (png plus table): util_plot2svg_object would do with svg, png version was tested, but uses typically much more memory. Function with this commit but removed thereafter. use lobstr::obj_size() for testing w/o save() :)
                       cross_item_level,
                       `cross-item_level`,
                       segment_level,
                       dataframe_level,
                       item_computation_level,
                       .internal =
                         rlang::env_inherits(
                           rlang::caller_env(),
                           parent.env(environment())),
                       checkpoint_resumed =
                         getOption("dataquieR.resume_checkpoint",
                                   dataquieR.resume_checkpoint_default),
                       name_of_study_data,
                       dt_adjust = as.logical(getOption("dataquieR.dt_adjust",
                                             dataquieR.dt_adjust_default))) { # TODO: on.exit(rstudioapi::executeCommand("activateConsole")), and also in the print()-method
  util_stop_if_not(is.list(advanced_options))

  old_O <- options(
    c(
      list(
        dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
        dataquieR.ERRORS_WITH_CALLER = FALSE,
        dataquieR.MESSAGES_WITH_CALLER = FALSE,
        dataquieR.WARNINGS_WITH_CALLER = FALSE
      ),
      advanced_options
    )
  )
  on.exit(options(old_O))

  my_storr_object <- util_storr_object(storr_factory)

  util_expect_scalar(dt_adjust, check_type = is.logical)
  util_expect_scalar(amend, check_type = is.logical)
  util_expect_scalar(checkpoint_resumed, check_type = is.logical)

  if (missing(amend) && !amend && checkpoint_resumed) {
    util_message(c("%s was %s, but %s was unset (default = %s).",
                   "I will set %s to %s to make %s work."),
                 sQuote("checkpoint_resumed"),
                 sQuote("TRUE"),
                 sQuote("amend"),
                 sQuote("FALSE"),
                 sQuote("amend"),
                 sQuote("TRUE"),
                 sQuote("checkpoint_resumed"))
    amend = TRUE
  } else if (!missing(amend) && !amend && checkpoint_resumed) {
    util_message(c("%s was %s, but %s was %s.",
                   "I will set %s to %s to avoid results from being touched",
                   "(%s = %s)"),
                 sQuote("amend"),
                 sQuote("FALSE"),
                 sQuote("checkpoint_resumed"),
                 sQuote("TRUE"),
                 sQuote("checkpoint_resumed"),
                 sQuote("FALSE"),
                 sQuote("amend"),
                 sQuote("FALSE")
                 )
    checkpoint_resumed <- FALSE
  }

  if (!is.null(my_storr_object) && (
    length(my_storr_object$list()) > 0 ||
    length(my_storr_object$list(
      util_get_storr_att_namespace(my_storr_object))) > 0 ||
    length(my_storr_object$list(
      util_get_storr_summ_namespace(my_storr_object))) > 0
  )) {
    if (amend) {
      util_message(c("Your storr-object is not empty, but %s was set %s,",
                     "so I'll amend the storage object. This is unsupported,",
                     "yet, so expect strange behavior."),
                   dQuote("amend"), sQuote(TRUE))
    } else {
      util_error(c("Your storr-object is not empty, and %s was set %s,",
                   "so I won't amend the storage object, which would",
                   "still be unsupported, so could cause strange behavior.",
                   "We strongly recommend to use clear storr objects (or",
                   "at least the default namespace (%s in your case)",
                   "and its sister namespaces (the default namespace suffixed",
                   "with %s and %s, should be empty. In case of %s, just",
                   "delete the folder that backs the storr."),
                 dQuote("amend"),
                 sQuote(FALSE),
                 sQuote(my_storr_object$default_namespace),
                 sQuote(".attributes"),
                 sQuote(".summary"),
                 sQuote("driver_rds")
                 )
    }
  }

  util_match_arg(ignore_empty_vars,
                 c("TRUE", "FALSE", "auto"))

  mode <- util_match_arg(mode)

  if (missing(title)) {
    attr(title, "default") <- TRUE
  } else {
    attr(title, "default") <- FALSE
  }

  if (missing(subtitle)) {
    attr(subtitle, "default") <- TRUE
  } else {
    attr(subtitle, "default") <- FALSE
  }

  util_expect_scalar(title, check_type = is.character,
                     error_message = sprintf("%s needs to be character(1)",
                                             sQuote("title")))

  util_expect_scalar(subtitle, check_type = is.character,
                     error_message = sprintf("%s needs to be character(1)",
                                             sQuote("subtitle")))

  if (!is.null(cores) && (missing(cores) || (
    is.vector(cores) && length(cores) == 1 && util_is_integer(cores)) &&
    cores > 1) &&
      mode == "default") {
    if (util_ensure_suggested(c("R6", "processx"),
                              goal =
     "use the queue mode, which is faster than parallel (but cannot run on distributed cluster nodes)",
                              err = FALSE)) {
      mode <- "queue"
    } else {
      mode <- "parallel"
    }
  } else if (mode == "default") {
      mode <- "parallel"
  }

  if (suppressWarnings(util_ensure_suggested("testthat", err = FALSE))) {
    if (testthat::is_testing()) {
      if (!(mode %in% c("parallel", "queue"))) {
        util_warning(
          "Internal problem: %s should be %s, %s or %s in the context of %s",
                     sQuote("mode"), dQuote("queue"), dQuote("parallel"),
          dQuote("default"),
          sQuote("testthat"))
      }
      if (!rlang::is_scalar_integerish(cores) ||
          cores > 1L) {
        if (!is.null(cores))
          util_warning(
            "Internal problem: %s should be an integer below %s in the context of %s",
            sQuote("cores"), dQuote("2"), sQuote("testthat"))
      }
    }
  }

  if (!missing(meta_data_v2)) {
    util_message("Have %s set, so I'll remove all loaded data frames",
                 sQuote("meta_data_v2"))
    prep_purge_data_frame_cache()
    prep_load_workbook_like_file(meta_data_v2)
    if (!exists("item_level", .dataframe_environment())) {
      w <- paste("Did not find any sheet named %s in %s, is this",
             "really dataquieR version 2 metadata?")
      if (requireNamespace("cli", quietly = TRUE)) {
        w <- cli::bg_red(cli::col_br_yellow(w))
      }
      util_warning(w, dQuote("item_level"), dQuote(meta_data_v2),
                   immediate = TRUE)
    }
  }

  # checks and fixes the function arguments
  util_ck_arg_aliases()

  if (missing(name_of_study_data)) {
    if (is.data.frame(study_data)) {
      name_of_study_data <- substr(head(as.character(substitute(study_data)), 1),
                                   1, 500)
    } else if (length(study_data) == 1 && is.character(study_data)) {
      name_of_study_data <- study_data
    } else {
      name_of_study_data <- "??No study data found??"
    }
  } else {
    util_expect_scalar(name_of_study_data,
                       check_type = is.character)
    substr(name_of_study_data, 1, 500)
  }
  util_expect_data_frame(study_data, keep_types = TRUE)
#  if (ncol(study_data) == 0) {
#    util_error("Study data has no column")
#  }
  prep_add_data_frames(data_frame_list = setNames(list(study_data),
                                                  nm = name_of_study_data))
  prep_add_data_frames(data_frame_list = setNames(list(study_data),
                                                  nm = "study_data"))

  util_handle_val_tab()

  # checks the data frame names in the dataframe cache
  if (!.internal) {
    util_verify_names(name_of_study_data = name_of_study_data)
  }

  try(util_expect_data_frame(meta_data), silent = TRUE)

  case_insens <- util_is_na_0_empty_or_false(
    getOption("dataquieR.study_data_colnames_case_sensitive",
              dataquieR.study_data_colnames_case_sensitive_default))

  if (case_insens) {
    colnames(study_data) <-
      util_align_colnames_case(.colnames = colnames(study_data),
                               .var_names = meta_data[[VAR_NAMES]])
  }

  ci_in_study <- FALSE
  if (is.data.frame(meta_data) && !case_insens) {
    in_study <- meta_data[[VAR_NAMES]] %in% colnames(study_data)
    ci_in_study <- tolower(meta_data[[VAR_NAMES]]) %in%
      tolower(colnames(study_data))
  }
  if (identical(getOption("dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
                dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default), "subset_u")) {
    if (is.data.frame(meta_data)) {
      meta_data <- meta_data[in_study, , FALSE]
    }
  }

  # try, meta_data my still be missing
  # note: we had the following twice, first w/o try, then w/ try. likely a bug
  try(meta_data <- prep_meta_data_v1_to_item_level_meta_data(meta_data),
                   silent = TRUE)
  warning_pred_meta <- NULL
  if (!is.data.frame(meta_data) || !prod(dim(meta_data))) {
    try_ci <- ""
    if (!case_insens) {
      if (any(ci_in_study)) {
        try_ci <- sprintf(
          paste("But maybe, if you enable case-insensitive mapping",
                "of meta_data on study data using %s, it could work?"),
          sQuote(
            "options(dataquieR.study_data_colnames_case_sensitive = FALSE)")
        )
      }
    }
    w <- paste("No item level metadata matching study data found. Will guess",
           "some from the study data. This will not be very helpful, please",
           "consider passing an item level metadata file.",
           try_ci)
    if (requireNamespace("cli", quietly = TRUE)) {
      w <- cli::bg_red(cli::col_br_yellow(cli::ansi_toupper(w)))
      w <- gsub("%S", "%s", w)
    }
    util_warning(w,
                 immediate = TRUE)
    predicted <- prep_study2meta(study_data, convert_factors = TRUE)
    meta_data <- predicted$MetaData
    study_data <- predicted$ModifiedStudyData
    warning_pred_meta <- paste("No item-level metadata matching study data",
                               "could not be found and was guessed from the",
                               "study data.", try_ci)
  } else {
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data) <- NULL
  }
  try(util_expect_data_frame(meta_data_segment), silent = TRUE)
  if (!is.data.frame(meta_data_segment)) {
    util_message("No segment level metadata %s found",
                 dQuote(meta_data_segment))
    meta_data_segment <- data.frame(STUDY_SEGMENT =
                                      unique(meta_data$STUDY_SEGMENT))
  } else {
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data_segment) <- NULL
  }
  try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
  if (!is.data.frame(meta_data_dataframe)) {
    util_message("No dataframe level metadata %s found.",
                 dQuote(meta_data_dataframe))
    meta_data_dataframe <- data.frame(DF_NAME =
                                        name_of_study_data)
  } else {
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data_dataframe) <- NULL
  }
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message("No cross-item level metadata %s found",
                   dQuote(meta_data_cross_item))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                      CHECK_LABEL = character(0))
  } else {
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data_cross_item) <- NULL
  }

  suppressWarnings(util_ensure_in(VAR_NAMES, names(meta_data), error = TRUE,
                 err_msg =
                   sprintf("Did not find the mandatory column %%s in the %s.",
                           sQuote("meta_data"))))

  if (!(VARIABLE_ROLE %in% colnames(meta_data))) {
    util_message("No %s assigned in item level metadata. Defaulting to %s.",
                 sQuote(VARIABLE_ROLE), dQuote(VARIABLE_ROLES$PRIMARY),
                 applicability_problem = TRUE)
    meta_data$VARIABLE_ROLE <- VARIABLE_ROLES$PRIMARY
  }

  which_not <- !(meta_data[[VARIABLE_ROLE]] %in% VARIABLE_ROLES)
  if (any(which_not)) { # Also for NA cells
    util_message(
         c("The variables %s have no or an invalid %s assigned in item level",
           "metadata: %s are not in %s. Defaulting to %s."), # TODO: normalize VARIABLE_ROLES
         util_pretty_vector_string(n_max = 5, meta_data[which_not, label_col,
                                               drop = TRUE]),
         sQuote(VARIABLE_ROLE),
         dQuote(util_pretty_vector_string(sort(unique(meta_data[which_not,
                                                         VARIABLE_ROLE,
                                             drop = TRUE])))),
         util_pretty_vector_string(VARIABLE_ROLES),
         dQuote(VARIABLE_ROLES$PRIMARY),
             applicability_problem = TRUE)
    meta_data$VARIABLE_ROLE[which_not] <- VARIABLE_ROLES$PRIMARY
  }

  util_expect_scalar(label_col, check_type = is.character)
  util_ensure_in(label_col, names(meta_data), error = TRUE,
                 err_msg =
        sprintf("Did not find a label column (%s) named %%s in the %s. Did you mean %%s?",
                sQuote("label_col"),
                    sQuote("meta_data")))

  try(util_expect_data_frame(meta_data_item_computation), silent = TRUE)
  if (!is.data.frame(meta_data_item_computation)) {
    meta_data_item_computation <- data.frame()
  }
  uaci <-
    util_add_computed_internals(meta_data_item_computation,
                                meta_data_cross_item,
                                meta_data,
                                label_col)
  meta_data_item_computation <-
    uaci$meta_data_item_computation
  meta_data <- uaci$meta_data
  if (is.data.frame(meta_data_item_computation)) {
    util_message("Computed items metadata defined. Computing them...")
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data_item_computation) <- NULL
    function_e <- environment()
    status <- try(local({
      res <- prep_add_computed_variables(
        study_data = study_data,
        meta_data = meta_data,
        label_col = label_col,
        rules = meta_data_item_computation
      )
      msd <- res$ModifiedStudyData[, meta_data_item_computation$VAR_NAMES,
                                   drop = FALSE]
      mapped <- identical(attr(res$ModifiedStudyData, "MAPPED"), TRUE)
      if (mapped) {
        mapped_lc <- attr(res$ModifiedStudyData, "label_col")
        colnames(msd) <- prep_map_labels(colnames(msd),
                               meta_data = meta_data,
                               to = VAR_NAMES,
                               from = mapped_lc,
                               ifnotfound = colnames(msd),
                               warn_ambiguous = FALSE)
      }
      function_e$study_data[,
                            setdiff(colnames(msd),
                                    colnames(function_e$study_data))] <-
        msd[, setdiff(colnames(msd),
                      colnames(function_e$study_data))]
      invisible(NULL)
    }), silent = TRUE)
    if (inherits(status, "try-error")) {
      util_warning(status)
    } else {
      prep_add_data_frames(data_frame_list = setNames(list(study_data),
                                                      nm = name_of_study_data))
      prep_add_data_frames(data_frame_list = setNames(list(study_data),
                                                      nm = "study_data"))
    }
  } else {
    meta_data_item_computation <- NULL
  }

  # ensure that VAR_NAMES and labels exist, are unique and not too long
  mod_label <- util_ensure_label(meta_data = meta_data,
                                 label_col = label_col)
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    meta_data <- mod_label$meta_data
    label_col <- mod_label$label_col
  }
  # Since we may also map to other metadata label columns, we have to ensure
  # that none of them contains empty fields to prevent errors.
  for (lcol in unique(c(label_col, LABEL, LONG_LABEL))) {
    if (is.data.frame(meta_data) && lcol %in% colnames(meta_data)) {
      meta_data[[lcol]][which(util_empty(meta_data[[lcol]]))] <-
        meta_data[[VAR_NAMES]][which(util_empty(meta_data[[lcol]]))]
    }
  }

  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  util_expect_scalar(dimensions,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character,
                     error_message =
                       sprintf("The argument %s must be character or NULL",
                               sQuote("dimensions")))
  if (length(dimensions) == 0 || (length(dimensions) == 1 &&
                                  !is.na(dimensions) &&
                                  tolower(trimws(dimensions)) == "all")) {
    dimensions <- c("completeness", "consistency", "accuracy")
  } else {
    dimensions <- tolower(dimensions)
  }
  dimensions[dimensions %in% c("acc", "accuracy")] <- "Accuracy"
  dimensions[dimensions %in% c("con", "consistency")] <- "Consistency"
  dimensions[dimensions %in% c("com", "completeness")] <- "Completeness"
  dimensions[dimensions %in% c("int", "integrity")] <- "Integrity"
  dimensions[dimensions %in% c("des", "descriptors")] <- "Descriptors"
  .dimensions <-
    util_ensure_in(dimensions,
                   c("Completeness", "Consistency", "Accuracy", "Integrity",
                     "Descriptors"),
                   error = FALSE,
                   applicability_problem = TRUE
              )

  util_expect_scalar(resp_vars,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  md100 <- meta_data # metadata with study data

  miss_from_study <- (!(md100[[VAR_NAMES]] %in% (c(colnames(study_data),
                                                   meta_data_item_computation$VAR_NAMES))))

  only_nas <- vapply(setNames(md100[[VAR_NAMES]], nm = md100[[label_col]]),
                     function(vn) {
                       all(util_empty(study_data[[vn]]))
                     },
                     FUN.VALUE = logical(1))

  if (any(miss_from_study)) {
    vars_not_found <- paste0(
      dQuote(paste0(md100[miss_from_study, label_col], " (",
                    md100[miss_from_study, VAR_NAMES], ")")),
      collapse = ", ")
    util_message(
      c("Could not find the following variables in %s:",
        "%s.\nThese will be preliminarily removed from the %s."),
      sQuote("study_data"),
      vars_not_found,
      sQuote("meta_data")
    )
    md100 <- md100[!miss_from_study, , FALSE]
  }

  if (length(resp_vars) == 0) {
    resp_vars <- md100[[label_col]] # TODO: Sort by VAR_ORDER?
  } else {
    resp_vars_m <- util_find_var_by_meta(
      resp_vars = resp_vars,
      meta_data = md100,
      label_col = label_col,
      # allowed_sources = ,
      target = label_col,
      ifnotfound = NA_character_)
    if (any(is.na(resp_vars_m))) {
      util_warning(
        c("Could not find the following variables in %s:",
          "%s.\nThese will be removed."),
        sQuote("meta_data"),
        paste0(
          dQuote(resp_vars[is.na(resp_vars_m)]),
          collapse = ", ")
      )
    }
    resp_vars <- resp_vars_m[!is.na(resp_vars_m)]
  }

  to_remove <-
    intersect(resp_vars, names(which(only_nas)))

  orig_ignore_empty_vars <- ignore_empty_vars

  if (length(resp_vars) == 0) {
    util_error("No response variables left.")
  }

  if (ignore_empty_vars == "auto") {
    ignore_empty_vars <- length(to_remove) / length(resp_vars) > .2 # 20%, see options for documentation, search for ignore_empty_vars20, if changing
  } else {
    ignore_empty_vars <- as.logical(ignore_empty_vars)
  }

  if (ignore_empty_vars) {
    util_warning(
      c("%s was %s, so removing the following variables from the report,",
        "because they only feature empty values: %s"),
      sQuote("ignore_empty_vars"),
      sQuote(orig_ignore_empty_vars),
      util_pretty_vector_string(to_remove, n_max = 6), immediate = TRUE)
    resp_vars <- setdiff(resp_vars, to_remove)
  } else if (length(to_remove) > 0) {
    util_message("Have variables only featuring empty data values: %s",
                 util_pretty_vector_string(to_remove, n_max = 6),
                 immediate = TRUE)
  }

  util_message("Pre-computing curated study data frames...")

  util_reset_cache()
  if (getOption("dataquieR.precomputeStudyData", default =
                dataquieR.precomputeStudyData_default)) {
    util_populate_study_data_cache(study_data, meta_data, label_col = LABEL)
  } else {
    util_purge_study_data_cache()
  }

  util_message("Pre-computing curated study data frames... done")

  util_expect_scalar(filter_indicator_functions,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)
  util_expect_scalar(filter_result_slots,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  scale_level_predicted <- FALSE

  if (!(SCALE_LEVEL %in% colnames(meta_data)) ||
      any(util_empty(meta_data[[SCALE_LEVEL]]))) {
    util_message("Estimating %s...", sQuote(SCALE_LEVEL))
    scale_level_predicted <- TRUE
    function_e <- environment()
    local(suppressWarnings(suppressMessages({
      prep_prepare_dataframes(.replace_hard_limits = FALSE,
                              .replace_missings = FALSE,
                              .adjust_data_type = TRUE,
                              .amend_scale_level = TRUE,
                              .meta_data = meta_data)
      vec_sl <- setNames(meta_data[[SCALE_LEVEL]],
                         nm = meta_data[[VAR_NAMES]])
      function_e$meta_data[[SCALE_LEVEL]] <-
        vec_sl[function_e$meta_data[[VAR_NAMES]]]
    })))
    util_message("Estimating %s... done", sQuote(SCALE_LEVEL))
  }

  all_calls <- util_generate_calls(dimensions = dimensions,
                                   meta_data = meta_data,
                                   label_col = label_col,
                                   meta_data_segment = meta_data_segment,
                                   meta_data_dataframe = meta_data_dataframe,
                                   meta_data_cross_item = meta_data_cross_item,
                                   specific_args = specific_args,
                                   arg_overrides = list(...),
                                   filter_indicator_functions =
                                     filter_indicator_functions,
                                   resp_vars = resp_vars)

  tm <- system.time(
    r <- util_evaluate_calls(
      cores = cores,
      all_calls = all_calls,
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      meta_data_segment = meta_data_segment,
      meta_data_dataframe = meta_data_dataframe,
      meta_data_cross_item = meta_data_cross_item,
      debug_parallel = debug_parallel,
      resp_vars = resp_vars,
      filter_result_slots = filter_result_slots,
      mode = mode,
      mode_args = mode_args,
      my_storr_object = my_storr_object,
      checkpoint_resumed = checkpoint_resumed,
      dt_adjust = dt_adjust
    )
  )

  start_from_call <- util_find_first_externally_called_functions_in_stacktrace()
  start_from_call <- length(sys.calls()) - start_from_call # refers to reverted sys.calls, so mirror the number
  if (is.na(start_from_call))
    start_from_call <- 1
  cl <- NULL
  try({
    cl <- sys.call(start_from_call)
  }, silent = TRUE)

  # get call for dq_report_by
  get_call <- try(deparse(sys.call(-10)), silent= TRUE)
  if (!inherits(get_call , "try-error")) {
    if (startsWith(paste(deparse(sys.call(-10)), collapse = ""),
                   "dq_report_by")) {
      cl <- eval.parent(quote({call_report_by}))
    }
  }

  p <- list( # TODO: add this also in Square2
    author = author,
    date = Sys.time(),
    call = cl, # TODO: Why does this not yet work?
    version = paste(packageName(), util_dataquieR_version()),
    R = R.version.string,
    os = osVersion,
    machine = paste(Sys.info()[["nodename"]],
                    sprintf("(%s)", Sys.info()[["version"]]),
                    Sys.info()["machine"]
    ),
    runtime = paste(round(tm[["elapsed"]], 1), "secs")
  )

  if (is.list(user_info)) {
    p <- c(user_info, p)
  }

  dq_report2_env <- environment()
  meta_data_hints <- list()
  capture <- function(cnd) {
    dq_report2_env$meta_data_hints <-
      c(dq_report2_env$meta_data_hints, list(cnd))
    if (inherits(cnd, "warning")) {
      invokeRestart("muffleWarning")
    }
    if (inherits(cnd, "message")) {
      invokeRestart("muffleMessage")
    }
  }

  # TODO: Do we need this if we run prep_prepare_dataframes above?
  suppressWarnings(suppressMessages(
    try(withCallingHandlers(meta_data <- util_validate_known_meta(meta_data),
                           error = capture,
                           warning = capture,
                           message = capture), silent = TRUE)))


  if (scale_level_predicted) { # TODO: sort all the hints and have only one metadata integrity hints object?
    suppressWarnings(suppressMessages(
      try(withCallingHandlers(util_message(c(
        "Missing some or all entries in %s column in item-level %s. Predicting",
                   "it from the data -- please verify these predictions, they",
                   "may be wrong and lead to functions claiming not to be",
                   "reasonably applicable to a variable."),
                 sQuote(SCALE_LEVEL), "meta_data",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = FALSE),
                 error = capture,
                 warning = capture,
                 message = capture), silent = TRUE)))
  }

  if (util_really_rstudio()) {
    rstudioapi::executeCommand("activateConsole")
  }

  if (!is.null(my_storr_object) &&
      inherits(my_storr_object, "storr") &&
      !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
    # to make summary work
    atts_r <- attributes(r)
    atts_r[["my_storr_object"]] <- NULL # dont save this ever
    my_storr_object$mset(key = names(atts_r), value = atts_r, namespace =
                           util_get_storr_att_namespace(my_storr_object))
  }

#  repsum <- summary(r) FIXME: Dead slow!!
repsum <- NA

  r <- util_attach_attr(r,
                   properties = p,
                   min_render_version = as.numeric_version("1.0.0"),
                   warning_pred_meta = warning_pred_meta,
                   label_modification_text = trimws(paste(
                     notes_from_wrapper[["label_modification_text"]],
                     mod_label$label_modification_text)),
                   label_modification_table = rbind(
                     notes_from_wrapper[["label_modification_table"]],
                     mod_label$label_modification_table),
                   label_meta_data_hints = meta_data_hints,
                   meta_data_item_computation = meta_data_item_computation,
                   repsum = repsum,
                   title = title,
                   subtitle = subtitle)

  if (!is.null(my_storr_object) &&
      inherits(my_storr_object, "storr") &&
      !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {

    # to make summary work
    atts_r <- attributes(r)
    atts_r[["my_storr_object"]] <- NULL # dont save this ever
    my_storr_object$mset(key = names(atts_r), value = atts_r, namespace =
                           util_get_storr_att_namespace(my_storr_object))

#    my_storr_object$mset(key = names(r), value = r)

    attr(r, "my_storr_object") <- my_storr_object

  }

  r
}

.study_data_cache <- new.env(parent = emptyenv())
util_purge_study_data_cache <- function() {
  rm(list = ls(.study_data_cache), envir = .study_data_cache)
}

# util_populate_study_data_cache(prep_get_data_frame("study_data"), prep_get_data_frame("meta_data"), LABEL)
#
#
# options(dataquieR.study_data_cache_quick_fill = FALSE)
#
# options(dataquieR.study_data_cache_quick_fill = TRUE)
# options(dataquieR.study_data_cache_metrics = TRUE)
# metrics <- new.env(parent = emptyenv())
# options(dataquieR.study_data_cache_metrics_env = metrics)
# rx <- dq_report2("study_data", meta_data_v2 = "meta_data_v2", dimensions = NULL, cores = NULL)
# to_populate <- lapply(names(metrics$usage), function(key) attr(dataquieR:::.study_data_cache[[key]], "call"))
# cat(unlist(lapply(to_populate, deparse, width.cutoff = 300)), sep = "\n", file = "|pbcopy") # but add ".study_data = study_data, .meta_data = meta_data, .label_col = label_col," everywhere, if missing
# options(dataquieR.study_data_cache_quick_fill = TRUE)
# options(dataquieR.study_data_cache_metrics = FALSE)
# options(dataquieR.study_data_cache_metrics_env = NULL)
# rm(metrics)
# rx <- dq_report2("study_data", meta_data_v2 = "meta_data_v2", dimensions = NULL)
util_populate_study_data_cache <- function(study_data, meta_data, label_col, quick = getOption("dataquieR.study_data_cache_quick_fill", dataquieR.study_data_cache_quick_fill_default)) {
  util_purge_study_data_cache()
  invisible(lapply(study_data, util_is_na_0_empty_or_false)) # TODO: maybe RAM intensive?
  if (quick) {
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_hard_limits = TRUE, .replace_missings = TRUE, .adjust_data_type = TRUE, .amend_scale_level = TRUE))
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_missings = FALSE))
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_missings = FALSE))
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col))
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .allow_empty = TRUE))
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_hard_limits = TRUE))
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_missings = FALSE, .adjust_data_type = FALSE))
  } else {
    combinations <- unlist(lapply(c(0, seq_along(.to_combine)),
                                  function(x) utils::combn(.to_combine, x,simplify = FALSE)), recursive = FALSE)
    calls <- lapply(combinations, function(set_true) {
      res <- .call_template
      res[set_true] <- TRUE
      res
    })
    study_data_hash <- rlang::hash(study_data)
    meta_data_hash <- rlang::hash(meta_data)
    a <- lapply(lapply(lapply(calls, rlang::call_args), `[`, .to_combine),
                rlang::hash)
    names(calls) <-
      paste0(a, "@", study_data_hash,
             "@", meta_data_hash, "@", label_col)

    my_env <- new.env(parent = parent.env(environment()))
    my_env$study_data <- study_data
    my_env$meta_data <- meta_data
    my_env$label_col <- label_col
    # TODO: Remove unneded, rarely used, calls

    # HINT: Do not touch my_env (use new.env(parent = my_env)) to
    #       keep the original study data for all cases.

    # parallelMap::parallelLibrary("dataquieR")
    # parallelMap::parallelExport("study_data", "meta_data", "label_col")
    # for_cache <- parallelMap::parallelLapply(calls,
    #                                          function(cl, my_env) {
    #                                            try(eval(cl, envir = new.env(parent = my_env)),
    #                                                silent = TRUE)
    #                                           },
    #                                          my_env = my_env)
    for_cache <- lapply(calls,
                        function(cl, my_env) { try(util_attach_attr(eval(cl, envir = new.env(parent = my_env)), call = cl),
                                                   silent = TRUE) },
                        my_env = my_env)
    for_cache <- for_cache[vapply(for_cache, is.data.frame,
                                  FUN.VALUE = logical(1))]
    list2env(for_cache, .study_data_cache)
  }
}

.call_template <- quote(prep_prepare_dataframes(
  .study_data = study_data,
  .meta_data = meta_data,
  .label_col = label_col,
  .replace_hard_limits = FALSE,
  .replace_missings = FALSE,
  .adjust_data_type = FALSE,
  .amend_scale_level = FALSE,
  .apply_factor_metadata = FALSE,
  .apply_factor_metadata_inadm = FALSE
))
.to_combine <- setdiff(names(.call_template),
                      c("", ".study_data", ".meta_data", ".label_col"))
