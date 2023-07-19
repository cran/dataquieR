#' Generate a full DQ report, v2
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for [parallelMap::parallelStart] or NULL,
#'                        if parallel has already been started by the caller.
#'                        Can also be a cluster.
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
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param notes_from_wrapper [list] a list containing notes about changed labels
#'                                  by `dq_report_by` (otherwise NULL)
#' @param title [character] optional argument to specify the title for
#'                          the first page of the data quality report
#' @param subtitle [character] optional argument to specify a subtitle for
#'                             the first page of the data quality report
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
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("inst/extdata/meta_data_v2.xlsx")
#' meta_data <- prep_get_data_frame("item_level")
#' meta_data_cross <- prep_get_data_frame("cross-item_level")
#' x <- dq_report2("study_data", dimensions = NULL, label_col = "LABEL")
#' xx <- pbapply::pblapply(x, util_eval_to_dataquieR_result, env = environment())
#' xx <- pbapply::pblapply(tail(x), util_eval_to_dataquieR_result, env = environment())
#' xx <- parallel
#' cat(vapply(x, deparse1, FUN.VALUE = character(1)), sep = "\n", file = "all_calls.txt")
#' rstudioapi::navigateToFile("all_calls.txt")
#' eval(x$`acc_multivariate_outlier.Blood pressure checks`)
#'}
dq_report2 <- function(study_data, # TODO: make meta_data_segment, ... optional
                       meta_data = "item_level",
                       label_col = LABEL,
                       meta_data_segment = "segment_level",
                       meta_data_dataframe = "dataframe_level",
                       meta_data_cross_item = "cross-item_level",
                       meta_data_v2,
                       ...,
                       dimensions = c("Completeness", "Consistency"),
                       cores = list(mode = "socket",
                                    logging = FALSE,
                                    cpus = util_detect_cores(),
                                    load.balancing = TRUE),
                       specific_args = list(), # TODO: check if list of lists
                       author = prep_get_user_name(),
                       title = "Data quality report",
                       subtitle = "Report information",
                       user_info = NULL,
                       debug_parallel = FALSE,
                       resp_vars = character(0), # FIXME: If we have only one, the report breaks down.
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
                       notes_from_wrapper = list()) {
  mode <- util_match_arg(mode)

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
  } else { # carefully consider https://github.com/r-lib/covr/pull/471 and https://github.com/r-lib/covr/issues/315
    mode <- "parallel"
  }

  if (suppressWarnings(util_ensure_suggested("testthat", err = FALSE))) {
    if (testthat::is_testing()) {
      if (mode != "parallel") {
        util_warning(
          "Internal problem: %s should be %s or %s in the context of %s",
                     sQuote("mode"), dQuote("parallel"), dQuote("default"))
      }
      if (!identical(cores, 1) &&
          !identical(cores, 1L)) {
        util_warning(
          "Internal problem: %s should be %s or %s in the context of %s",
          sQuote("cores"), dQuote("1"), sQuote("NULL"))
      }
    }
  }

  if (!missing(meta_data_v2)) {
    util_message("Have %s set, so I'll remove all loaded data frames",
                 sQuote("meta_data_v2"))
    prep_purge_data_frame_cache()
    prep_load_workbook_like_file(meta_data_v2)
    if (!exists("item_level", .dataframe_environment)) {
      w <- paste("Did not find any sheet named %s in %s, is this",
             "really dataquieR version 2 metadata?")
      if (requireNamespace("cli", quietly = TRUE)) {
        w <- cli::bg_red(cli::col_br_yellow(w))
      }
      util_warning(w, dQuote("item_level"), dQuote(meta_data_v2),
                   immediate = TRUE)
    }
  }
  if (is.data.frame(study_data)) {
    name_of_study_data <- head(as.character(substitute(study_data)), 1)
  } else if (length(study_data) == 1 && is.character(study_data)) {
    name_of_study_data <- study_data
  } else {
    name_of_study_data <- "??No study data found??"
  }
  util_expect_data_frame(study_data)
  prep_add_data_frames(data_frame_list = setNames(list(study_data),
                                                  nm = name_of_study_data))
  try(meta_data <- prep_meta_data_v1_to_item_level_meta_data(meta_data),
                   silent = TRUE)
  warning_pred_meta <- NULL
  if (!is.data.frame(meta_data)) {
    w <- paste("No item level metadata %s found. Will guess some from the study data.",
           "This will not be very helpful, please consider passing an item level",
           "metadata file.")
    if (requireNamespace("cli", quietly = TRUE)) {
      w <- cli::bg_red(cli::col_br_yellow(cli::ansi_toupper(w)))
      w <- gsub("%S", "%s", w)
    }
    util_warning(w,
                 dQuote(meta_data),
                 immediate = TRUE)
    predicted <- prep_study2meta(study_data, convert_factors = TRUE)
    meta_data <- predicted$MetaData
    study_data <- predicted$ModifiedStudyData
    warning_pred_meta <- paste("The item-level metadata could not be found",
                               "and was guessed from the study data.")
  }
  try(util_expect_data_frame(meta_data_segment), silent = TRUE)
  if (!is.data.frame(meta_data_segment)) {
    util_message("No segment level metadata %s found",
                 dQuote(meta_data_segment))
    meta_data_segment <- data.frame(STUDY_SEGMENT =
                                      unique(meta_data$STUDY_SEGMENT))
  }
  try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
  if (!is.data.frame(meta_data_dataframe)) {
    util_message("No dataframe level metadata %s found",
                 dQuote(meta_data_dataframe))
    meta_data_dataframe <- data.frame(DF_NAME =
                                        name_of_study_data)
  }
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message("No cross-item level metadata %s found",
                   dQuote(meta_data_cross_item))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                      CHECK_LABEL = character(0))
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
         util_pretty_vector_string(meta_data[which_not, label_col,
                                               drop = TRUE]),
         util_pretty_vector_string(meta_data[which_not, VARIABLE_ROLE,
                                             drop = TRUE]),
         util_pretty_vector_string(VARIABLE_ROLES),
         sQuote(VARIABLE_ROLE), dQuote(VARIABLE_ROLES$PRIMARY),
             applicability_problem = TRUE)
    meta_data$VARIABLE_ROLE[] <- VARIABLE_ROLES$PRIMARY
  }

  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )
  util_expect_scalar(label_col, check_type = is.character)
  util_ensure_in(label_col, names(meta_data), error = TRUE,
                 err_msg =
        sprintf("Did not find a label column (%s) named %%s in the %s. Did you mean %%s?",
                sQuote("label_col"),
                    sQuote("meta_data")))

  # ensure that labels exist, are unique and not too long
  mod_label <- util_ensure_label(meta_data = meta_data,
                                 label_col = label_col)
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    meta_data <- mod_label$meta_data
  }

  util_expect_scalar(dimensions,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)
  if (length(dimensions) == 0) {
    dimensions <- c("Completeness", "Consistency", "Accuracy")
  }
  dimensions[dimensions == "acc"] <- "Accuracy"
  dimensions[dimensions == "con"] <- "Consistency"
  dimensions[dimensions == "com"] <- "Completeness"
  dimensions[dimensions == "int"] <- "Integrity"
  .dimensions <-
    util_ensure_in(dimensions,
                   c("Completeness", "Consistency", "Accuracy", "Integrity"),
                   error = FALSE,
                   applicability_problem = TRUE
              )

  util_expect_scalar(resp_vars,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  md100 <- meta_data # metadata with study data

  miss_from_study <- (!(md100[[VAR_NAMES]] %in% colnames(study_data)))

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

  util_expect_scalar(filter_indicator_functions,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)
  util_expect_scalar(filter_result_slots,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

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

  old_O <- options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.ERRORS_WITH_CALLER = FALSE,
    dataquieR.MESSAGES_WITH_CALLER = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE)
  on.exit(options(old_O))

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
      mode_args = mode_args
    )
  )

  if (util_ensure_suggested("summarytools",
                            goal =
                            "descriptive summary statistics in the report",
                            err = FALSE)) {
    prep_prepare_dataframes()
    # TODO: Maybe make a labelled, since this is supported by summarytools
    summary_stat <- summarytools::dfSummary(ds1, headings = FALSE)
    summary_descr <- summarytools::descr(ds1, transpose = TRUE,
                                         headings = FALSE)

    summary_descr$Pct.Valid <- scales::percent(summary_descr$Pct.Valid,
                                               scale = 1)

    colnames(summary_descr) <-
      c(Pct.Valid = "Percentage Valid",
        N.Valid = "Number Valid",
        Std.Dev = "Standard Deviation",
        setNames(nm = colnames(summary_descr))
        )[colnames(summary_descr)]

    # remove unneded cols from summarytools tables.
    summary_stat$Variable <- gsub("\\\\$", "",
                                  perl = TRUE,
                                  gsub("(?msi)\\s*\\[[, a-z]+\\]",
                                       "",
                                       perl = TRUE,

                                       summary_stat$Variable)
                                  )
    colnames(summary_stat) <-
      gsub("Variable", "Variables", colnames(summary_stat), fixed = TRUE)

    summary_stat$No <- NULL
    summary_stat$text.graph <- NULL
    summary_stat[] <-
      lapply(summary_stat,
             gsub,
             pattern = "\\",
             replacement = "<br />", fixed = TRUE)
  } else {
    summary_stat <- NULL
    summary_descr <- NULL
  }

  start_from_call <- util_find_first_externally_called_functions_in_stacktrace()
  start_from_call <- length(sys.calls()) - start_from_call # refers to reverted sys.calls, so mirror the number
  if (is.na(start_from_call))
    start_from_call <- 1
  cl <- NULL
  try({
    cl <- sys.call(start_from_call)
  }, silent = TRUE)

  p <- list( # TODO: add this also in Square2
    author = author,
    date = Sys.time(),
    call = cl, # TODO: Why does this not yet work?
    version = paste(packageName(), packageVersion(packageName())),
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

  util_attach_attr(r,
                   properties = p,
                   min_render_version = as.numeric_version("1.0.0"),
                   warning_pred_meta = warning_pred_meta,
                   label_modification_text = trimws(paste(
                     notes_from_wrapper[["label_modification_text"]],
                     mod_label$label_modification_text)),
                   label_modification_table = rbind(
                     notes_from_wrapper[["label_modification_table"]],
                     mod_label$label_modification_table),
                   summary_stat = summary_stat,
                   summary_descr = summary_descr,
                   title = title,
                   subtitle = subtitle)
}

