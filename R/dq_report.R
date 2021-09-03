#' Generate a full DQ report
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#' @param strata_attribute [character] variable of a variable attribute coding
#'                         study segments.
#'                         Values other than leaving this empty or passing
#'                         KEY_STUDY_SEGMENT are not yet supported.
#'                         Stratification is not yet fully supported, please
#'                         use [dq_report_by].
#' @param strata_vars [character] name of variables to stratify the report on,
#'                                such as "study_center". Not yet supported,
#'                                please use [dq_report_by].
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for [parallelMap::parallelStart] or NULL,
#'                        if parallel has already been started by the caller.
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
#'                   for more details.
#'
#' @return a [dataquieR_resultset]. Can be printed creating a RMarkdown-report.
#'
#' @details
#' See [dq_report_by] for a way to generate stratified or splitted reports
#' easily.
#'
#' @seealso `r paste0(" * [", methods(class = "dataquieR_resultset"), "]")`
#' * [dq_report_by]
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{ # really long-running example.
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' report <- dq_report(study_data, meta_data, label_col = LABEL) # most easy use
#' report <- dq_report(study_data, meta_data,
#'   label_col = LABEL, dimensions =
#'     c("Completeness", "Consistency", "Accuracy"),
#'   check_table = read.csv(system.file("extdata",
#'     "contradiction_checks.csv",
#'     package = "dataquieR"
#'   ), header = TRUE, sep = "#"),
#'   show_causes = TRUE,
#'   cause_label_df = read.csv(
#'     system.file("extdata", "Missing-Codes-2020.csv", package = "dataquieR"),
#'     header = TRUE, sep = ";"
#'   )
#' )
#' save(report, file = "report.RData") # careful, this contains the study_data
#' report <- dq_report(study_data, meta_data,
#'   label_col = LABEL,
#'   check_table = read.csv(system.file("extdata",
#'     "contradiction_checks.csv",
#'     package = "dataquieR"
#'   ), header = TRUE, sep = "#"),
#'   specific_args = list(acc_univariate_outlier = list(resp_vars = "K")),
#'     resp_vars = "SBP_0"
#' )
#' report <- dq_report(study_data, meta_data,
#'   label_col = LABEL,
#'   check_table = read.csv(system.file("extdata",
#'     "contradiction_checks.csv",
#'     package = "dataquieR"
#'   ), header = TRUE, sep = "#"),
#'   specific_args = list(acc_univariate_outlier = list(resp_vars = "DBP_0")),
#'     resp_vars = "SBP_0"
#' )
#' report <- dq_report(study_data, meta_data,
#'   label_col = LABEL,
#'   check_table = read.csv(system.file("extdata",
#'     "contradiction_checks.csv",
#'     package = "dataquieR"
#'   ), header = TRUE, sep = "#"),
#'   specific_args = list(acc_univariate_outlier = list(resp_vars = "DBP_0")),
#'   resp_vars = "SBP_0", cores = NULL
#' )
#' }
dq_report <- function(study_data,
                      meta_data,
                      label_col = NULL,
                      ...,
                      dimensions = c("Completeness", "Consistency"),
                      strata_attribute,
                      strata_vars,
                      cores = list(mode = "socket",
                                   logging = FALSE,
                                   cpus = util_detect_cores(),
                                   load.balancing = TRUE),
                      specific_args = list()) {
  util_prepare_dataframes(.replace_missings = FALSE)

  .worker <- function(row) {
    args <- list()
    if (length(row) > 1) {
      args <- row[2:length(row)]
    }
    args$study_data <- study_data
    args$meta_data <- meta_data
    args$label_col <- label_col
    # try(do.call(row$fct_, args))
    do.call(util_make_function(row$fct_), args)
  }

  if (!all(is.character(dimensions))) {
    util_error("'dimensions' needs to be a character vector")
  }
  if (!all(dimensions %in% c("Completeness", "Consistency", "Accuracy"))) {
    util_error('"dimensions" need to be in "Completeness", "Consistency",
               "Accuracy"')
  }
  if (!missing(strata_attribute) && !identical(strata_attribute,
                                               KEY_STUDY_SEGMENT)) {
    util_error(
      "segment attributes other than KEY_STUDY_SEGMENT are unsupported yet")
  } else {
    split_segments <- !missing(strata_attribute)
    if (!split_segments) strata_attribute <- NA_character_
  }
  if (!missing(strata_vars)) {
    util_error("The report generated by dq_report() cannot be stratified yet.
               Please see dq_report_by() for respective functionality.")
  }

  # ---------------------------- applicability matrix -------------------
  app_mat <- pro_applicability_matrix(study_data = study_data, meta_data =
                                        meta_data, label_col = label_col,
                                      split_segments = split_segments)

  segments <- unique(app_mat$SummaryTable$KEY_STUDY_SEGMENT)
  if (KEY_STUDY_SEGMENT %in% colnames(app_mat$SummaryTable)) {
    variables <- split(app_mat$SummaryTable$Variables,
                       app_mat$SummaryTable[[KEY_STUDY_SEGMENT]])
  } else { # nocov start
    # this should never happen, because the applicability matrix
    # must return a column KEY_STUDY_SEGMENT, maybe with only one level.
    variables <- list(all_variables = app_mat$SummaryTable$Variables)
  } # nocov end
  functions_to_trigger <-
    setdiff(colnames(app_mat$SummaryTable), c("Variables", KEY_STUDY_SEGMENT))
  functions_to_trigger <-
    setdiff(functions_to_trigger, "acc_multivariate_outlier")
  if (!("Completeness" %in% dimensions)) {
    functions_to_trigger <-
      functions_to_trigger[!startsWith(functions_to_trigger, "com_")]
  }
  if (!("Consistency" %in% dimensions)) {
    functions_to_trigger <-
      functions_to_trigger[!startsWith(functions_to_trigger, "con_")]
  }
  if (!("Accuracy" %in% dimensions)) {
    functions_to_trigger <-
      functions_to_trigger[!startsWith(functions_to_trigger, "acc_")]
  }
  exstFns <- functions_to_trigger %in% getNamespaceExports(utils::packageName())
  if (!all(exstFns)) { # nocov start
    # this can only happen on incompatible changes in the app-matrix
    # so don't test this explicitly.
    util_warning(
      "Don't know, how to compute the indicator(s) %s. Ignoring this function.",
      paste0(dQuote(functions_to_trigger[!exstFns]), collapse = ", "),
      applicability_problem = FALSE)
    functions_to_trigger <- functions_to_trigger[exstFns]
  } # nocov end

  functions_to_trigger <- data.frame(
    functions_to_trigger = functions_to_trigger,
    dimension = substr(functions_to_trigger, 1, 4),
    stringsAsFactors = FALSE
  )
  if (!all(endsWith(functions_to_trigger$dimension, "_"))) { # nocov start
    # this would also be a convention break in package functions, so don't
    # test this explicitly..
    util_error(c(
      "Internal error. All dimension prefixes",
      "should end with an underscore '_'."))
  } # nocov end
  functions_to_trigger$dimension <- ordered(gsub("_$", "",
                                                 functions_to_trigger$dimension
                                                 ), levels =
                                              c("com", "con", "acc"))

  functions_to_trigger <-
    functions_to_trigger[order(functions_to_trigger$dimension),
                         "functions_to_trigger", TRUE]

  # ---------------------------- compute --------------------------------
  long_format <- lapply(setNames(nm = functions_to_trigger), function(x) list())

  use_cache <- FALSE

  if (length(functions_to_trigger) > 0) {
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
  }

  i <- 0
  for (x in functions_to_trigger[!startsWith(functions_to_trigger, "acc_")]) {
    i <- i + 1
    fct <- try(get(x, envir = getNamespace(utils::packageName()), mode =
                     "function", inherits = FALSE))
    if (!is.function(fct)) { # nocov start
      # should not be possible, because pro_applicability_matrix should
      # not report the applicability non-existing functions
      res <- data.frame()
    } else { # nocov end
      args <- list(...)
      .args <- specific_args[[x]]
      if (is.list(.args) && length(.args) > 0) {
        for (a in names(.args)) {
          args[[a]] <- .args[[a]]
        }
      }
      #                          0. Non-matching datatype + Incomplete metadata,
      #                          1. Non-matching datatype + complete metadata,
      #                          2. Matching datatype + Incomplete metadata,
      #                          3. Matching datatype + complete metadata,
      #                          4. Not applicable according to data type
      if (startsWith(x, "com_")) {
        app_levels_to_use <- 2:3
      } else {
        app_levels_to_use <- 3
      }

      if (!("include_sysmiss" %in% names(args)) && # if include_sysmiss is unset
            "include_sysmiss" %in% names(formals(fct))) { # and we call itemmiss
        if (is.null(formals(fct)$include_sysmiss)) { # if default still NULL
          args$include_sysmiss <- TRUE # switch default here TRUE for dq_report
        }
      }

      if (!("resp_vars" %in% names(args))) {
        args[["resp_vars"]] <-
          as.character(app_mat$SummaryTable$Variables[app_mat$SummaryTable[[x]]
                                                      %in% app_levels_to_use])
      } else {
        if (is.null(args[["resp_vars"]]) || all(is.na(args[["resp_vars"]]))) {
          if ("label_col" %in% names(args) && args$label_col %in%
              colnames(meta_data)) {
            args[["resp_vars"]] <- meta_data[[args$label_col]]
          } else {
            args[["resp_vars"]] <- meta_data[["VAR_NAMES"]]
          }
        }
        args[["resp_vars"]] <- setdiff(
          args[["resp_vars"]],
          as.character(app_mat$SummaryTable$Variables)[
            !(app_mat$SummaryTable[[x]] %in% app_levels_to_use)]
        )
        args[["resp_vars"]] <- args[["resp_vars"]][!is.na(args[["resp_vars"]])]
      }
      message(sprintf("%s [%s] %d of %d, %s -- %d variables", Sys.time(),
                      "INFO", i, length(functions_to_trigger), x,
                      length(args[["resp_vars"]])))
      if (startsWith(x, "acc_")) {
        util_error("Internal error: found acc in none-acc-part")
      } else {
        if (missing(strata_vars)) {
          args <- c(args, list(study_data = study_data, meta_data = meta_data,
                       label_col = label_col))
        } else {
          args <- c(args, list(study_data = study_data, meta_data = meta_data,
                       label_col = label_col, strata_vars = strata_vars))
        }
        args <- args[names(args) %in% names(formals(fct))]
        r <- try(do.call(x, args))
        df_args <- setdiff(names(args),
                           c("study_data", "meta_data", "label_col"))
        res <- as.data.frame(matrix(NA, nrow = 1, ncol = (length(df_args) + 1),
                                    dimnames = list(NULL,
                                                    c(df_args, "results"))))
        if (length(df_args) > 0) {
          for (a in df_args) {
            res[[a]] <- args[a]
          }
        }
        res[["results"]] <- list(r)
        if ("ModifiedStudyData" %in% names(res[["results"]][[1]]) &&
            is.data.frame(res[["results"]][[1]]$ModifiedStudyData)) {
          study_data <- res[["results"]][[1]]$ModifiedStudyData
          if ("label_col" %in% names(args)) {
            cn <- util_map_labels(
              x = colnames(study_data),
              meta_data = meta_data,
              to = VAR_NAMES,
              from = args$label_col,
              ifnotfound = NA_character_
            )
            if (any(is.na(cn))) {
              util_error(
                "Cannot re-map Var-Names from %s to %s: %s",
                dQuote(args$label_col),
                dQuote(VAR_NAMES),
                paste0(dQuote(colnames(study_data)[is.na(cn)]), collapse = ", ")
              )
            }
            colnames(study_data) <- cn
            attr(study_data, "MAPPED") <- FALSE
          }
        }
      }
      message(sprintf("%s [%s] %d of %d, %s -- done", Sys.time(), "INFO", i,
                      length(functions_to_trigger), x))
    }
    long_format[[x]] <- res
  }

  acc_plan <- list()
  acc_df <- list()
  for (x in functions_to_trigger[startsWith(functions_to_trigger, "acc_")]) {
    i <- i + 1
    fct <- try(get(x, envir = getNamespace(utils::packageName()),
                   mode = "function", inherits = FALSE))
    if (!is.function(fct)) {
      # res <- data.frame()
    } else {
      args <- list(...)
      .args <- specific_args[[x]]
      if (is.list(.args) && length(.args) > 0) {
        for (a in names(.args)) {
          args[[a]] <- .args[[a]]
        }
      }
      #                          0. Non-matching datatype + Incomplete metadata,
      #                          1. Non-matching datatype + complete metadata,
      #                          2. Matching datatype + Incomplete metadata,
      #                          3. Matching datatype + complete metadata,
      #                          4. Not applicable according to data type
      if (!("resp_vars" %in% names(args))) {
        args[["resp_vars"]] <-
          as.character(app_mat$SummaryTable$Variables[app_mat$SummaryTable[[x]]
                                                      %in% 2:3]) # appl. matrix
                                            # for accuracy is a bit too careful
      } else {
        if (is.null(args[["resp_vars"]]) || all(is.na(args[["resp_vars"]]))) {
          if ("label_col" %in% names(args) && args$label_col %in%
              colnames(meta_data)) {
            args[["resp_vars"]] <- meta_data[[args$label_col]]
          } else {
            args[["resp_vars"]] <- meta_data[["VAR_NAMES"]]
          }
        }
        args[["resp_vars"]] <- setdiff(
          args[["resp_vars"]],
          as.character(app_mat$SummaryTable$Variables)[
            !(app_mat$SummaryTable[[x]] %in% 2:3)] # appl. matrix
                                            # for accuracy is a bit too careful
        )
        args[["resp_vars"]] <- args[["resp_vars"]][!is.na(args[["resp_vars"]])]
      }
      message(sprintf(
        "%s [%s] %d of %d, %s -- %d variables: planning parallel computation",
        Sys.time(), "INFO", i, length(functions_to_trigger), x,
        length(args[["resp_vars"]])))
      if (startsWith(x, "acc_")) {
        if (!identical(args$resp_vars, character(0))) {
          # app mat likely found no suitable variable for calling x
          args <- args[names(args) %in% names(formals(fct))]
          args <- c(
            list(fct = x, compute_plan_only = TRUE, study_data = study_data,
                 meta_data = meta_data, label_col = label_col),
            args,
            list(result_groups = NULL, cores = NULL, use_cache = TRUE)
          )
          facc_df <- do.call(pipeline_vectorized, args)
          acc_df[[x]] <- facc_df
          if (length(facc_df) > 0) {
            facc_df <- cbind.data.frame(data.frame(fct_ = x, stringsAsFactors =
                                                     FALSE), facc_df)
            facc_plan <- prep_pmap(facc_df, list, cores = NULL)
            acc_plan <- c(acc_plan, facc_plan) # within pmap append an x each
          }
        }
      } else {
        util_error("Internal error: found none-acc in acc-part")
      }
      message(sprintf(
        "%s [%s] %d of %d, %s: planning parallel computation -- done",
        Sys.time(), "INFO", i, length(functions_to_trigger), x))
    }
  }
  if ("label_col" %in% names(args)) {
    label_col <- args[["label_col"]]
  } else {
    label_col <- VAR_NAMES
  }
  if (length(acc_plan) > 0) {
    oldO <- options(parallelMap.show.info = FALSE)
    on.exit(options(oldO), add = TRUE)
    parallelMap::parallelExport("study_data", "meta_data", "label_col")
    # this exports the static data to the cluster, this is always done, even if
    # already avail. functions don't touch the exported data.
    currentCpus <- parallelMap::parallelGetOptions()$settings$cpus

    if (parallelMap::parallelGetOptions()$settings$mode == "local") {
      environment(.worker) <- new.env(parent = asNamespace("dataquieR"))
    } else if (parallelMap::parallelGetOptions()$settings$mode == "multicore") {
      environment(.worker) <- environment()
    } else {
      environment(.worker) <- asNamespace("dataquieR")
    }

    n_nodes <- max(1, as.integer(currentCpus[[1]]), na.rm = TRUE)
    # was length(parallel::getDefaultCluster()), but parallelMap doesn't use
    # defaultcluster any more!!
    tasks_per_node <- ceiling(length(acc_plan) / n_nodes)
    indices <- 1:length(acc_plan)
    length(indices) <- n_nodes * tasks_per_node # make equal length
    task_matrix <- matrix(indices, ncol = n_nodes, nrow = tasks_per_node,
                          byrow = TRUE)
    r <- unlist(lapply(
      1:nrow(task_matrix),
      function(row) {
        slices <- task_matrix[row, ]
        slices <- slices[!is.na(slices)]
        message(
          sprintf("%s [%s] %d of %d, %s", Sys.time(), "INFO",
                  row, nrow(task_matrix), "Accuracy of single variables")
        )
        R.devices::suppressGraphics(
          # don't use any auto graphics device (needed for certain
          # parallelization methods)
          parallelMap::parallelLapply(impute.error = identity,
                                      acc_plan[slices], .worker)
        )
      }
    ), recursive = FALSE)

    message(
      sprintf("%s [%s], %s", Sys.time(), "INFO",
              "Accuracy of single variables -- done")
    )

    # call_plan[['results']] <- r
    dfs <- lapply(acc_plan, as.data.frame, stringsAsFactors = FALSE)
    long_formats <- mapply(FUN = function(dfr, res) {
      dfr[["results"]] <- list(res)
      dfr
    }, dfs, r, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    functions <- unlist(lapply(dfs, `[[`, "fct_"), recursive = FALSE)
    new_long_format <- split(long_formats, functions)
    new_long_format_df <- lapply(new_long_format, do.call, what =
                                   rbind.data.frame)
    new_long_format_clean <- lapply(new_long_format_df, function(dfr) {
      dfr$fct_ <- NULL
      dfr
    })
    long_format[names(new_long_format_clean)] <-
      new_long_format_clean[names(new_long_format_clean)]
  }
  # ---------------------------- output ---------------------------------
  if (missing(strata_vars)) {
    r <- dataquieR_resultset(long_format = long_format, app_mat = app_mat,
                             study_data = study_data, meta_data = meta_data,
                             strata_attribute = strata_attribute,
                             label_col = label_col)
  } else {
    r <- dataquieR_resultset(long_format = long_format, app_mat = app_mat,
                             study_data = study_data, meta_data = meta_data,
                             strata_attribute = strata_attribute,
                             strata_vars = strata_vars, label_col = label_col)
  }
  return(r)
}
