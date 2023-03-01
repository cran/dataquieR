#' Call (nearly) one "Accuracy" function with many parameterizations at once
#' automatically
#'
#' @description
#' This is a function to automatically call indicator functions of the
#' "Accuracy" dimension in a vectorized manner with a set of parameterizations
#' derived from the metadata.
#'
#' @details
#'
#' The function to call is given as first argument. All arguments of the called
#' functions can be given here, but `pipline_vectorized` can derive technically
#' possible values (most of them) from the metadata, which can be controlled
#' using the arguments `key_var_names` and `variable_roles`. The function
#' returns an encapsulated list by default, but it can also return a
#' [data.frame]. See also [pipeline_recursive_result] for these two options.
#' The argument `use_cache` controls, whether the input data (`study_data` and
#' `meta_data`) should be passed around, if running in parallel or being
#' distributed beforehand to the compute nodes. All calls will be done in
#' parallel, if possible. This can be configured, see argument `cores` below.
#'
#' If the function is called in a larger framework (such as [dq_report]), then
#' `compute_plan_only` controls, not to actually call functions but return
#' a [data.frame] with parameterizations of "Accuracy" functions only. Also in
#' such a scenario, one may want not to start and stop a cluster with entry
#' and leaving of [pipeline_vectorized] but use an existing cluster. This can
#' be achieved by setting the `cores` argument `NULL`.
#'
#' @param fct [function] function to call
#' @param resp_vars [variable list] the name of the measurement variables,
#'                                  if NULL (default), all variables are used.
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param ... additional arguments for the function
#' @param key_var_names [character] character vector named by arguments to be
#'                      filled by meta data GROUP_VAR-entries as follows:
#'                      c(group_vars = GROUP_VAR_OBSERVER) -- may be missing,
#'                      then all possible combinations will be analyzed.
#'                      Cannot contain resp_vars.
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for [parallelMap::parallelStart] or NULL, if
#'                        parallel has already been started by the caller.
#' @param variable_roles [list] restrict each function argument (referred to by
#'                       its name matching a name in `names(variable_roles)`)
#'                       to variables of the role given here.
#' @param result_groups [character] columns to group results to encapsulated
#'                                  lists or NULL receive a data frame with all
#'                                  call arguments and their respective results
#'                                  in a column 'result' -- see
#'                      [pipeline_recursive_result]
#' @param use_cache [logical] set to FALSE to omit re-using already distributed
#'                            study- and metadata on a parallel cluster
#' @param compute_plan_only [logical] set to TRUE to omit computations and
#'                                    return only the compute plan
#'                                    filled with planned evaluations. used in
#'                                    pipelines.
#'
#' @return
#'  - if `result_groups` is set, a list with:
#'    - first argument's values in `result_groups`, each containing second's
#'      argument's values as a similar list recursively;
#'  - if `result_groups` is not set, a data frame with one row per function
#'    call, all the arguments of each call in its columns and a column `results`
#'    providing the function calls' results.
#'
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{ # really long-running example
#' load(system.file("extdata/study_data.RData", package = "dataquieR"))
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"))
#' a <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER)
#' )
#' b <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL
#' )
#' b_adj <-
#'   pipeline_vectorized(
#'     fct = acc_margins, study_data = study_data,
#'     meta_data = meta_data, label_col = LABEL, co_vars = c("SEX_1", "AGE_1")
#'   )
#' c <- pipeline_vectorized(
#'   fct = acc_loess, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   variable_roles = list(
#'     resp_vars = list(VARIABLE_ROLES$PRIMARY),
#'     group_vars = VARIABLE_ROLES$PROCESS
#'   )
#' )
#' d <- pipeline_vectorized(
#'   fct = acc_loess, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   variable_roles = list(
#'     resp_vars = list(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$SECONDARY),
#'     group_vars = VARIABLE_ROLES$PROCESS
#'   )
#' )
#' e <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER), co_vars = "SEX_0"
#' )
#'
#' f <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER), co_vars = "SEX_0",
#'   result_groups = NULL
#' )
#' pipeline_recursive_result(f)
#' g <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER), co_vars = "SEX_0",
#'   result_groups = c("co_vars")
#' )
#' g1 <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER), co_vars = "SEX_0",
#'   result_groups = c("group_vars")
#' )
#' g2 <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER), co_vars = "SEX_0",
#'   result_groups = c("group_vars", "co_vars")
#' )
#' g3 <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   key_var_names = c(group_vars = GROUP_VAR_OBSERVER), co_vars = "SEX_0",
#'   result_groups = c("co_vars", "group_vars")
#' )
#' g4 <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_data, label_col = LABEL,
#'   co_vars = "SEX_0", result_groups = c("co_vars")
#' )
#' meta_datax <- meta_data
#' meta_datax[9, "GROUP_VAR_DEVICE"] <- "v00011"
#' g5 <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_datax, label_col = LABEL,
#'   co_vars = "SEX_0", result_groups = c("co_vars")
#' )
#' g6 <- pipeline_vectorized(
#'   fct = acc_margins, study_data = study_data,
#'   meta_data = meta_datax, label_col = LABEL,
#'   co_vars = "SEX_0", result_groups = c("co_vars", "group_vars")
#' )
#' }
pipeline_vectorized <- function(fct, resp_vars = NULL, study_data, meta_data,
                                label_col, ..., key_var_names,
                                cores = list(mode = "socket",
                                             logging = FALSE,
                                             load.balancing = TRUE),
                                variable_roles = list(
                                  resp_vars = list(VARIABLE_ROLES$PRIMARY,
                                                   VARIABLE_ROLES$SECONDARY),
                                  group_vars = VARIABLE_ROLES$PROCESS
                                ),
                                result_groups, use_cache = FALSE,
                                compute_plan_only = FALSE) {
  # --------------------- Preps and Checks ----------------
  # if not in global environment go to parent environment
  try(caller. <- sys.call(0), silent = TRUE)
  if (is.character(fct)) {
    .fct_name <- fct
  } else {
    .fct_name <- deparse(substitute(fct))
  }
  .fct <- match.fun(fct)

  if (!missing(key_var_names) && "resp_vars" %in% names(key_var_names)) {
    util_error(c(
      "Found resp_vars assigned in key_var_names. Cannot fetch the resp_vars",
      "from the metadata for all resp_vars.\n"))
  }
  args_to_fill <- names(formals(.fct))
  explicit_vals <- list(...)
  explicit_args <- names(explicit_vals)
  if (missing(key_var_names)) {
    indirect_args <- c()
  } else {
    indirect_args <- names(key_var_names)
  }

  util_prepare_dataframes(.replace_missings = FALSE)
  if (nrow(ds1) == 0) {
    util_error("No observations found in study data.")
  }
  if (ncol(ds1) == 0) {
    util_error("No variables found in study data.")
  }

  if (is.null(resp_vars)) { # Autofill resp_vars
    resp_vars <- colnames(ds1)
  }

  # --------------------- Fill Call Plan ----------------
  call_plan <- as.data.frame(matrix(nrow = length(resp_vars), ncol =
                                      length(args_to_fill), dimnames =
                                      list(resp_vars, args_to_fill)),
                             stringsAsFactors = FALSE)

  filled <- setNames(rep(FALSE, length(args_to_fill)), args_to_fill)

  call_plan$resp_vars <- resp_vars
  filled[["resp_vars"]] <- TRUE

  # fill in all fixed args
  for (a in explicit_args) {
    call_plan[[a]] <- rep(list(explicit_vals[[a]]), nrow(call_plan))
    filled[[a]] <- TRUE
  }

  # fill in all metadata derived args
  for (a in indirect_args) {
    call_plan[[a]] <-
      unlist(mget(as.character(resp_vars), as.environment(setNames(as.list(
        meta_data[[key_var_names[[a]]]]), nm = meta_data[[label_col]])),
        ifnotfound = NA_character_))
    call_plan[[a]] <-
      unlist(unname(mget(as.character(unlist(call_plan[[a]])), as.environment(
        setNames(as.list(meta_data[[label_col]]), nm =
                   meta_data[["VAR_NAMES"]])), ifnotfound = NA_character_)))
    filled[[a]] <- TRUE
  }

  yet_to_fill <- names(which(!filled, useNames = TRUE))

  # build plan for filling all open *_var args with (*NOT* expand.grid(args),
  # see https://stackoverflow.com/a/30085595), but merge,
  # ingenious idea, btw:
  vars_yet_to_fill <- grep("_vars$", yet_to_fill, value = TRUE)

  # fill in all GROUP_VAR_-vars that refer to a suitable process variable
  ## Autofill group_vars and Autofill strata_vars
  for (a in c("strata_vars", "group_vars")) {
    if (a %in% vars_yet_to_fill) {
      all_key_vars <- util_variable_references(meta_data)
      all_key_vars <- setdiff(all_key_vars, PART_VAR)
      util_warning(
        c("Found %s in arguments of %s. Will call %s for",
          "all available GROUP_VAR (%s)\n"),
        dQuote(a), dQuote(.fct_name), dQuote(.fct_name),
        paste0(dQuote(all_key_vars), collapse = ", "),
        applicability_problem = TRUE
      )
      if ("time_vars" %in% args_to_fill) {
        # do no use TIME_VAR if the function cares about time.
        all_key_vars <- setdiff(all_key_vars, c(TIME_VAR))
      }
      if (.fct_name %in% c("acc_margins", "acc_varcomp")) {
        # no margins plot / ICC for date/time nor for study segment
        all_key_vars <- setdiff(all_key_vars, c(TIME_VAR))
      }
      names(all_key_vars) <- all_key_vars

      if (length(all_key_vars) > 0) {
        call_plan <-
          merge(
            call_plan[, setdiff(colnames(call_plan), a)],
            data.frame(setNames(list(all_key_vars), a),
                       stringsAsFactors = FALSE, row.names = NULL)
          ) # *NOT* expand.grid(args), see https://stackoverflow.com/a/30085595

        call_plan[[a]] <-
          apply(call_plan[, c("resp_vars", a), FALSE], 1, function(x) {
            var <- x[[1]]
            kvn <- x[[2]]
            r <- unlist(mget(as.character(var),
                             as.environment(setNames(as.list(meta_data[[kvn]]),
                                                     nm =
                                                       meta_data[[label_col]])),
                             ifnotfound = NA_character_))
            unlist(mget(as.character(r),
                        as.environment(setNames(as.list(meta_data[[label_col]]),
                                                nm = meta_data[["VAR_NAMES"]])),
                        ifnotfound = NA_character_))
          })
        filled[[a]] <- TRUE
      }
    }
  }

  ## Autofill time_vars
  a <- "time_vars"
  if (a %in% vars_yet_to_fill) {
    all_key_vars <- grep("(TIME_VAR)", colnames(meta_data),
                         value = TRUE)
    util_warning(
      c("Found %s in arguments of %s. Will call %s for all",
        "available TIME_VAR (%s)\n"),
      dQuote(a), dQuote(.fct_name), dQuote(.fct_name),
      paste0(dQuote(all_key_vars), collapse = ", "),
      applicability_problem = TRUE
    )
    names(all_key_vars) <- all_key_vars

    if (length(all_key_vars) > 0) {
      call_plan <-
        merge(
          call_plan[, setdiff(colnames(call_plan), a)],
          data.frame(setNames(list(all_key_vars), a), stringsAsFactors = FALSE,
                     row.names = NULL)
        ) # *NOT* expand.grid(args), see https://stackoverflow.com/a/30085595

      call_plan[[a]] <-
        apply(call_plan[, c("resp_vars", a), FALSE], 1, function(x) {
          var <- x[[1]]
          kvn <- x[[2]]
          r <- unlist(mget(as.character(var), as.environment(setNames(
            as.list(meta_data[[kvn]]), nm = meta_data[[label_col]])),
            ifnotfound = NA_character_))
          unlist(mget(as.character(r), as.environment(setNames(as.list(
            meta_data[[label_col]]), nm = meta_data[["VAR_NAMES"]])),
            ifnotfound = NA_character_))
        })

      filled[[a]] <- TRUE
    }
  }

  if (any(c("co_vars", "id_vars") %in%
          names(which(!filled, useNames = TRUE)))) {
    util_warning(
      "For co_vars and id_vars, auto-fill has not yet been implemented.",
      applicability_problem = TRUE)
  }

  # reduce call_plan by columns never specified hoping for the target function
  # to have reasonable defaults in place
  filled_cols <- names(which(filled, useNames = TRUE))
  call_plan <- call_plan[, filled_cols, FALSE]

  # --------------------- Call Plan Filled, Remove void calls ----------------

  use <- rep(TRUE, nrow(call_plan)) # call this row of call plan?

  for (argument in names(variable_roles)) { # for each argument with attached
                                            # variable roles restrictions
    if (argument %in% colnames(call_plan)) {
      referred_vars <- call_plan[[argument]] # fetch its used variable name
      if (VARIABLE_ROLE %in% colnames(meta_data)) {
        roles <- # fetch the roles of these variables
          unname(vapply(referred_vars,
            util_map_labels,
            meta_data = meta_data,
            to = VARIABLE_ROLE,
            from = label_col,
            ifnotfound = list("NA" = NA_character_), FUN.VALUE = character(1)
          ))
        use <- use & (roles %in% c(NA_character_,
                                   variable_roles[[argument]])) # will remove
                                           # rows with void roles from call plan
      }
    }
  }

  call_plan <- call_plan[use, , FALSE] # actually reduce call plan
  call_plan <-
    call_plan[!duplicated(call_plan), , FALSE] # actually reduce call plan

  fct_ <- function(...) {
    # call a dataquieR function producing a dataquieR_result
    # (having warnings, messages and errors attached)
    rv <- dQuote(list(...)[["resp_vars"]])
    env <- new.env(environment())
    env$e <- list()
    env$w <- list()
    env$m <- list()
    r <-
      tryCatch(withCallingHandlers(
        .fct(...),
        warning = function(.w) {
          .w$message <- sprintf("In %s (%s): %s", .fct_name, rv, .w$message)
          .w$call <- caller.
          env$w[[length(env$w) + 1]] <- .w
          invokeRestart("muffleWarning")
        },
        message = function(.m) {
          .m$message <- sprintf("In %s (%s): %s", .fct_name, rv, .m$message)
          .m$call <- caller.
          env$m[[length(env$m) + 1]] <- .m
          invokeRestart("muffleMessage")
        }
      ), error = function(.e) {
        .e$message <- sub("^In .fct:", sprintf("In %s (%s):", .fct_name, rv),
                          .e$message)
        .e$call <- caller.
        env$e[[length(env$e) + 1]] <- .e
        list()
      })
    attr(r, "error") <- env$e
    attr(r, "message") <- env$m
    attr(r, "warning") <- env$w
    class(r) <- c(class(r), "dataquieR_result")
    r
  }
  environment(fct_) <- new.env(parent = asNamespace("dataquieR"))
  assign(".fct", .fct, environment(fct_))
  assign(".fct_name", .fct_name, environment(fct_))
  assign("caller.", .fct_name, environment(fct_))

  if (nrow(call_plan) > 0) {
    # if we still have something to call
    if (nrow(call_plan) > 1) {
      # if we still have more than 1 calculation, do it parallel
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



    if (!compute_plan_only) {
      r <- R.devices::suppressGraphics(
        # don't use any auto graphics device
        # (needed for certain parallelization methods)
        util_par_pmap(call_plan, fct_,
          study_data = study_data, meta_data = meta_data, label_col = label_col,
          cores = cores, use_cache = use_cache
        )
      )
      call_plan[["results"]] <- r
    }

    rownames(call_plan) <- NULL
    if (!missing(result_groups) && is.null(result_groups)) {
      return(call_plan)
    } else {
      if (missing(result_groups)) {
        result_groups <- colnames(call_plan)[2:(ncol(call_plan) - 1)]
      }
      pipeline_recursive_result(call_plan, result_groups)
    }
  } else {
    list()
  }
}
