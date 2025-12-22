#' Prepare and verify study data with metadata
#'
#' @description
#' This function ensures, that a data frame `ds1` with suitable variable
#' names study_data and meta_data exist as base [data.frame]s.
#'
#' @details
#'
#' This function defines `ds1` and modifies `study_data` and `meta_data` in the
#' environment of its caller (see [eval.parent]). It also defines or modifies
#' the object `label_col` in the calling environment. Almost all functions
#' exported by `dataquieR` call this function initially, so that aspects common
#' to all functions live here, e.g. testing, if an argument `meta_data` has been
#' given and features really a [data.frame]. It verifies the existence of
#' required metadata attributes ([VARATT_REQUIRE_LEVELS]). It can also replace
#' missing codes by `NA`s, and calls [prep_study2meta] to generate a minimum
#' set of metadata from the study data on the fly (should be amended, so
#' on-the-fly-calling is not recommended for an instructive use of `dataquieR`).
#'
#' The function also detects `tibbles`, which are then converted to base-R
#' [data.frame]s, which are expected by `dataquieR`.
#'
#' If `.internal` is `TRUE`, differently from the other utility function that
#' work in their caller's environment, this function modifies objects in the
#' calling function's environment. It defines a new object `ds1`,
#' it modifies `study_data` and/or `meta_data`
#' and `label_col`.
#'
#' @param .study_data if provided, use this data set as study_data
#' @param .meta_data if provided, use this data set as meta_data
#' @param .label_col if provided, use this as label_col
#' @param .replace_hard_limits replace `HARD_LIMIT` violations by `NA`,
#'                             defaults to `FALSE`.
#' @param .replace_missings replace missing codes, defaults to `TRUE`
#' @param .sm_code missing code for `NAs`, if they have been
#'                 re-coded by `util_combine_missing_lists`
#' @param .allow_empty allow `ds1` to be empty, i.e., 0 rows and/or 0 columns
#' @param .adjust_data_type ensure that the data type of variables in the study
#'                data corresponds to their data type specified in the metadata
#' @param .amend_scale_level ensure that `SCALE_LEVEL` is available in the
#'                           item-level `meta_data`. internally used to prevent
#'                           recursion, if called from
#'                           [prep_scalelevel_from_data_and_metadata()].
#' @param .internal [logical] internally called, modify caller's environment.
#' @param .apply_factor_metadata  [logical] convert categorical variables to
#'                                          labeled factors.
#' @param .apply_factor_metadata_inadm  [logical] convert categorical variables
#'                                          to labeled factors keeping
#'                                          inadmissible values. Implies, that
#'                                          .apply_factor_metadata will be set
#'                                          to `TRUE`, too.
#'
#' @seealso acc_margins
#'
#' @return `ds1` the study data with mapped column names, `invisible()`, if
#'         not `.internal`
#'
#' @examples
#' \dontrun{
#' acc_test1 <- function(resp_variable, aux_variable,
#'                       time_variable, co_variables,
#'                       group_vars, study_data, meta_data) {
#'   prep_prepare_dataframes()
#'   invisible(ds1)
#' }
#' acc_test2 <- function(resp_variable, aux_variable,
#'                       time_variable, co_variables,
#'                       group_vars, study_data, meta_data, label_col) {
#'   ds1 <- prep_prepare_dataframes(study_data, meta_data)
#'   invisible(ds1)
#' }
#' environment(acc_test1) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#'
#' environment(acc_test2) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#' acc_test3 <- function(resp_variable, aux_variable, time_variable,
#'                       co_variables, group_vars, study_data, meta_data,
#'                       label_col) {
#'   prep_prepare_dataframes()
#'   invisible(ds1)
#' }
#' acc_test4 <- function(resp_variable, aux_variable, time_variable,
#'                       co_variables, group_vars, study_data, meta_data,
#'                       label_col) {
#'   ds1 <- prep_prepare_dataframes(study_data, meta_data)
#'   invisible(ds1)
#' }
#' environment(acc_test3) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#'
#' environment(acc_test4) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#' meta_data <- prep_get_data_frame("meta_data")
#' study_data <- prep_get_data_frame("study_data")
#' try(acc_test1())
#' try(acc_test2())
#' acc_test1(study_data = study_data)
#' try(acc_test1(meta_data = meta_data))
#' try(acc_test2(study_data = 12, meta_data = meta_data))
#' print(head(acc_test1(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test2(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test3(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test3(study_data = study_data, meta_data = meta_data,
#'   label_col = LABEL)))
#' print(head(acc_test4(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test4(study_data = study_data, meta_data = meta_data,
#'   label_col = LABEL)))
#' try(acc_test2(study_data = NULL, meta_data = meta_data))
#' }
#'
#' @export
#'
#' @importFrom rlang caller_fn
#' @importFrom utils object.size
prep_prepare_dataframes <- function(.study_data, .meta_data, .label_col,
                                    .replace_hard_limits,
                                    .replace_missings, .sm_code = NULL,
                                    .allow_empty = FALSE,
                                    .adjust_data_type = TRUE,
                                    .amend_scale_level = TRUE,
                                    .apply_factor_metadata = FALSE,
                                    .apply_factor_metadata_inadm = FALSE,
                                    .internal =
                                      rlang::env_inherits(
                                        rlang::caller_env(),
                                        parent.env(environment()))) {

  case_insens <- util_is_na_0_empty_or_false(
    getOption("dataquieR.study_data_colnames_case_sensitive",
              dataquieR.study_data_colnames_case_sensitive_default))

#  dimension <- substr(rlang::call_name(rlang::caller_call()), 1, 3)
  util_expect_scalar(.sm_code,
                     check_type = util_all_is_integer,
                     allow_null = TRUE)

  util_expect_scalar(.apply_factor_metadata, check_type = is.logical)
  util_expect_scalar(.apply_factor_metadata_inadm, check_type = is.logical)
  if (.apply_factor_metadata_inadm)
    .apply_factor_metadata <- TRUE

  if (missing(.replace_hard_limits)) .replace_hard_limits <- FALSE
  util_expect_scalar(.replace_hard_limits, check_type = is.logical)

  util_expect_scalar(.allow_empty, check_type = is.logical)
  if (!missing(.replace_missings) && (length(.replace_missings) != 1 ||
                                      !is.logical(.replace_missings) ||
                                      is.na(.replace_missings))) {
    util_error(
      c("Internal error, sorry, please report: .replace_missings needs to",
        "be 1 logical value."))
  }
  if (missing(.replace_missings)) .replace_missings <- TRUE


  callfn <- caller_fn(1)

  caller_defaults <- suppressWarnings(formals(callfn))
  caller_formals <- names(caller_defaults)
  caller_has_default <- suppressWarnings({
    !vapply(formals(callfn), identical, rlang::missing_arg(),
            FUN.VALUE = logical(1))
  })

  missing_in_parent <- suppressWarnings({
    vapply(names(formals(callfn)), function(x) eval(call("missing",
                                                         as.symbol(x)),
                                                    envir = parent.frame(3)),
           FUN.VALUE = logical(1))
  })

  if (missing(.label_col)) {
    if ("label_col" %in% caller_formals) {
      if (!eval.parent(substitute({
        missing(label_col)
      }))) {
        quoted_label_col <- eval.parent(substitute(substitute(label_col)))
        .label_col <- try(eval(quoted_label_col, envir = parent.frame()),
                          silent = TRUE)
        if (inherits(.label_col, "try-error")) {
          .label_col <- try(eval(quoted_label_col, envir =
                                   dataquieR::WELL_KNOWN_META_VARIABLE_NAMES),
                            silent = TRUE)
        }
        if (inherits(.label_col, "try-error")) {
          util_error("Cannot resolve %s", dQuote(paste0("label_col", " = ",
                                                        quoted_label_col)),
                     applicability_problem = TRUE)
        }
      } else if (caller_has_default[["label_col"]]) {
        .label_col <- eval(caller_defaults[["label_col"]],
                           envir = asNamespace("dataquieR"), enclos =
                             parent.frame(2))
      } else {
        .label_col <- VAR_NAMES
      }
    } else if (exists("label_col", parent.frame())) {
      .label_col <- get("label_col", parent.frame())
    } else {
      .label_col <- VAR_NAMES
    }
  }

  # if no study_data have been provided -> error
  if (missing(.study_data)) {
    if ("study_data" %in% caller_formals) {
      if (!eval.parent(substitute({
        missing(study_data)
      }))) {
        if (exists("study_data", parent.frame())) { # TODO: Test this and the same for metadata
          .study_data <- try(eval(quote(study_data), parent.frame()),
                             silent = TRUE)
          if (inherits(.study_data, "try-error")) {
            cnd <- attr(.study_data, "condition")
            cnd$call <- sys.call(1)
            util_error(cnd)
          }
        } else {
          util_error("object %s not found", dQuote("study_data"))
        }
      } else if (caller_has_default[["study_data"]]) {
        .study_data <- try(eval.parent(caller_defaults[["study_data"]], n = 2),
                           silent = TRUE)
        if (inherits(.study_data, "try-error")) {
          cnd <- attr(.study_data, "condition")
          cnd$call <- sys.call(1)
          util_error(cnd)
        }
      } else {
        .study_data <- NULL
      }
    } else if (exists("study_data", parent.frame())) {
      .study_data <- get("study_data", parent.frame())
    } else {
      .study_data <- NULL
    }
  }

  e <- new.env(parent = environment())
  e$study_data <- .study_data
  .study_data <-
    eval(
      quote(try(util_expect_data_frame(study_data, keep_types = TRUE),
                silent = TRUE)),
      e
    )
  if (inherits(.study_data, "try-error")) {
    util_error("Need study data as a data frame: %s",
               conditionMessage(attr(.study_data, "condition")))
  }
  .study_data <- util_normalize_time_only_columns(.study_data)

  if (.internal &&
      !exists("already_ppdf", parent.frame()) &&
      all(c("item_level", "meta_data") %in% caller_formals) &&
      !isTRUE(missing_in_parent["item_level"]) &&
      !isTRUE(missing_in_parent["meta_data"]) &&
      !identical(dynGet("item_level"), dynGet("meta_data"))
      ) {
    # IDEA: if (rlang::hash())
    util_error(c("You cannot provide both, %s as well as %s",
                 "these arguments are synonyms and must be",
                 "used mutually exclusively"),
               sQuote("item_level"),
               sQuote("meta_data"))
    # see prep_get_labels
  }

  if (missing(.meta_data)) {
    if ("meta_data" %in% caller_formals &&
       (!isTRUE(missing_in_parent["meta_data"]) ||
                  caller_has_default[["meta_data"]])) {
      if (isTRUE(missing_in_parent["meta_data"])) {
        .meta_data <- try(eval(caller_defaults[["meta_data"]], parent.frame(2)),
                          silent = TRUE)
        if (util_is_try_error(.meta_data)) {
          .meta_data <- try(eval(caller_defaults[["meta_data"]], parent.frame(1)),
                            silent = TRUE)
        }
      } else {
        .meta_data <- try(eval(quote(meta_data), parent.frame(1)),
                          silent = TRUE)
      }
      if (inherits(.meta_data, "try-error")) {
        cnd <- attr(.meta_data, "condition")
        cnd$call <- sys.call(1)
        util_error(cnd)
      }
    }
  }


  if (missing(.meta_data)) {
    if ("item_level" %in% caller_formals &&
        (!isTRUE(missing_in_parent["item_level"]) ||
         caller_has_default[["item_level"]])) {
      if (isTRUE(missing_in_parent["item_level"])) {
        .meta_data <- try(eval(caller_defaults[["item_level"]], parent.frame(2)),
                          silent = TRUE)
        if (util_is_try_error(.meta_data)) {
          .meta_data <- try(eval(caller_defaults[["item_level"]], parent.frame(1)),
                            silent = TRUE)
        }
      } else {
        .meta_data <- try(eval(quote(item_level), parent.frame(1)),
                          silent = TRUE)
      }
      if (inherits(.meta_data, "try-error")) {
        cnd <- attr(.meta_data, "condition")
        cnd$call <- sys.call(1)
        util_error(cnd)
      }
    }
  }
  if (missing(.meta_data)) {
    if (exists("item_level", envir = parent.frame()) &&
        !("item_level" %in% caller_formals)) {
      .meta_data <- get("item_level", envir = parent.frame())
    } else if (exists("meta_data", envir = parent.frame()) &&
               !("meta_data" %in% caller_formals)) {
      .meta_data <- get("meta_data", envir = parent.frame())
    }
  }
  if (missing(.meta_data)) {
    if ("item_level" %in% prep_list_dataframes()) {
      .meta_data <- util_expect_data_frame("item_level")
    }
  }
  if (!missing(.meta_data) && !is.data.frame(.meta_data)) {
    if (util_is_try_error(try(util_expect_data_frame(.meta_data, keep_types = FALSE),
        silent = TRUE))) {
      .meta_data <- rlang::missing_arg()
      # util_error("Need metadata as a data frame", applicability_problem = TRUE)
    }
  }
  if (missing(.meta_data)) {
    w <- paste("Missing %s, try to guess a preliminary one from the data",
           "using %s. Please consider amending this minimum guess manually.")
    if (requireNamespace("cli", quietly = TRUE)) {
      w <- cli::bg_red(cli::col_br_yellow(w))
    }

    util_warning(
      w,
      dQuote("meta_data"),
      dQuote("prep_study2meta"),
      applicability_problem = TRUE, immediate = TRUE
    )
    .meta_data <- prep_study2meta(.study_data, level =
                                    VARATT_REQUIRE_LEVELS$REQUIRED) # recommended would include part vars, which causes many warnings
  } else if (!is.data.frame(.meta_data)) {
    util_error("Need metadata as a data frame", applicability_problem = TRUE)
  }

  .meta_data <- util_normalize_clt(meta_data = .meta_data)
  # prep_add_data_frames(item_level = .meta_data)

  # if no meta_data have been provided -> error
  e <- new.env(parent = environment())
  e$meta_data <- .meta_data
  .meta_data <-
    eval(
      quote(try(util_expect_data_frame(meta_data), silent = TRUE)),
      e
    )

  if (inherits(.meta_data, "try-error")) {
    util_error("Need metadata as a data frame: %s",
               conditionMessage(attr(.meta_data, "condition")))
  }

  util_expect_data_frame(.meta_data)

  if (!prod(dim(.meta_data))) {
    util_warning(
      c("Missing %s, try to guess a preliminary one from the data using %s.",
        "Please consider amending this minimum guess manually."),
      dQuote("meta_data"),
      dQuote("prep_prepare_dataframes"),
      applicability_problem = TRUE
    )
    .meta_data <- prep_study2meta(.study_data, level =
                                    VARATT_REQUIRE_LEVELS$REQUIRED)
  }

  if (.internal) {
    assign("already_ppdf", TRUE, envir = parent.frame())
  }

  if (is.null(.label_col)) {
    .label_col <- VAR_NAMES
  }

  study_data <- .study_data
  meta_data <- .meta_data
  label_col <- .label_col

  if (any(is.na(colnames(study_data)))) {
    util_error("%s must not feature columns with columns names being %s",
               sQuote("study_data"),
               sQuote("NA"), applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  if (case_insens) {
    colnames(study_data) <-
      util_align_colnames_case(.colnames = colnames(study_data),
                               .var_names = meta_data[[VAR_NAMES]])
  }

  # Exchanged to "label_col"
  if (!exists("label_col")) {
    label_col <- VAR_NAMES
  }

  try(if (missing(label_col)) {
    label_col <- VAR_NAMES
  }, silent = TRUE)

  util_expect_scalar(label_col, check_type = is.character,
                     error_message =
                       sprintf("%s needs to be of type character",
                               sQuote("label_col")))

  # TODO: include a check that VAR_NAMES/label_col exists, and that they are unique and not too long
  # intermediate solution - fill empty labels, since we will map to metadata label columns, and otherwise empty fields will cause errors
  for (lcol in unique(c(label_col, LABEL, LONG_LABEL))) {
    if (is.data.frame(meta_data) && lcol %in% colnames(meta_data)) {
      meta_data[[lcol]][which(util_empty(meta_data[[lcol]]))] <-
        meta_data[[VAR_NAMES]][which(util_empty(meta_data[[lcol]]))]
    }
  }

  meta_data <- prep_meta_data_v1_to_item_level_meta_data(meta_data =
                                                           meta_data,
                                                         verbose = FALSE,
                                                         label_col = label_col)

  if (!(DATA_TYPE %in% colnames(meta_data)) ||
      any(util_empty(meta_data[[DATA_TYPE]])) ||
      !all(meta_data[[DATA_TYPE]] %in% DATA_TYPES)) {
    if (!(DATA_TYPE %in% colnames(meta_data))) {
      meta_data[[DATA_TYPE]] <- NA_character_
    }

    meta_data <- .util_fix_data_types(meta_data, study_data)
  }

  if (!.called_in_pipeline) meta_data <- util_validate_known_meta(meta_data)

  if (.replace_missings) {
    # Are missing codes replaced?
    if (!isTRUE(attr(study_data, "Codes_to_NA"))) {
      study_data <-
        util_replace_codes_by_NA(
          study_data = study_data, meta_data = meta_data,
          split_char = SPLIT_CHAR, sm_code = .sm_code)
    }
  }

  study_data_hash <- rlang::hash(.study_data)
  meta_data_hash <- rlang::hash(.meta_data)
  .tabs <- sort(unique(unlist(meta_data[,
                                       endsWith(
                                         colnames(
                                           meta_data),
                                         "_TABLE")])))
  .tabs <- .tabs[!is.na(.tabs)]
  meta_tabs_hash <- rlang::hash(lapply(.tabs,
                                       function(tb) {
                                         try(prep_get_data_frame(tb),
                                             silent = TRUE)
                                       }))
  a <- formals(prep_prepare_dataframes)[.to_combine]
  a2 <- rlang::call_args(sys.call())
  a2 <- a2[intersect(names(a2), .to_combine)]
  a[names(a2)] <- a2
  missing_from_a <- vapply(a, rlang::is_missing, FUN.VALUE = logical(1))
  a[missing_from_a] <- mget(names(which(missing_from_a)))
  a <- rlang::hash(a)
  key <-
    paste0(a,
           "@", study_data_hash,
           "@", meta_data_hash,
           "@", meta_tabs_hash,
           "@", label_col,
           "@", getOption("dataquieR.old_type_adjust",
                          dataquieR.old_type_adjust_default))

  if (key %in% names(.study_data_cache)) {
    if (getOption("dataquieR.study_data_cache_metrics",
                  dataquieR.study_data_cache_metrics_default)) {
      e <- getOption("dataquieR.study_data_cache_metrics_env",
                     dataquieR.study_data_cache_metrics_env_default)
      if (!is.environment(e) ||
          rlang::env_is_locked(e) ||
          any(rlang::env_binding_are_locked(e, intersect(
            names(e),
            c("usage"))))) {
        util_warning(c("in `option()` %s, %s expects an unlocked",
                       "environment with an unlocked binding in %s.
                       Use %s, instead."),
                     sQuote("dataquieR.study_data_cache_metrics_env"),
                     sQuote(packageName),
                     sQuote("usage"),
                     sQuote('dataquieR.study_data_cache_metrics_env_default'))
        e <- dataquieR.study_data_cache_metrics_env_default
        e$usage <- list()
      }
      if (is.null(e$usage)) e$usage <- list()
      if (is.null(e$usage[[key]])) e$usage[[key]] <- 0
      e$usage[[key]] <- e$usage[[key]] + 1
    }
    ds1 <- .study_data_cache[[key]]
    study_data <- attr(ds1, "study_data") # IDEA, but not robust enough: use a hash as name and keep the dataframe in the cache only once
  } else {
    ds1 <- NULL
  }

  # If study_data exist and metadata were already mapped, then we can return ds1
  # directly, but only if all requested modifications are already considered
  # (if possible).
  if (isTRUE(attr(study_data, "MAPPED", exact = TRUE))) {
    ds1 <- study_data
    study_data <- attr(ds1, "study_data")
    # labels can be modified easily
    if (attr(ds1, "label_col") != label_col) {
      colnames(ds1) <-
        util_map_labels(colnames(ds1),
                        meta_data = meta_data,
                        from = attr(ds1, "label_col"),
                        to = label_col)
      attr(ds1, "label_col") <- label_col
    }
    ds1_ready <- TRUE
    # replacement of missing value codes can not be undone
    if (isTRUE(attr(ds1, "Codes_to_NA")) && !.replace_missings) {
      ds1_ready <- FALSE
    }
    # replacement of hard limits can not be undone
    if (isTRUE(attr(ds1, "HL_viol_to_NA")) &&
        !.replace_hard_limits) {
      ds1_ready <- FALSE
    }
    # replacement of hard limits can not be undone, also, it may prevent hard limit replacement, e.g.
    if (isTRUE(attr(ds1, "apply_fact_md"))) {
      ds1_ready <- FALSE
    }
    if (isTRUE(attr(ds1, "apply_fact_md_inadm"))) {
      ds1_ready <- FALSE
    }
    # data type correction can not be undone
    if (isTRUE(attr(ds1, "Data_type_matches")) &&
        !.adjust_data_type) {
      ds1_ready <- FALSE
    }
    check_sl <-
      (.amend_scale_level && SCALE_LEVEL %in% colnames(meta_data) &&
         !any(util_empty(meta_data[[SCALE_LEVEL]][meta_data$VAR_NAMES %in%
                                                    colnames(study_data)]))) ||
      !.amend_scale_level

    ds1_ready <- ds1_ready &&
      check_sl

    if (ds1_ready) {
      if (.internal) {
        assign("study_data", study_data, parent.frame())
        assign("meta_data", meta_data, parent.frame())
        assign("ds1", ds1, parent.frame())
        assign("label_col", label_col, parent.frame())
      }
      return(invisible(study_data))
    } else {
      ds1 <- NULL
    }
  }

  if (!"VAR_NAMES" %in% colnames(meta_data)) {
    # Should get caught before by 'util_validate_known_meta'.
    util_error("'VAR_NAMES' not found in metadata [%s]",
               paste0(colnames(meta_data), collapse = ", "),
               applicability_problem = TRUE)
  }

  if (!isTRUE(attr(study_data, "MAPPED", exact = TRUE))) {
    study_data <- study_data[, order(colnames(study_data)), FALSE]
    meta_data <- meta_data[order(meta_data[[VAR_NAMES]]), , FALSE]
  }

  relevant_vars_for_warnings <- NULL

  # adjust data types, if enabled
  util_stop_if_not(
    `Pipeline should never request study data w/ unchanged datatypes, sorry, internal error, please report` =
                     !(!.adjust_data_type && .called_in_pipeline))
  if (.adjust_data_type || .apply_factor_metadata) {
    relevant_vars_for_warnings <- lapply(
      setNames(nm = names(.meta_data_env)[endsWith(names(.meta_data_env), "_vars")]), # all arguments populated by the pipeline with some variable references
      util_find_indicator_function_in_callers
    )
    if (is.null(relevant_vars_for_warnings$resp_vars)) { # original call was not for item level, so no filtering of problems by related items
      relevant_vars_for_warnings <- NULL
    }
    relevant_vars_for_warnings <- unlist(relevant_vars_for_warnings,
                                         recursive = TRUE)

    # Important: Remove all non-character matches (maybe, our call-stack kept the metadata-env-look-up-functions, whyever)
    relevant_vars_for_warnings <-
      relevant_vars_for_warnings[vapply(relevant_vars_for_warnings,
                                         is.character, FUN.VALUE = logical(1))]

    relevant_vars_for_warnings <-
      relevant_vars_for_warnings[!util_empty(relevant_vars_for_warnings)]

    try(relevant_vars_for_warnings <- # TODO: sometimes, relevant... does not have variables from label_col, if correct variable use used find var by names and mapped them to varnames already # this corresponds with TODO POSSIBLE_VARS in util_correct_variable_use
          util_find_var_by_meta(relevant_vars_for_warnings,
                                meta_data = meta_data,
                                label_col = label_col,
                                target = VAR_NAMES,
                                allowed_sources =
                                  c(VAR_NAMES, LABEL, LONG_LABEL, label_col),
                                ifnotfound = relevant_vars_for_warnings),
        silent = TRUE)

  }
  if (.adjust_data_type && !.called_in_pipeline) {
    study_data <- util_adjust_data_type(
      study_data = study_data,
      meta_data = meta_data,
      relevant_vars_for_warnings = relevant_vars_for_warnings)
  }

  if (.amend_scale_level && (!(SCALE_LEVEL %in% colnames(meta_data)) ||
                             any(util_empty(
                               meta_data[[SCALE_LEVEL]][meta_data$VAR_NAMES %in%
                                                        colnames(study_data)])))) {
    util_message(c(
      "Missing some or all entries in %s column in item-level %s. Predicting",
                   "it from the data -- please verify these predictions, they",
                   "may be wrong and lead to functions claiming not to be",
                   "reasonably applicable to a variable."),
                   sQuote(SCALE_LEVEL), "meta_data",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = FALSE)

    meta_data <- prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col
    )
  }

  # create ds1 -----------------------------------------------------------------
  if (is.null(ds1) || !isTRUE(attr(ds1, "MAPPED", exact = TRUE))) {
    ds1 <- util_map_all(label_col = label_col, study_data = study_data,
                        meta_data = meta_data)$df
  }
  if (!(.allow_empty) && ncol(ds1) * nrow(ds1) == 0) {
    util_error(
      "No data left. Aborting, since mapping of %s on %s was not possible",
      sQuote("meta_data"),
      sQuote("study_data"),
      applicability_problem = FALSE)
  }

  my_atts <- attributes(ds1)[.ds1_attribute_names]
  ds1 <- as.data.frame(ds1)
  attributes(ds1)[.ds1_attribute_names] <- my_atts

  meta_data <- as.data.frame(meta_data)

  .all <- ncol(ds1)
  my_atts <- attributes(ds1)[.ds1_attribute_names]
  ds1 <- ds1[, colnames(ds1) %in% meta_data[[label_col]], FALSE]
  attributes(ds1)[.ds1_attribute_names] <- my_atts
  .mapped <- ncol(ds1)
  if (.all > .mapped) { # nocov start
    # Should be dead code, because util_map_all is called above, which
    # performs an analogous check and clears out the unannotated variables
    # from the study data.
    util_warning("Lost %d variables, that I could not map using %s",
                 .all - .mapped, dQuote(label_col),
                 applicability_problem = TRUE)
  } # nocov end
  if (!.allow_empty) {
    # this would delete the metadata, if the mapping has failed totally
    meta_data <- meta_data[meta_data[[label_col]] %in% colnames(ds1), , FALSE]
  }

  if (STUDY_SEGMENT %in% colnames(meta_data) &&
      any(util_empty(meta_data[[STUDY_SEGMENT]]))) {
    dummy_name <- "SEGMENT"
    i <- 1
    while (dummy_name %in% meta_data[[STUDY_SEGMENT]]) {
      dummy_name <- sprintf("SEGMENT %d", i)
      i <- i + 1
    }
    util_message(c(
      "Some %s are NA. Will assign those to an artificial",
      "segment %s"), sQuote(STUDY_SEGMENT), dQuote(dummy_name),
      applicability_problem = TRUE
    )
    meta_data[[STUDY_SEGMENT]][util_empty(meta_data[[STUDY_SEGMENT]])] <-
      dummy_name
  }

  if (VARIABLE_ORDER %in% colnames(meta_data)) {
    meta_data[] <- meta_data[order(meta_data[[VARIABLE_ORDER]]), , drop = FALSE]
    vars <- meta_data[, label_col, drop = TRUE]
    vars <- vars[vars %in% colnames(ds1)]
    my_atts <- attributes(ds1)[.ds1_attribute_names]
    ds1 <- ds1[, vars, drop = FALSE]
    attributes(ds1)[.ds1_attribute_names] <- my_atts
  }

  if (!isTRUE(attr(study_data, "MAPPED", exact = TRUE)) &&
      PART_VAR %in% colnames(meta_data)) local({
    .kssvs <- intersect(colnames(study_data), meta_data[, PART_VAR])
    for (rv in .kssvs) {
      vals <- util_replace_codes_by_NA(
        study_data = study_data[, rv, drop = FALSE],
        meta_data = meta_data, sm_code = .sm_code)[[rv]]
      vals <- vals[!is.na(vals)]
      if (is.character(vals)) vals <- vals[trimws(vals) != ""]
      vals <- suppressWarnings(as.numeric(vals))
      if (!all(vals %in% c(0:1, NA), na.rm = TRUE)) {
        util_warning(c( # TODO: Maybe too frequently shown? see relevant_vars_for_warnings above
          "Found entries different from TRUE/FALSE/1/0 and <empty>, segment",
          "participation is expected, if values different from 0 are found.",
          "For the segment indicator variable %s, a table of inadmissible",
          "values: %s"),
          dQuote(rv),
          paste(capture.output(print(table(setdiff(vals, c(0:1, NA))))),
                collapse = "\n"))
      }
    }
  })

  attr(study_data, "Codes_to_NA") <- .replace_missings
  attr(ds1, "Codes_to_NA") <- .replace_missings

  attr(ds1, "MAPPED") <- TRUE
  attr(ds1, "label_col") <- label_col
  attr(ds1, "Data_type_matches") <- .adjust_data_type

  if (.replace_hard_limits) {
    # Are hard limit violations replaced?
    if (!isTRUE(attr(ds1, "HL_viol_to_NA"))) {
      ds1 <-
        util_replace_hard_limit_violations(
          study_data = ds1, meta_data = meta_data, label_col = label_col)
    }
  }

  attr(ds1, "HL_viol_to_NA") <- .replace_hard_limits

  if (.apply_factor_metadata) {
    relevant_vars_for_warnings_lb <-
      util_map_labels(relevant_vars_for_warnings, meta_data = meta_data,
                      from = VAR_NAMES, to = label_col,
                      relevant_vars_for_warnings)

    # convert columns to factors?
    if (!isTRUE(attr(ds1, "apply_fact_md"))) {
      which_cols <- intersect(meta_data[[attr(ds1, "label_col")]],
                              colnames(ds1))
      ds1[, which_cols] <- lapply(which_cols,
                                  function(cl) { # IDEA: What to do with missing labels for numerical variables?
                                    # prep_load_workbook_like_file("meta_data_v2")
                                    # (prep_prepare_dataframes(.study_data = "study_data", .meta_data = il, .label_col = LABEL, .apply_factor_metadata = TRUE))
                                    if (!(
                                      SCALE_LEVEL %in% colnames(meta_data))) {
                                      util_error(c("%s was called with %s = %s",
                                                   "and %s = %s",
                                                   "but %s does not provide a",
                                                   "column %s -- please provide",
                                                   "this column or call with ",
                                                   "%s = %s"),
                                                 sQuote(rlang::call_name(sys.call(1))),
                                                 sQuote(".apply_factor_metadata"),
                                                 dQuote(.apply_factor_metadata),
                                                 sQuote(".amend_scale_level"),
                                                 dQuote(.amend_scale_level),
                                                 "meta_data",
                                                 sQuote(SCALE_LEVEL),
                                                 sQuote(".amend_scale_level"),
                                                 dQuote(TRUE),
                                                 applicability_problem = TRUE)
                                    }
                                    sl <- meta_data[meta_data[[attr(ds1, "label_col")]] == cl,
                                                    SCALE_LEVEL, drop = TRUE]
                                    if (sl %in% c(
                                      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$ORDINAL
                                    )) {
                                      util_stop_if_not(
                                        `Internal error, sorry, please report: unexp. VALUE_LABELS` =
                                          util_empty(meta_data[
                                            meta_data[[attr(ds1, "label_col")]] == cl,
                                            VALUE_LABELS, drop = TRUE]))
                                      vlt <- try(prep_get_data_frame(
                                        meta_data[meta_data[[attr(ds1, "label_col")]] == cl,
                                                  VALUE_LABEL_TABLE, drop = TRUE]), silent = TRUE)
                                      levels <- NULL
                                      labels <- NULL
                                      if (!util_is_try_error(vlt)) {
                                        if (CODE_VALUE %in% colnames(vlt)) {
                                          levels <- vlt[[CODE_VALUE]]
                                        }
                                        if (CODE_LABEL %in% colnames(vlt)) {
                                          labels <- vlt[[CODE_LABEL]]
                                        }
                                        if (is.null(levels) && !is.null(labels)) {
                                          util_error(applicability_problem = TRUE,
                                                     "Have only code labels, but not code levels for %s",
                                                     dQuote(cl)
                                          )
                                        } else if (is.null(labels)) {
                                          labels <- levels
                                        }
                                      }
                                      ordered <- sl == SCALE_LEVELS$ORDINAL
                                      if (is.null(levels)) {
                                        levels <- unique(sort(ds1[[cl]]))
                                      }
                                      if (is.null(labels)) {
                                        labels <- levels
                                      }
                                      if (!.replace_missings) {
                                        .m <- util_get_code_list(x = cl,
                                                           code_name = MISSING_LIST,
                                                           mdf = meta_data,
                                                           label_col = label_col,
                                                           warning_if_no_list = FALSE,
                                                           warning_if_unsuitable_list = FALSE)
                                        .j <- util_get_code_list(x = cl,
                                                           code_name = JUMP_LIST,
                                                           mdf = meta_data,
                                                           label_col = label_col,
                                                           warning_if_no_list = FALSE,
                                                           warning_if_unsuitable_list = FALSE)
                                        defined <- levels %in% c(unname(.m),
                                                                 unname(.j))
                                        levels <- levels[!defined]
                                        labels <- labels[!defined]

                                        levels <- c(levels, unname(.j))
                                        labels <- c(labels, names(.j))
                                        levels <- c(levels, unname(.m))
                                        labels <- c(labels, names(.m))
                                      }
                                      if (.apply_factor_metadata_inadm) {
                                        empir <- unique(ds1[[cl]])
                                        empir <- empir[!is.na(empir)]
                                        empir <- setdiff(empir, levels)
                                        levels <- c(levels, empir)
                                        labels <- c(labels, empir)
                                      }
                                      .fct <- do.call(factor, list(
                                        x = ds1[[cl]],
                                        levels = levels,
                                        labels = labels,
                                        ordered = ordered
                                      ), quote = TRUE);
                                      if
                                        (any(is.na(.fct) != is.na(ds1[[cl]]))) {
                                        hint <- sprintf(paste(
                                          c("Found the following",
                                            "inadmissible categorical",
                                            "values in %s: %s -- I've",
                                            "removed them.")),
                                          sQuote(cl),
                                          util_pretty_vector_string(
                                            unique(ds1[[cl]][is.na(.fct) !=
                                                               is.na(ds1[[cl]])])
                                          ))
                                      } else {
                                        hint <- character(0)
                                      }
                                      .fct <-
                                        util_attach_attr(
                                          .fct,
                                          hint = hint)
                                    } else {
                                      .fct <- ds1[[cl]]
                                    }
                                    return(.fct)
                                  }
      )
    }
    # message for hints in columns - currently only inadm.cat.values.
    for (cl in intersect(relevant_vars_for_warnings_lb,
                         colnames(ds1))) {
      if (length(attr(ds1[[cl]], "hint")) > 0 &&
          !all(is.na(attr(ds1[[cl]], "hint")))) {
        util_message(attr(ds1[[cl]], "hint"))
      }
    }

    ##
  }

  attr(ds1, "apply_fact_md") <- .apply_factor_metadata
  attr(ds1, "apply_fact_md_inadm") <- .apply_factor_metadata_inadm

  study_data <- util_cast_off(study_data, "study_data", TRUE)
  meta_data <- util_cast_off(meta_data, "meta_data", TRUE)
  ds1 <- util_cast_off(ds1, "ds1", TRUE)
  attr(ds1, "study_data") <- study_data

  if (.internal) {
    assign("study_data", study_data, parent.frame())
    assign("meta_data", meta_data, parent.frame())
    assign("ds1", ds1, parent.frame())
    assign("label_col", label_col, parent.frame())
  }

  dataquieR.study_data_cache_max <-
    getOption("dataquieR.study_data_cache_max",
              dataquieR.study_data_cache_max_default)

  if (is.data.frame(ds1) &&
      !(key %in% names(.study_data_cache))) {
      if (sum(object.size(ds1),
              vapply(.study_data_cache, object.size, FUN.VALUE = numeric(1)),
              na.rm = TRUE) <= dataquieR.study_data_cache_max) {
        .study_data_cache[[key]] <- util_attach_attr(ds1, call = sys.call())
      } else if (getOption("dataquieR.study_data_cache_metrics",
                           dataquieR.study_data_cache_metrics_default)) {
        class(dataquieR.study_data_cache_max) <- "object_size"
        rlang::inform(
          sprintf(
            paste(
              "Maximum size for study_data cache (%s) reached, not caching.",
              "Adjust the %s to control"),
            format(dataquieR.study_data_cache_max, units = "auto"),
            sQuote("option(dataquieR.study_data_cache_max = )")
          ),
          .frequency = "once", .frequency_id = paste(key)
        )
      }
  }

  if (.internal) {
    invisible(ds1)
  } else {
    ds1
  }
}

util_maybe_load_meta_data_v2 <- function() {
  if ("meta_data_v2" %in% rlang::fn_fmls_names(fn = rlang::caller_fn(1))) {
    eval.parent(substitute({
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
    }))
  } else {
    util_error(c("Internal error: Need a %s formal, sorry. Please report.",
                 "As a dataquieR developer: You need to declare a formal",
                 "argument %s, if you want to call %s."),
               sQuote("meta_data_v2"),
               sQuote("meta_data_v2"),
               sQuote("util_maybe_load_meta_data_v2"))
  }
}

# fixes aliases for all metadata levels except item level for the data frame names as well as for arguemnts of exported functions
util_ck_arg_aliases <- function() {
  arg_maps_to = c( # this function does not handle item_level/meta_data, because this pair is addressed by prep_prepare_dataframes()
    segment_level = "meta_data_segment",
    cross_item_level = "meta_data_cross_item",
    `cross-item_level` = "meta_data_cross_item",
    dataframe_level = "meta_data_dataframe",
    item_computation_level =
      "meta_data_item_computation"
  )

  # handle metadata argument aliases --------
  caller_formal_names <- rlang::fn_fmls_names(fn = rlang::caller_fn(1))
  caller_formals <- rlang::fn_fmls(fn = rlang::caller_fn(1))
  missing_in_parent <- suppressWarnings({
    vapply(caller_formal_names, function(x) eval(call("missing",
                                                         as.symbol(x)),
                                                    envir = parent.frame(3)),
           FUN.VALUE = logical(1))
  })

  for (arg in intersect(names(arg_maps_to),
                        caller_formal_names)) {
    list_of_synonyms <- names(arg_maps_to)[arg_maps_to == arg_maps_to[[arg]]]
    for (syn in setdiff(
      union(list_of_synonyms, arg_maps_to[[arg]]), arg)) {
      if (syn %in% caller_formal_names) { # have both
        if (!missing_in_parent[[syn]] &&
            !missing_in_parent[[arg]] &&
            !identical(dynGet(syn), dynGet(arg))) {
          # IDEA: if (rlang::hash())
          util_error(c("You cannot provide both, %s as well as %s",
                       "these arguments are synonyms and must be",
                       "used mutually exclusively"),
                     sQuote(syn),
                     sQuote(arg))
        }
      }
    }
    v <- rlang::missing_arg()
    if (!missing_in_parent[[arg]]) {
      v <- eval.parent(as.symbol(arg))
    } else for (syn in setdiff(
      union(list_of_synonyms, arg_maps_to[[arg]]), arg)) {
      if (!missing_in_parent[[syn]]) {
        v <- eval.parent(as.symbol(syn))
      }
    }
    if (!missing(v)) {
      assign(arg_maps_to[[arg]], v, envir = parent.frame())
      for (syn in list_of_synonyms) {
        assign(syn, v, envir = parent.frame())
      }
    }
  }

  # handle data frame alias names --------
  # order does matter, because if the above stops,
  # the data frame cache stays untouched
  df_names <- prep_list_dataframes()
  cils2rn <- grep("(^|\\|)\\s*cross_item_level$", perl =  TRUE, value = TRUE,
                  df_names)
  for (cil2rn in cils2rn) { # rename dataframes named cross_item_level
    new_nm <- sub("cross_item_level$", "cross-item_level", cil2rn)
    if (!new_nm %in% df_names) {
      prep_add_data_frames(
        data_frame_list =
          setNames(list(
            prep_get_data_frame(cil2rn)
          ), nm = new_nm)
      )
    }
  }

}

# used, in case ds1 is copied or similar to keep the attributes in place
.ds1_attribute_names <- c("Codes_to_NA", "MAPPED", "label_col", "HL_viol_to_NA",
                          "Data_type_matches", "apply_fact_md",
                          "apply_fact_md_inadm", "study_data",
                          "normalized", "version")

.util_fix_data_types <- function(meta_data, study_data) {
  if (missing(study_data))
    study_data <- data.frame()
  which_wrong <-
    util_empty(meta_data[[DATA_TYPE]]) |
    !(meta_data[[DATA_TYPE]] %in% DATA_TYPES)

  wrong_names <- meta_data[which_wrong, VAR_NAMES, drop = TRUE]

  wrong_names <- intersect(colnames(study_data), wrong_names)

  datatypes <- prep_datatype_from_data(resp_vars = wrong_names,
                                       study_data = study_data)

  if (length(datatypes) != 0) {
    util_warning(
      c("For the variables %s, I have no valid %s in the %s. I've predicted",
        "the %s from the %s yielding %s."),
      util_pretty_vector_string(wrong_names),
      sQuote(DATA_TYPE),
      sQuote("meta_data"),
      sQuote(DATA_TYPE),
      sQuote("study_data"),
      dQuote(prep_deparse_assignments(names(datatypes), datatypes,
                                      mode = "string_codes")),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )
  }
  meta_data[which_wrong, DATA_TYPE] <-
    datatypes[meta_data[which_wrong, VAR_NAMES]]

  meta_data
}
