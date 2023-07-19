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
#' Different from the other utility function that work
#' in the caller's environment, so it modifies objects in the calling function.
#' It defines a new object `ds1`, it modifies `study_data` and/or `meta_data`
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
#' @param .allow_empty allow `ds1` to be empty. i.e., 0 rows and/or 0 columns
#'
#' @seealso acc_margins
#'
#' @return `ds1` the study data with mapped column names
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
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"))
#' load(system.file("extdata/study_data.RData", package = "dataquieR"))
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
#'
prep_prepare_dataframes <- function(.study_data, .meta_data, .label_col,
                                    .replace_hard_limits,
                                    .replace_missings, .sm_code = NULL,
                                    .allow_empty = FALSE) {

#  dimension <- substr(rlang::call_name(rlang::caller_call()), 1, 3)
  # TODO: Add missing tables from meta_data$MISSING_LIST_TABLE

  util_expect_scalar(.sm_code,
                     check_type = util_all_is_integer,
                     allow_null = TRUE)

  if (missing(.replace_hard_limits)) .replace_hard_limits <- FALSE
  util_expect_scalar(.replace_hard_limits, check_type = is.logical)

  util_expect_scalar(.allow_empty, check_type = is.logical)
  if (!missing(.replace_missings) && (length(.replace_missings) != 1 ||
                                      !is.logical(.replace_missings) ||
                                      is.na(.replace_missings))) {
    util_error(
      "Called prepare_dataframes with .replace_missings not being logical(1)",
      applicability_problem = TRUE)
  }
# TODO: include check for (hard) limits similar to replace_missings
  callfn <- caller_fn(1)

  caller_defaults <- suppressWarnings(formals(callfn))
  caller_formals <- names(caller_defaults)
  caller_has_default <- suppressWarnings({
    !vapply(formals(callfn), identical, formals(function(x) {
        })[["x"]], FUN.VALUE = logical(1))
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
        if (exists("study_data", parent.frame())) { # FIXME: Test this and the same for metadata
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
      quote(try(util_expect_data_frame(study_data), silent = TRUE)),
      e
    )
  if (inherits(.study_data, "try-error")) {
    util_error("Need study data as a data frame: %s",
               conditionMessage(attr(.study_data, "condition")))
  }

  if (missing(.meta_data)) {
    if ("meta_data" %in% caller_formals) {
      if (!eval.parent(substitute({
        missing(meta_data)
      }))) {
        if (exists("meta_data", parent.frame())) {
          .meta_data <- try(eval(quote(meta_data), envir = parent.frame()),
                             silent = TRUE)
          if (inherits(.meta_data, "try-error")) {
            cnd <- attr(.meta_data, "condition")
            cnd$call <- sys.call(1)
            util_error(cnd)
          }
        } else {
          util_error("object %s not found", dQuote("meta_data"))
        }
      } else if (caller_has_default[["meta_data"]]) {
        .meta_data <- try(eval.parent(caller_defaults[["meta_data"]], n = 2),
                          silent = TRUE)
        if (inherits(.meta_data, "try-error")) {
          cnd <- attr(.meta_data, "condition")
          cnd$call <- sys.call(1)
          util_error(cnd)
        }
      } else {
        if ("item_level" %in% prep_list_dataframes()) {
          .meta_data <- util_expect_data_frame("item_level")
        }
        if (missing(.meta_data) || !is.data.frame(.meta_data)) {
          util_warning(
            c("Missing %s, try to guess a preliminary one from the data",
              "using %s. Please consider amending this minimum guess manually."),
            dQuote("meta_data"),
            dQuote("prep_prepare_dataframes"),
            applicability_problem = TRUE
          )
          .meta_data <- prep_study2meta(.study_data, level =
                                          VARATT_REQUIRE_LEVELS$RECOMMENDED)
        }
      }
    } else if (exists("meta_data", parent.frame())) {
      .meta_data <- get("meta_data", parent.frame())
    } else {
      .meta_data <- data.frame()
    }
  }

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

  if (is.null(.label_col)) {
    .label_col <- VAR_NAMES
  }

  study_data <- .study_data
  meta_data <- .meta_data
  label_col <- .label_col

  if (!exists("var_names")) {
    var_names <- "meta_data"
  }

  try(if (missing(var_names)) {
    var_names <- "meta_data"
  }, silent = TRUE)

  # Exchanged to "label_col"
  if (!exists("label_col")) {
    label_col <- VAR_NAMES
  }

  try(if (missing(label_col)) {
    label_col <- VAR_NAMES
  }, silent = TRUE)

  tryCatch({
      var_names <- match.arg(var_names, c("meta_data", "study_data"),
                             several.ok = FALSE)
    },
    error = function(e) {
      util_error("var_names should be 'meta_data' or 'study_data'")
    }
  )

  meta_data <- prep_meta_data_v1_to_item_level_meta_data(meta_data =
                                                           meta_data,
                                                         verbose = FALSE,
                                                         label_col = label_col)

  if (missing(.replace_missings) || .replace_missings) {
    # Are missing codes replaced?
    if (!("Codes_to_NA" %in% names(attributes(study_data)))) {
      study_data <-
        util_replace_codes_by_NA(
          study_data = study_data, meta_data = meta_data,
          split_char = SPLIT_CHAR, sm_code = .sm_code)
    }
  }

  repl_lim_viol <- quote({
    if (.replace_hard_limits) {
      # Are hard limit violations replaced?
      if (!("HL_viol_to_NA" %in% names(attributes(ds1)))) {
        ds1 <-
          util_replace_hard_limit_violations(
            study_data = ds1, meta_data = meta_data, label_col = label_col)
      }
    }
  })

  # if study_data exist and metadata were already mapped then return ds1
  # directly
  if (isTRUE(attr(study_data, "MAPPED", exact = TRUE))) {
    ds1 <- study_data
    eval(repl_lim_viol)
    assign("study_data", study_data, parent.frame())
    assign("meta_data", meta_data, parent.frame())
    assign("ds1", ds1, parent.frame())
    assign("label_col", label_col, parent.frame())
    return(invisible(study_data))
  }

  Codes_to_NA <- attr(study_data, "Codes_to_NA")

  if (var_names == "meta_data") {
    if (!"VAR_NAMES" %in% colnames(meta_data)) {
      util_error("'VAR_NAMES' not found in metadata [%s]",
                 paste0(colnames(meta_data), collapse = ", "),
                 applicability_problem = TRUE)
    }

    study_data <- study_data[, order(colnames(study_data)), FALSE]
    meta_data <- meta_data[order(meta_data[[VAR_NAMES]]), , FALSE]

    ds1 <- util_map_all(label_col = label_col, study_data = study_data,
                        meta_data = meta_data)$df
    if (!(.allow_empty) && ncol(ds1) * nrow(ds1) == 0) {
      util_error(
        "No data left. Aborting, since mapping of %s on %s was not possible",
        sQuote("meta_data"),
        sQuote("study_data"),
        applicability_problem = FALSE)
    }
  } else { # nocov start
    # unsupported now
    ds1 <- study_data
  } # nocov end

  ds1 <- as.data.frame(ds1)
  meta_data <- as.data.frame(meta_data)

  .all <- ncol(ds1)
  ds1 <- ds1[, colnames(ds1) %in% meta_data[[label_col]], FALSE]
  .mapped <- ncol(ds1)
  if (.all > .mapped) { # nocov start
    # This could only happen, if var_names would be "study_data",
    # which is not used any more.
    # for var_names == "meta_data", util_map_all is called above, which
    # performs an analogous check and clears out the unannotated variables
    # from the study data.
    util_warning("Lost %d variables, that I could not map using %s",
                 .all - .mapped, dQuote(label_col),
                 applicability_problem = TRUE)
  } # nocov end
  if (!.allow_empty) { # this would delete the metadata, if the mapping has failed totally
    meta_data <- meta_data[meta_data[[label_col]] %in% colnames(ds1), , FALSE]
  }

  if (VARIABLE_ORDER %in% colnames(meta_data)) {
    meta_data[] <- meta_data[order(meta_data[[VARIABLE_ORDER]]), , drop = FALSE]
    vars <- meta_data[, label_col, drop = TRUE]
    vars <- vars[vars %in% colnames(ds1)]
    ds1 <- ds1[, vars, drop = FALSE]
  }

  if (PART_VAR %in% colnames(meta_data)) local({
    .kssvs <- intersect(colnames(study_data), meta_data[, PART_VAR])
    for (rv in .kssvs) {
      vals <- util_replace_codes_by_NA(
        study_data = study_data[, rv, drop = FALSE],
        meta_data = meta_data, sm_code = .sm_code)[[rv]]
      vals <- vals[!is.na(vals)]
      if (is.character(vals)) vals <- vals[trimws(vals) != ""]
      vals <- suppressWarnings(as.numeric(vals))
      if (!all(vals %in% c(0:1, NA), na.rm = TRUE)) {
        util_warning(c(
          "Found entries different from TRUE/FALSE/1/0 and <empty>, segment",
          "participation is exptected, if values different from 0 are found.",
          "For the segment indicator variable %s, a table of inadmissible",
          "values: %s"),
          dQuote(rv),
          paste(capture.output(print(table(setdiff(vals, c(0:1, NA))))),
                collapse = "\n"))
      }
    }
  })

  attr(study_data, "Codes_to_NA") <- Codes_to_NA
  attr(ds1, "Codes_to_NA") <- Codes_to_NA

  attr(ds1, "MAPPED") <- TRUE
  attr(ds1, "label_col") <- label_col

  eval(repl_lim_viol)

  assign("study_data", study_data, parent.frame())
  assign("meta_data", meta_data, parent.frame())
  assign("ds1", ds1, parent.frame())
  assign("label_col", label_col, parent.frame())

  invisible(ds1)
}

.ds1_attribute_names <- c("Codes_to_NA", "MAPPED", "label_col", "HL_viol_to_NA")
