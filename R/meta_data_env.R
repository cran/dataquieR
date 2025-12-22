# This file creates an environment with functions, that
# employ meta_data found in this environment (must be placed there by their
# caller, before)
# used by [util_generate_calls_for_function]

# CAVE: FUNCTIONS MUST NOT START WITH A . IF USED FOR POPULATION!!

#' `.meta_data_env` -- an environment for easy metadata access
#'
#' used by the dq_report2-pipeline
#' @seealso `meta_data_env_id_vars()` `meta_data_env_co_vars()`
#'          `meta_data_env_time_vars()` `meta_data_env_group_vars()`
#' @name meta_data_env
#' @keywords internal
.meta_data_env <- new.env(parent = environment())

#' Extract id variables for a given item or variable group
#' @param entity vector of item- or variable group identifiers
#' @details
#' In the environment, `target_meta_data` should be set either to
#' `item_level` or to `cross-item_level`.
#' @return a vector with id-variables for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_id_vars
#' @noRd
.meta_data_env$id_vars <- function(entity) {
  util_expect_scalar(entity, check_type = is.character)
  if (target_meta_data == "cross-item_level") {
    entity <- names(util_parse_assignments(entity))
  } else if (target_meta_data == "item_level") {
    entity <- entity
  } else {
    util_error()
  }
  segments <- unique(util_find_var_by_meta(entity,
                                           target = STUDY_SEGMENT,
                                           meta_data = meta_data))
  r <- unique(meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] %in%
                                  segments,
                                SEGMENT_ID_VARS])
  r <- names(util_parse_assignments(r))
  r <- util_find_var_by_meta( r,
                              meta_data,
                              label_col = label_col,
                              target = label_col,
                              ifnotfound = r)
  if (length(r) > 0)
    attr(r, "explode") <- FALSE
  r
}


#' Extract `MULTIVARIATE_OUTLIER_CHECK` for variable group
#' @param entity vector of item- or variable group identifiers
#' @details
#' In the environment, `target_meta_data` should be set either to
#' `item_level` or to `cross-item_level`.
#' @return a vector with flags for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_criteria
#' @noRd
.meta_data_env$multivariate_outlier_check <- function(entity) {
  util_expect_scalar(entity, check_type = is.character)
  if (target_meta_data == "cross-item_level") {
    if (!CONTRADICTION_TERM %in% colnames(meta_data_cross_item)) {
      meta_data_cross_item[[CONTRADICTION_TERM]] <- NA_character_
    }
    r <- util_empty(
      meta_data_cross_item[!util_empty(meta_data_cross_item[[CHECK_ID]]) &
                             meta_data_cross_item[[CHECK_ID]] ==
                             entity, CONTRADICTION_TERM])
    if (MULTIVARIATE_OUTLIER_CHECK %in% colnames(meta_data_cross_item)) {
      .r <- tolower(trimws(
        meta_data_cross_item[!util_empty(meta_data_cross_item[[CHECK_ID]]) &
                               meta_data_cross_item[[CHECK_ID]] ==
                               entity, MULTIVARIATE_OUTLIER_CHECK]))
      if (.r %in% c("true", "1", "t", "+")) {
        r <- TRUE
      } else if (.r %in% c("false", "0", "f", "-")) {
        r <- FALSE
      } else {
        if (!util_empty(.r)) {
          util_warning("Found invalid entry in %s, treated as missing",
                       sQuote(MULTIVARIATE_OUTLIER_CHECK),
                       applicability_problem = TRUE)
        }
        mvolc <- getOption("dataquieR.MULTIVARIATE_OUTLIER_CHECK",
                      dataquieR.MULTIVARIATE_OUTLIER_CHECK_default)
        util_expect_scalar(mvolc)
        mvolc <- tolower(trimws(as.character(mvolc)))
        if (mvolc == "true") {
          r <- TRUE
        } else if (mvolc == "false") {
          r <- FALSE
        } else {
          if (mvolc != "auto") {
            util_warning("Found invalid entry in option %s, treated as %s",
                         sQuote("dataquieR.MULTIVARIATE_OUTLIER_CHECK"),
                         dQuote(dataquieR.MULTIVARIATE_OUTLIER_CHECK_default))
          }
        }
      }
    }
  } else {
    util_error()
  }
  if (length(r) > 0)
    attr(r, "explode") <- FALSE
  r
}

#' Extract `MAHALANOBIS_THRESHOLD` for variable group
#' @param entity vector of item- or variable group identifiers
#' @details
#' In the environment, `target_meta_data` should be set either to
#' `item_level` or to `cross-item_level`.
#' @return a vector with thresholds for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_criteria
#' @noRd
.meta_data_env$mahalanobis_threshold <- function(entity) {
  util_expect_scalar(entity, check_type = is.character)
  if (target_meta_data == "cross-item_level") {
    .r <- getOption("dataquieR.MAHALANOBIS_THRESHOLD",
              dataquieR.MAHALANOBIS_THRESHOLD_default)
    if (MAHALANOBIS_THRESHOLD %in% colnames(meta_data_cross_item)) {
      .r <-
        meta_data_cross_item[!util_empty(meta_data_cross_item[[CHECK_ID]]) &
                               meta_data_cross_item[[CHECK_ID]] ==
                               entity, MAHALANOBIS_THRESHOLD]
      if (tolower(trimws(.r)) %in% c("true", "1", "t", "+")) {
        .r <- dataquieR.MAHALANOBIS_THRESHOLD_default
      }
    } else {
      return("") # do not run the check by default
    }
    r <- suppressWarnings(as.numeric(.r))
    if (!util_empty(r) && (
        r < 0 ||
        r > 1
    )) {
      r <- dataquieR.MAHALANOBIS_THRESHOLD_default
      util_warning("Found invalid entry in %s, used %s",
                   sQuote("MAHALANOBIS_THRESHOLD"),
                   dQuote(r),
                   applicability_problem = TRUE)
    }
  } else {
    util_error()
  }
  if (length(r) > 0)
    attr(r, "explode") <- FALSE
  r
}

#' Extract selected outlier criteria for a given item or variable group
#' @param entity vector of item- or variable group identifiers
#' @details
#' In the environment, `target_meta_data` should be set either to
#' `item_level` or to `cross-item_level`.
#' @return a vector with id-variables for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_criteria
#' @noRd
.meta_data_env$criteria <- function(entity) {
  util_expect_scalar(entity, check_type = is.character)
  if (target_meta_data == "cross-item_level") {
    r <- tolower(trimws(unlist(util_parse_assignments(
      meta_data_cross_item[!util_empty(meta_data_cross_item[[CHECK_ID]]) &
                             meta_data_cross_item[[CHECK_ID]] ==
                           entity, MULTIVARIATE_OUTLIER_CHECKTYPE]))))

  } else if (target_meta_data == "item_level") {
    r <- tolower(trimws(unlist(util_parse_assignments(util_find_var_by_meta(
      entity,
                                meta_data,
                                label_col = label_col,
                                target = UNIVARIATE_OUTLIER_CHECKTYPE,
                                ifnotfound = NA_character_)))))
  } else {
    util_error()
  }
  if (length(r) > 0)
    attr(r, "explode") <- FALSE
  r
}

#' Extract outlier rules-number-threshold for a given item or variable group
#' @param entity vector of item- or variable group identifiers
#' @details
#' In the environment, `target_meta_data` should be set either to
#' `item_level` or to `cross-item_level`.
#' @return a vector with id-variables for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_n_rules
#' @noRd
.meta_data_env$n_rules <- function(entity) {
  util_expect_scalar(entity, check_type = is.character)
  if (target_meta_data == "cross-item_level") {
    r <- unname(unlist(util_parse_assignments(
      meta_data_cross_item[!util_empty(meta_data_cross_item[[CHECK_ID]]) &
                              meta_data_cross_item[[CHECK_ID]] ==
                             entity, N_RULES])))

  } else if (target_meta_data == "item_level") {
    r <- unname(unlist(util_find_var_by_meta(entity,
                                      meta_data = meta_data,
                                      label_col = label_col,
                                      target = N_RULES,
                                      ifnotfound = NA_integer_)))
  } else {
    util_error()
  }
  if (identical(r, "NA")) {
    r <- NA_character_
  }
  r1 <- suppressWarnings(as.integer(r))
  if (any(is.na(r) != is.na(r1))) {
    util_warning("For %s, %s must be an integer number, it is %s",
                 dQuote(entity), sQuote(N_RULES), dQuote(r),
                 applicability_problem = TRUE)
  }
  r <- r1
  if (length(r) > 0)
    attr(r, "explode") <- FALSE
  r
}

# TODO: Add the following
#use_value_labels,  # TODO: make use_value_labels accessible from metadata
# replace_missing_by c(NA, LABEL, INTERPRET)
# use_replace_limits


#' Extract co-variables for a given item
#' @param entity vector of item-identifiers
#' @return a vector with co-variables for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_co_vars
#' @keywords internal
.meta_data_env$co_vars <- function(resp_vars) {
  util_expect_scalar(resp_vars, check_type = is.character)
  r <- lapply(intersect(colnames(meta_data), CO_VARS), function(gv) {
    r <- util_map_labels(resp_vars, meta_data, from = label_col, to = gv,
                         ifnotfound = NA_character_)
    if (all(is.na(r))) {
      return(NA_character_)
    }
    r <- names(util_parse_assignments(r))
    util_find_var_by_meta(r,
                          meta_data,
                          label_col = label_col,
                          target = label_col,
                          ifnotfound = r)

  })
  r <- r[!is.na(r)]
  r <- unlist(r)
  if (length(r) > 0)
    attr(r, "explode") <- FALSE
  r
}

#' Extract measurement time variable for a given item
#' @param entity vector of item-identifiers
#' @return a vector with time-variables (usually one per item) for each
#'         entity-entry, having the `explode` attribute set to `TRUE`
#' @seealso [meta_data_env]
#' @name meta_data_env_time_vars
#' @noRd
.meta_data_env$time_vars <- function(resp_vars) {
  util_expect_scalar(resp_vars, check_type = is.character)
  r <- vapply(FUN.VALUE = character(1),
              intersect(colnames(meta_data), TIME_VAR), function(gv) {
                r <- util_map_labels(resp_vars, meta_data, from = label_col, to = gv,
                                     ifnotfound = NA_character_)
                util_find_var_by_meta(r,
                                      meta_data,
                                      label_col = label_col,
                                      target = label_col,
                                      ifnotfound = r)
              })
  if (length(r) > 0)
    attr(r, "explode") <- TRUE
  r
}

# TODO: write an internal documentation
.meta_data_env$resp_vars <- function(resp_vars, f = fkt) { # FIXME: Make multivariate
  if (length(resp_vars) == 0) return(resp_vars)
  util_stop_if_not(length(resp_vars) == 1)
  if (COMPUTED_VARIABLE_ROLE %in% colnames(meta_data)) {
    cvr <- meta_data[meta_data[[label_col]] == resp_vars,
                      COMPUTED_VARIABLE_ROLE, TRUE]
    if (length(cvr) == 1 && !util_empty(cvr)) {
      applicable_functions <- unlist(util_parse_assignments(util_get_concept_info("ssi",
                                                   get("SSI_METRICS") == cvr,
                                                   "functions",
                                                   drop = TRUE)))
      applicable_functions <- unique(gsub("\\..*$", "", applicable_functions))
      if (fkt %in% applicable_functions) {
        return(resp_vars)
      } else {
        return(character(0))
      }
    }
  }
  only_roles <- util_get_concept_info("implementations", get("function_R")
                                   == f, "only_roles")[["only_roles"]]
  if (length(only_roles) == 1 && !util_empty(only_roles)) {
    only_roles <- util_parse_assignments(only_roles)
  } else {
    if (length(only_roles) != 1) {
      util_warning(c("Internal warning, sorry; please report: not exactly",
                     "one entry for %s inside DQ_OBS"),
                   sQuote(f))
    }
    if (startsWith(f, "int_") ||
        startsWith(f, "des_")) {
      ## default for integrity and descripors
      only_roles <- unname(vapply(VARIABLE_ROLES,  # for all variables
                                  identity,
                                  FUN.VALUE = character(1)))
    } else { # default for neither int_ nor des_
      only_roles <- c(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$SECONDARY)
    }
  }
  if (meta_data[meta_data[[label_col]] == resp_vars, VARIABLE_ROLE, TRUE]
      %in% only_roles) {
    resp_vars
  } else {
    character(0)
  }
}

#' Extract group variables for a given item
#' @param entity vector of item-identifiers
#' @return a vector with possible group-variables (can be more than  one per
#'         item) for each entity-entry, having the `explode` attribute
#'         set to `TRUE`
#' @name meta_data_env_group_vars
#' @seealso [meta_data_env]
#' @noRd
.meta_data_env$group_vars <- function(resp_vars) {
  util_expect_scalar(resp_vars, check_type = is.character)
  r <- vapply(FUN.VALUE = character(1),
              colnames(meta_data)[startsWith(colnames(meta_data),
                                             "GROUP_VAR_")], function(gv) {
                                               r <- util_map_labels(resp_vars, meta_data, from = label_col, to = gv,
                                                                    ifnotfound = NA_character_)
                                               util_find_var_by_meta(r,
                                                                     meta_data,
                                                                     label_col = label_col,
                                                                     target = label_col,
                                                                     ifnotfound = r)

                                             })
  r <- r[!util_empty(r)]
  r <- c(NA_character_, r)
  if (length(r) > 0)
    attr(r, "explode") <- TRUE
  r
}

.dqenv <- environment()
util_meta_data_env <- local({
  # make all the functions in the environment enclosed by this environment, too,
  # so that they can look up this environment for metadata
  fix_fkts <- function(e = .meta_data_env) {
    for (f in ls(e)) {
      if (is.function(e[[f]])) {
        environment(e[[f]]) <- e
      }
    }
    e
  }
  .dqenv$.meta_data_env <- fix_fkts(.meta_data_env)
  return(function(meta_data_v2,
                  label_col = LABEL,
                  item_level = "item_level",
                  meta_data_cross = "cross-item_level",
                  meta_data_segment = "segment_level",
                  meta_data_dataframe = "dataframe_level",
                  study_data) {
    .dfre <- new.env(parent = emptyenv())
    list2env(as.list(.dataframe_environment()), .dfre)
    with_dataframe_environment(
      env = .dfre,
      {
      util_maybe_load_meta_data_v2()
      clon <- rlang::env_clone(.meta_data_env)
      clon$.dfre <- .dfre
      clon$label_col <- label_col
      clon$meta_data <- data.frame(VAR_NAMES = character(0),
                                 LABEL = character(0),
                                 LONG_LABEL = character(0))
      clon$meta_data[[label_col]] <- character(0)
      try(
        clon$meta_data <- util_expect_data_frame(item_level,
                                               dont_assign = TRUE),
        silent = TRUE
      )
      if (!missing(study_data)) {
        clon$study_data <- util_expect_data_frame(study_data,
                                                dont_assign = TRUE,
                                                keep_types = TRUE)
      }
      clon$meta_data_segment <- data.frame(STUDY_SEGMENT = character(0))
      try(
        clon$meta_data_segment <- util_expect_data_frame(meta_data_segment,
                                               dont_assign = TRUE),
        silent = TRUE
      )
      clon$meta_data_cross_item <- data.frame(CHECK_ID = character(0),
                                            CHECK_LABEL = character(0))
      try(
        clon$meta_data_cross_item <- util_expect_data_frame(meta_data_cross,
                                               dont_assign = TRUE),
        silent = TRUE
      )
      clon$meta_data_dataframe <- data.frame(DF_NAME = character(0))
      try(
        clon$meta_data_dataframe <- util_expect_data_frame(meta_data_dataframe,
                                               dont_assign = TRUE),
        silent = TRUE
      )
      clon$provisionize_call <- function(cal, internal = FALSE, env) { # TODO: never pass the call, always find it on the stack to make this less quirks.
        util_expect_scalar(internal, check_type = is.logical)
        if (!internal) {
          cal <- substitute(cal)
        }
        util_stop_if_not(is.call(cal))
        util_stop_if_not(rlang::call_ns(cal) == "dataquieR")
        fkt <- rlang::call_name(cal)
        if (!internal && !(fkt %in% getNamespaceExports("dataquieR"))) {
          util_error("%s is not an exported function of %s",
                     sQuote(fkt),
                     sQuote(packageName()))
        }
        fn <- try(get(fkt, envir = asNamespace("dataquieR")), silent = TRUE)
        if (internal && util_is_try_error(fn) &&
            (identical(conditionMessage(attr(fn, "condition")),
                      "object 'FUN' not found") ||
             identical(conditionMessage(attr(fn, "condition")),
                       "invalid first argument"))) {
          fn <- try(rlang::caller_fn(3), silent = TRUE)
        }
        util_stop_if_not(is.function(fn))
        cal <- rlang::call_match(cal, fn) # normalize call, so that all arguments are nambed, if possible
        to_fill <-
          setdiff(intersect(names(formals(fn)), ls(parent.env(environment()))),
                  rlang::call_args_names(cal))
        if ("item_level" %in% names(cal) && !identical(cal$item_level,
                                                       rlang::missing_arg())) {
          to_fill <- setdiff(to_fill, "meta_data")
        }
        target_meta_data <- NULL
        if ("resp_vars" %in% names(formals(fn))) {
          if ("variable_group" %in% names(formals(fn))) {
            util_error(c("Internal error, sorry, please report:",
                         "%s must either work on item- or on cross-item-",
                         "level, but not both -- it has the formal arguments",
                         "%s as well as %s, which cannot be."),
                       sQuote(rlang::call_name(cal)),
                       sQuote("resp_vars"),
                       sQuote("variable_group")
                      )
          }
          target_meta_data <- "item_level"
          if ("resp_vars" %in% names(cal)) {
            if (!missing(env) && is.environment(env)) {
              cal$resp_vars <- eval(as.symbol("resp_vars"), envir = env)
            }
            entity <- util_find_var_by_meta(cal$resp_vars,
                                            meta_data = meta_data,
                                            label_col = label_col,
                                            target = label_col)
            if (any(is.na(entity)) && !internal) {
              util_warning(
                c("Could not find the following variables in %s: %s,",
                  "ignoring them."),
                sQuote("meta_data"),
                util_pretty_vector_string(cal$resp_vars[is.na(entity)]))
              entity <- entity[!is.na(entity)]
            }
          } else {
            entity <- formals(fn)[["resp_vars"]]
          }
        } else if ("variable_group" %in% names(formals(fn))) {
          if ("resp_vars" %in% names(formals(fn))) {
            util_error(c("Internal error, sorry, please report:",
                         "%s must either work on item- or on cross-item-",
                         "level, but not both -- it has the formal arguments",
                         "%s as well as %s, which cannot be."),
                       sQuote(rlang::call_name(cal)),
                       sQuote("resp_vars"),
                       sQuote("variable_group")
            )
          }
          target_meta_data <- "cross-item_level"
          if ("variable_group" %in% names(cal)) {
            if (!missing(env) && is.environment(env)) {
              cal$variable_group <- eval(as.symbol("variable_group"),
                                         envir = env)
            }
            entity <- cal$variable_group
          } else{
            entity <- formals(fn)[["variable_group"]]
          }
        }
        assign("target_meta_data", target_meta_data,
               envir = parent.env(environment()))
        withr::defer({
          rm("target_meta_data", envir = parent.env(environment()))
        })
        assign("fkt", fkt,
               envir = parent.env(environment()))
        withr::defer({
          rm("fkt", envir = parent.env(environment()))
        })
        for (arg in to_fill) {
          if (is.function(parent.env(environment())[[arg]]) &&
              !util_is_try_error(try(!missing(entity), silent = TRUE)) &&
              !missing(entity)) {
            .pv_args <- list(entity)
            .vals <- try(do.call(parent.env(environment())[[arg]], .pv_args), silent = TRUE)
            if (util_is_try_error(.vals)) {
              .vals <- NULL
            } else if (identical(attr(.vals, "explode"), TRUE)) {
              if (sum(!is.na(.vals)) == 1) {
                .vals <- .vals[!is.na(.vals)][[1]]
              } else {
                .vals <- .vals[!is.na(.vals)]
                if (length(.vals) == 0) {
                  rsnble <- "<none>"
                } else {
                  rsnble <- sQuote(paste0(.vals, " (", names(.vals), ")",
                                          collapse = ", "))
                }
                util_error(c("For %s, you need to specify %s explicitly,",
                             "according to your metadata, the following",
                             "values may be reasonable: %s"),
                           dQuote(entity),
                           sQuote(arg),
                           rsnble,
                           applicability_problem = TRUE,
                           intrinsic_applicability_problem = FALSE)
              }
            }
            if (!is.null(cal[[arg]]) || !is.null(.vals)) {
              cal[[arg]] <-
                .vals
            }
          } else {
            .vals <- parent.env(environment())[[arg]]
            if (!is.null(cal[[arg]]) || (!is.null(.vals) &&
                                         !is.function(.vals))) {
              cal[[arg]] <-
                .vals
            }
          }
        }
        cal
      }

      clon$call <- function(cal) {
        cl <- sys.call()
        cl[[1]] <- as.symbol("provisionize_call") # a bit hacky, and using debug output, it should not work this way, but it does?!
        with_dataframe_environment(env = .dfre,
                                   eval.parent(eval(cl)))
      }

      # Alternative solution using kind of a functinal this but with
      # cirucular references:
      # clon$call <- function(cal) {
      #   cal <- substitute(cal)
      #   prov_cal <-
      #     clon$provisionize_call(cal, internal = TRUE)
      #   with_dataframe_environment(env = .dfre,
      #                              eval.parent(prov_cal))
      #
      # }
      #
      # # The following code needs to be cleaned to save memory, but according to
      # # Gemini and ChatGPT does R a "Mark-and-Sweep" and will remove this, as
      # # soon as no reference to clon exists, any more. 2025-06-06 Str /
      # clon$clon <- clon
      # ##

      r <- fix_fkts(clon)
      r
    })
  })
})
