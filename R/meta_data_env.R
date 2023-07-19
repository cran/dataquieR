# This file creates an environment with functions, that
# employ meta_data found in this environment (must be placed there by their
# caller, before)
# used by [util_generate_calls_for_function]

#' `.meta_data_env` -- an environment for easy metadata access
#'
#' used by the dq_report2-pipeline
#' @seealso [meta_data_env_id_vars] [meta_data_env_co_vars]
#'          [meta_data_env_time_vars] [meta_data_env_group_vars]
#' @name meta_data_env
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
                                           target = STUDY_SEGMENT))
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

#' Extract selected outlier criteria for a given item or variable group
#' @param entity vector of item- or variable group identifiers
#' @details
#' In the environment, `target_meta_data` should be set either to
#' `item_level` or to `cross-item_level`.
#' @return a vector with id-variables for each entity-entry, having the
#'         `explode` attribute set to `FALSE`
#' @seealso [meta_data_env]
#' @name meta_data_env_criteria
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
.meta_data_env$n_rules <- function(entity) {
  util_expect_scalar(entity, check_type = is.character)
  if (target_meta_data == "cross-item_level") {
    r <- unname(unlist(util_parse_assignments(
      meta_data_cross_item[!util_empty(meta_data_cross_item[[CHECK_ID]]) &
                              meta_data_cross_item[[CHECK_ID]] ==
                             entity, N_RULES])))

  } else if (target_meta_data == "item_level") {
    r <- unname(unlist(util_find_var_by_meta(entity,
                                      meta_data,
                                      label_col = label_col,
                                      target = N_RULES,
                                      ifnotfound = NA_integer_)))
  } else {
    util_error()
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

.meta_data_env$resp_vars <- function(resp_vars) {
  util_stop_if_not(length(resp_vars) == 1)
  only_roles <- util_get_concept_info("implementations", get("function_R")
                                   == fkt, "only_roles")[["only_roles"]]
  if (length(only_roles) == 1 && !util_empty(only_roles)) {
    only_roles <- util_parse_assignments(only_roles)
  } else {
    only_roles <- c(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$SECONDARY)
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
  if (length(r) > 0)
    attr(r, "explode") <- TRUE
  r
}

# make all the functions in the environment enclosed by this environment, too,
# so that they can look up this environment for metadata
for (f in ls(.meta_data_env)) {
  if (is.function(.meta_data_env[[f]])) {
    environment(.meta_data_env[[f]]) <- .meta_data_env
  }
}
