#' Check referred variables
#'
#' This function operates in the environment of its caller
#' (using [eval.parent], similar to [Function like C-Preprocessor-Macros](
#' https://www.programiz.com/c-programming/c-preprocessor-macros#example-define)
#' ).
#' Different from the other utility function that work
#' in the caller's environment ([util_prepare_dataframes]), It has no side
#' effects except that the argument
#' of the calling function specified in `arg_name` is normalized (set to its
#' default or a general default if missing, variable names being all white
#' space replaced by NAs).
#' It expects two objects in the caller's environment: `ds1` and `meta_data`.
#' `meta_data` is the meta data frame and `ds1` is produced by a preceding call
#' of [util_prepare_dataframes] using `meta_data` and `study_data`.
#'
#' [util_correct_variable_use] and [util_correct_variable_use2] differ only in
#' the default of the argument `role`.
#'
#' [util_correct_variable_use] and [util_correct_variable_use2] put strong
#' effort on producing compressible
#' error messages to the caller's caller (who is typically an end user of
#' a `dataquieR` function).
#'
#' The function ensures, that a specified argument of its caller that refers
#' variable names
#' (one or more as character vector) matches some expectations.
#'
#' This function accesses the caller's environment!
#'
#' @param arg_name [character] Name of a function argument of the caller
#'                             of [util_correct_variable_use]
#' @param allow_na [logical] default = FALSE. allow NAs in the variable names
#'                                            argument given in `arg_name`
#' @param allow_more_than_one [logical] default = FALSE. allow more than one
#'                                                  variable names in `arg_name`
#' @param allow_null [logical] default = FALSE. allow an empty variable name
#'                                             vector in the argument `arg_name`
#' @param allow_all_obs_na [logical] default = TRUE. check observations for not
#'                                                   being all `NA`
#' @param allow_any_obs_na [logical] default = TRUE. check observations for
#'                                               being complete without any `NA`
#' @param min_distinct_values [integer] Minimum number of distinct observed
#'                              values of a study variable
#' @param need_type [character] if not `NA`, variables must be of data type
#'                                       `need_type` according to the meta data,
#'                                       can be a pipe (`|`) separated list of
#'                                       allowed data types. Use `!` to exclude
#'                                       a type. See [DATA_TYPES] for the
#'                                       predefined variable types of the
#'                                       `dataquieR` concept.
#' @param role [character] variable-argument role. Set different defaults for
#'                         all `allow`-arguments and `need_type` of
#'                         this `util_correct_variable_use.`. If given, it
#'                         defines the intended use
#'                         of the verified argument. For typical arguments and
#'                         typical use cases, roles
#'                         are predefined in [.variable_arg_roles].
#'                         The role's defaults can be overwritten by the
#'                         arguments. If `role` is "" (default),
#'                         the standards are `allow_na = FALSE`,
#'                         `allow_more_than_one = FALSE`, `allow_null = FALSE`,
#'                         `allow_all_obs_na = TRUE`, `allow_any_obs_na = TRUE`,
#'                         and `need_type = NA`.
#'                         Use [util_correct_variable_use2] for using the
#'                         `arg_name` as default for `role`.
#'                         See [.variable_arg_roles] for currently available
#'                         variable-argument roles.
#' @param overwrite [logical] overwrite vector of variable names
#'                         to match the labels given in `label_col`.
#' @param do_not_stop [logical] do not throw an error, if one of the variables
#'                         violates `allow_all_obs_na`, `allow_any_obs_na` or
#'                         `min_distinct_values`. Instead, the variable will be
#'                         removed from `arg_name` in the parent environment
#'                         with a warning. This is helpful for functions which
#'                         work with multiple variables.
#' @param remove_not_found TODO: Not yet implemented
#'
#' @seealso [.variable_arg_roles]
#' @importFrom stats na.omit
#'
util_correct_variable_use <- function(arg_name,
                                      allow_na,
                                      allow_more_than_one,
                                      allow_null,
                                      allow_all_obs_na,
                                      allow_any_obs_na,
                                      min_distinct_values,
                                      need_type,
                                      role = "",
                                      overwrite = TRUE,
                                      do_not_stop = FALSE,
                                      remove_not_found = TRUE) { # TODO: Remove not found must be implemented
  # interpret the arg_name argument
  try({
      arg_name <- as.character(substitute(arg_name))
    }, # if called with a symbol, get that symbol as a character,
    silent = TRUE # if called with a character, do nothing
  )
  if (!all(is.character(arg_name))) { # nocov start
    # if we did not get a character up to here, there is an error in
    # calling this util_correct_variable_use
    # this should never happen anyway, so cannot test this
    util_error(c(
      "argument arg_name must be either one character or one symbol,",
      "wrong use of util_correct_variable_use?"))
  } # nocov end
  if (length(arg_name) != 1) {
    # if we have 0 or more than one argument name, this is also an
    # error in calling util_correct_variable_use
    util_error(c(
      "argument arg_name must be of length 1,",
      "wrong use of util_correct_variable_use?"))
  }

  # verify the "roles" argument
  if (role %in% .variable_arg_roles$name) {
    # if `role` is a known variable-argument role
    if (missing(allow_na)) { # if missing the allow_na argument
      allow_na <- subset(.variable_arg_roles, get("name") == role,
                         "allow_na", drop = TRUE)
    } # fetch it from the role's defaults
    if (missing(allow_more_than_one)) {
      # if missing the allow_more_than_one argument
      allow_more_than_one <- subset(.variable_arg_roles, get("name") == role,
                                    "allow_more_than_one", drop = TRUE)
    } # fetch it from the role's defaults
    if (missing(allow_null)) { # if missing the allow_null argument
      allow_null <- subset(.variable_arg_roles, get("name") == role,
                           "allow_null", drop = TRUE)
    } # fetch it from the role's defaults
    if (missing(allow_all_obs_na)) { # if missing the allow_all_obs_na argument
      allow_all_obs_na <- subset(.variable_arg_roles, get("name") == role,
                                 "allow_all_obs_na", drop = TRUE)
    } # fetch it from the role's defaults
    if (missing(allow_any_obs_na)) { # if missing the allow_any_obs_na argument
      allow_any_obs_na <- subset(.variable_arg_roles, get("name") == role,
                                 "allow_any_obs_na", drop = TRUE)
    } # fetch it from the role's defaults
    if (missing(min_distinct_values)) {
      # if missing the min_distinct_values argument
      min_distinct_values <- subset(.variable_arg_roles, get("name") == role,
                                 "min_distinct_values", drop = TRUE)
    } # fetch it from the role's defaults
    if (missing(need_type)) { # if missing the need_type argument
      need_type <- subset(.variable_arg_roles, get("name") == role,
                          "need_type", drop = TRUE)
    } # fetch it from the role's defaults
  } else { # no known variable-argument in argument `role`
    if (role != "") { # if role is not empty
      util_error(
        c("Unknown variable-argument role: %s for argument %s,",
          "wrong use of util_correct_variable_use?"),
        role, arg_name
      ) # this is again a wrong use of this function.
    }
    if (missing(allow_na)) {
      # set the default for allow_na if it has not been given
      allow_na <- FALSE
    }
    if (missing(allow_more_than_one)) {
      # set the default for allow_more_than_one if it has not been given
      allow_more_than_one <- FALSE
    }
    if (missing(allow_null)) {
      # set the default for allow_null if it has not been given
      allow_null <- FALSE
    }
    if (missing(allow_all_obs_na)) {
      # set the default for allow_all_obs_na if it has not been given
      allow_all_obs_na <- TRUE
    }
    if (missing(allow_any_obs_na)) {
      # set the default for allow_any_obs_na if it has not been given
      allow_any_obs_na <- TRUE
    }
    if (missing(min_distinct_values)) {
      # set the default for min_distinct_values if it has not been given
      min_distinct_values <- 0
    }
    if (missing(need_type)) {
      # set the default for need_type if it has not been given
      need_type <- NA
    }
  }

  p <- parent.frame(1) # access the caller's environment
  if (!exists(arg_name, envir = p)) {
    # check, if the function argument exists in the calling function.
    util_error(
      c("Unknown function argument %s checked,",
        "wrong use of util_correct_variable_use?"),
      arg_name)
  }

  # if we have a label col, use it
  if (exists("label_col", envir = p)) {
    # calling function has a label_col argument
    label_col <- try(get("label_col", envir = p), silent = TRUE)
    # try to get its value
    if (inherits(label_col, "try-error")) {
      # no value avail, i.e. no default and not given
      label_col <- VAR_NAMES # use VAR_NAMES as default
    }
  } else {
    label_col <- VAR_NAMES
    # calling function does not support label_col, so use VAR_NAMES as default
  }

  missing_in_parent <- eval.parent(call("missing", as.symbol(arg_name)))

  variable <- try(get(arg_name, envir = p), silent = TRUE)
  # try to get the value of the callers argument called `arg_name`.
  if (inherits(variable, "try-error")) {
    if (missing_in_parent) {
      util_warning(
        c("Missing argument %s without default value. Setting to NULL. As",
          "a dataquieR developer, please add a default value for %s to",
          "remove this warning."),
        dQuote(arg_name), dQuote(arg_name),
        applicability_problem = TRUE)
    } else {
      util_warning(
        c("Could not get value of argument %s for unexpected reasons. Setting",
          "to NULL."), arg_name, applicability_problem = TRUE)
      util_warning(variable, applicability_problem = TRUE)
    }
    variable <- NULL
  }

  if (!exists("ds1", envir = p)) {
    # if the calling function does not have a ds1, this is a wrong use of
    # util_correct_variable_use
    util_error(c(
      "Did not find merged study data and meta data ds1.",
      "Wrong use of util_correct_variable_use?"))
  }

  if (!exists("meta_data", envir = p)) {
    # if the calling function does not have a meta_data, this is a wrong use of
    # util_correct_variable_use
    util_error(
      "Did not find meta data. Wrong use of util_correct_variable_use?")
  }

  ds1 <- try(get("ds1", envir = p), silent = TRUE)
  # try to fetch the ds1 object from the callers environment
  if (!is.data.frame(ds1)) { # it this is not a data frame, this is again a
    # wrong use of util_correct_variable_use
    util_error(
      c("ds1 does not provide merged study data and meta data.",
        "Wrong use of util_correct_variable_use?"))
  }

  meta_data <- try(get("meta_data", envir = p), silent = TRUE)
  # try to fetch the meta_data object from the callers environment
  if (!is.data.frame(meta_data)) {
    # it this is not a data frame, this is again a wrong use of
    # util_correct_variable_use
    util_error(
      c("meta_data does not provide a meta data frame.",
        "Wrong use of util_correct_variable_use?"))
  }

  if (!allow_null && is.null(variable)) {
    # if we need the argument in arg_name, but the user provided NULL
    util_error("Argument %s is NULL", arg_name,
               applicability_problem = TRUE) # this is an error
  }

  if (allow_more_than_one) {
    # if we allow more than one variable in the argument arg_name
    if (!allow_null && length(variable) == 0) {
      # and we do need at least one variable name here, but the user did not
      # provide any
      util_error("Need at least one element in argument %s, got 0", arg_name,
                 applicability_problem = TRUE)
      # this is an error
    }
  } else { # if we expect one value in the argument arg_name at most
    if ((!allow_null && length(variable) != 1) ||
        (allow_null && length(variable) > 1)) {
      # but the user gave more than one or none although
      # allow_null prohibits this
      util_error("Need exactly one element in argument %s, got %d: [%s]",
                 arg_name, length(variable), paste0(variable, collapse = ", "),
                 applicability_problem = TRUE)
      # this is an error
    }
  }
  if (length(variable) > 0) { # if we have at least one variable name
    if (!all(is.character(variable))) { # and one name is not a character string
      util_error("Need character variable names in argument %s", arg_name,
                 applicability_problem = TRUE)
      # this is (at least up to now, may allow symbols here similar
      # to subset or with) an error
    }
    variable[trimws(variable) == ""] <- NA # replace all variable names that are
                                           # empty characters by NA.
  }
  assign(arg_name, variable, envir = p) # re-assign the possibly modified
                                    # argument value in the caller's environment
  if (!allow_na && any(is.na(variable))) {
    # if we have NAs in the list of variable names in `arg_name`
    # and this is not allowed
    util_error("Missing entries not allowed in argument %s", arg_name,
               applicability_problem = TRUE)
    # then this is an error.
  }

  # Check whether variables can be found in the data (using all possible label columns).
  possible_vars <- unique(c(meta_data[[VAR_NAMES]],
                            meta_data[[LABEL]],
                            meta_data[[LONG_LABEL]],
                            meta_data[[label_col]]))
  if (!all(na.omit(variable) %in% possible_vars)) {
    # if we have variable names in the caller's variable argument `arg_name`
    # missing in the data
    non_matching_vars <-
      na.omit(variable)[!(na.omit(variable) %in% possible_vars)] # find them
    fuzzy_match <- vapply( # and try to guess what the user wanted to put there
      non_matching_vars,
      function(v) {
        unique(possible_vars)[which.min(adist(
          trimws(v),
          trimws(unique(possible_vars)),
          ignore.case = TRUE,
          fixed = TRUE
        ))]
      }, "")
    util_error(
      "Variable '%s' (%s) not found in study data. Did you mean '%s'?",
        # then emit an informative error message.
      paste0(non_matching_vars, collapse = ", "),
      arg_name,
      paste0(fuzzy_match, collapse = ", "),
      applicability_problem = TRUE
    )
  } else {
    # If users mix VAR_NAMES and LABELs (and maybe also LONG_LABELs), we need to
    # map the variable names to the column names of ds1.
    if (!all(na.omit(variable) %in% colnames(ds1))) {
      non_matching_ind <- intersect(which(!(variable %in% colnames(ds1))),
                                    which(!is.na(variable)))
      non_matching_vars <- variable[non_matching_ind]
      other_col <- setdiff(c(VAR_NAMES, LABEL, LONG_LABEL), label_col)
      map_res <- lapply(setNames(nm = other_col), function(oc) {
        unname(
          util_map_labels(
            non_matching_vars,
            warn_ambiguous = TRUE,
            meta_data = meta_data,
            to = label_col,
            from = oc,
            ifnotfound = NA_character_
          )
        )
      })
      map_res <- as.data.frame(map_res)
      # check whether the user-specified variable could not be found in any of the other columns:
      map_res_NA <- vapply(
        1:nrow(map_res),
        FUN.VALUE = logical(1),
        FUN = function(i) {
          all(is.na(map_res[i, ]))
        }
      )
      if (any(map_res_NA)) { # may be dead code?
        util_error("Variable '%s' (%s) not found in the metadata.",
                   paste0(non_matching_vars[map_res_NA],
                          collapse = ", "),
                   arg_name,
                   applicability_problem = TRUE)
      }
      # check whether the user-specified variable was found in different label columns and maps to different variables (= ambiguity)
      map_res_ambiguous <- vapply(
        1:nrow(map_res),
        FUN.VALUE = logical(1),
        FUN = function(i) {
          length(unique(na.omit(as.character(map_res[i, ])))) > 1
        }
      )
      if (any(map_res_ambiguous)) {
        util_warning("Variable '%s' (%s) is ambiguous in the metadata.",
                     paste0(non_matching_vars[map_res_ambiguous],
                            collapse = ", "),
                     arg_name,
                     applicability_problem = TRUE)
      }

      variable[non_matching_ind] <- vapply(
          1:nrow(map_res),
          FUN.VALUE = character(1),
          FUN = function(i) {
            unique(na.omit(as.character(map_res[i, ])))[1]
            # We select here the first element, such that in case of ambiguities
            # VAR_NAME is preferred over LABEL and LONG_LABEL, and LABEL over LONG_LABEL.
            # If there are no ambiguities, this works as well.
          }
        )
      # The 'variable' vector contains now all variable names according to label_col.
      if (overwrite) {
      # Pass the 'variable' vector to the environment to use it within the calling function.
        assign(arg_name, variable, envir = parent.frame())
      }
    }
  }

  record_problem <- ifelse(do_not_stop, util_warning, util_error)
  empty_container <- new.env(parent = emptyenv())
  for (v in variable) { # now, check all variable values from the data
    if (!allow_all_obs_na && all(is.na(ds1[[v]]))) {
      # if all observations are NA and this is prohibited
      record_problem("Variable '%s' (%s) has only NA observations",
                     na.omit(v)[na.omit(v) %in% colnames(ds1)], arg_name,
                     applicability_problem = FALSE)
      empty_container[[v]] <- TRUE
    }
# names(empty_containter) gibt die geflaggten Variablen
    if (!allow_any_obs_na && any(is.na(ds1[[v]]))) {
      # if no NAs are allowed at all, but we have some
      record_problem("Variable '%s' (%s) has NA observations, which is not allowed",
                     na.omit(v)[na.omit(v) %in% colnames(ds1)], arg_name,
                     applicability_problem = FALSE)
      empty_container[[v]] <- TRUE
    }

    uniq_rv <- unique(ds1[[v]])
    uniq_rv <- uniq_rv[!util_empty(uniq_rv)]
    n_distinct_values <- length(uniq_rv)
    if (n_distinct_values < min_distinct_values) {
      # if we have fewer distinct values than required
      record_problem("Variable '%s' (%s) has fewer distinct values than required",
                     na.omit(v)[na.omit(v) %in% colnames(ds1)], arg_name,
                     applicability_problem = FALSE)
      empty_container[[v]] <- TRUE
    }
  }

  if (length(names(empty_container)) > 0) {
    variable <- setdiff(variable, names(empty_container))
    if (length(variable) == 0) {
      util_error(
        "In %s, none of the specified variables matches the requirements.",
        dQuote(arg_name),
        applicability_problem = FALSE
      )
    } else {
      util_warning(
        c("In %s, variables %s were excluded."),
        dQuote(arg_name),
        paste(dQuote(names(empty_container)), collapse = ", "),
        applicability_problem = FALSE
      )
      assign(arg_name, variable, envir = p)
    }
  }

  # data type given in metadata? (similar to util_get_meta_from_var ...)
  if (DATA_TYPE %in% colnames(meta_data)) {
    types <- meta_data[meta_data[[label_col]] %in% variable, DATA_TYPE]
      # try to fetch the types from the meta data.
    names(types) <- meta_data[meta_data[[label_col]] %in% variable, label_col]
    types <- types[!is.na(types) & !is.na(names(types))]
  } else {
    types <- NULL
    names(types) <- NULL
  }

  if (!all(is.na(need_type)) && length(types) <
        length(unique(variable[!is.na(variable)]))) {
    util_warning(
      c("In %s, variables with types matching %s should be specified, but not",
        "all variables have a type assigned in the meta data.",
        "I have %d variables but only %d types."),
      dQuote(arg_name),
      dQuote(need_type),
      length(unique(variable[!is.na(variable)])),
      length(types),
      applicability_problem = TRUE
    )
  }

  for (..vname in names(types)) {
    # for all variables that have a data type assigned in the meta data
    type <- types[[..vname]]
    # fetch the type of the currently checked variable
    if (!is.null(type) && !is.na(type) && !is.na(need_type)) {
      # if we have a needed type and a variable type for the currently checked
      # variable,
      need_type. <- tolower(trimws(unlist(strsplit(need_type, "|",
                                                   fixed = TRUE),
                                          recursive = TRUE)))
        # parse the type (split at pipes |)
      not_type <-
        tolower(trimws(substring(need_type.[startsWith(need_type., "!")], 2)))
        # and parse the type for negation (e.g. *not* INTEGER)
      well_type <-
        tolower(trimws(need_type.[!startsWith(need_type., "!")]))
        # and parse the type for no negation resp. (e.g. INTEGER)

      if ((length(well_type) > 0) &&
          (!all(well_type %in%
                tolower(trimws(names(DATA_TYPES)))))) {
        util_error(c(
          "Internal error:",
          "%s's %s contains invalid type names %s (allowed are %s).",
          "As a dataquieR developer, you should fix your call of %s."
          ),
         sQuote(arg_name),
         sQuote("need_type"),
         paste(dQuote(
           well_type[!(well_type %in%
             tolower(trimws(names(DATA_TYPES))))]
         ), collapse = ", "),
         paste(dQuote(tolower(trimws(names(DATA_TYPES)))), collapse = ", "),
         sQuote(sys.call()[[1]])
         )
      }

      if ((length(not_type) > 0) &&
         (!all(not_type %in%
               tolower(trimws(names(DATA_TYPES)))))) {
        util_error(c(
          "Internal error:",
          "%s's %s contains invalid !type names %s (allowed are %s).",
          "As a dataquieR developer, you should fix your call of %s."
        ),
        sQuote(arg_name),
        sQuote("need_type"),
        paste(dQuote(
          not_type[!(not_type %in%
                        tolower(trimws(names(DATA_TYPES))))]
        ), collapse = ", "),
        paste(dQuote(tolower(trimws(names(DATA_TYPES)))), collapse = ", "),
        sQuote(sys.call()[[1]])
        )
      }

      if ((length(well_type) > 0) && (!tolower(trimws(type)) %in% well_type)) {
          # now check for the allowed types
        util_error(
          "Argument %s: Variable '%s' (%s) does not have an allowed type (%s)",
          dQuote(arg_name), ..vname, tolower(trimws(type)), need_type,
          applicability_problem = TRUE)
      }
      if ((length(not_type) > 0) && (tolower(trimws(type)) %in% not_type)) {
        # and for the disallowed types
        util_error(
          "Argument %s: Variable '%s' (%s) does not have an allowed type (%s)",
          dQuote(arg_name), ..vname, tolower(trimws(type)), need_type,
          applicability_problem = TRUE)
      }
      # Check, if variable is really of declared data type.
      sd_type <- prep_datatype_from_data(..vname, ds1)
      if (sd_type != type) {
        if (sd_type != DATA_TYPES$INTEGER || type != DATA_TYPES$FLOAT) {
          util_warning(
            c("Argument %s: Variable '%s' (%s) does not have matching",
              "data type in the study data (%s)"),
            dQuote(arg_name), ..vname, tolower(trimws(type)),
            prep_dq_data_type_of(ds1[, ..vname, TRUE]),
            applicability_problem = TRUE)
        }
      }
    } else { # nocov start
      # if the type is NA, it is not part of the types vector
      # so this is dead code, likely (see
      # "types <- types[!is.na(types) & !is.na(names(types))]" above).
      if (!is.na(need_type)) {
        util_warning(
          c("Argument %s: No data type found for '%s' in the",
            "meta data, cannot check type"), dQuote(arg_name), variable,
          applicability_problem = TRUE)
      }
    } # nocov end
  }
}

#' Variable-argument roles
#'
#' A Variable-argument role is the intended use of an argument of a indicator
#' function -- an argument that refers variables.
#' In general for the table .variable_arg_roles, the suffix _var means one
#' variable allowed,
#' while _vars means more than one. The default  sets of arguments
#' for [util_correct_variable_use]/[util_correct_variable_use2] are defined
#' from the point of usage, e.g. if it could be, that NAs are in
#' the list of variable names, the function should be able to remove certain
#' response variables
#' from the output and not disallow them by setting `allow_na` to `FALSE`.
#'
#' @seealso [util_correct_variable_use()]
#' @seealso [util_correct_variable_use2()]
#'
.variable_arg_roles <-
  dplyr::tribble(
    ~name, ~allow_na, ~allow_more_than_one, ~allow_null, ~allow_all_obs_na,
    ~allow_any_obs_na, ~min_distinct_values, ~need_type,
    "resp_var", FALSE, FALSE, FALSE, FALSE, TRUE, 0, NA,
    "response_var", FALSE, FALSE, FALSE, FALSE, TRUE, 0, NA,
    "resp_vars", FALSE, TRUE, FALSE, FALSE, TRUE, 0, NA,
    "response_vars", FALSE, TRUE, FALSE, FALSE, TRUE, 0, NA,
    "control_vars", TRUE, TRUE, TRUE, FALSE, TRUE, 0, NA,
    "co_vars", TRUE, TRUE, TRUE, FALSE, TRUE, 0, NA,
    "cluster_vars", FALSE, TRUE, FALSE, FALSE, FALSE, 0, NA,
    "group_vars", FALSE, TRUE, FALSE, FALSE, FALSE, 0, NA,
    "level_vars", FALSE, TRUE, FALSE, FALSE, FALSE, 0, NA,
    "cluster_var", FALSE, FALSE, TRUE, FALSE, TRUE, 0, NA,
    "level_var", FALSE, FALSE, TRUE, FALSE, TRUE, 0, NA,
    "strata_vars", FALSE, FALSE, TRUE, FALSE, TRUE, 0, NA,
    "time_vars", FALSE, FALSE, FALSE, FALSE, FALSE, 0, NA,
    "id_vars", FALSE, FALSE, FALSE, FALSE, FALSE, 0, NA
  )
# "process_vars",
# "key_segment_vars"

#' @importFrom stats na.omit
#' @rdname util_correct_variable_use
util_correct_variable_use2 <- util_correct_variable_use
formals(util_correct_variable_use2)[["role"]] <- quote(arg_name)
