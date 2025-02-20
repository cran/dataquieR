#' Evaluate a parsed redcap rule for given study data
#'
#' also allows to use `VAR_NAMES` in the rules,
#' if other labels have been selected
#'
#' @param rule the redcap rule (parsed, already)
#' @param ds1 the study data as prepared by `prep_prepare_dataframes`
#' @param meta_data the metadata
#' @param use_value_labels map columns with `VALUE_LABELS` as factor variables
#' @param replace_limits [logical] replace hard limit violations by `NA`
#' @param replace_missing_by [enum] LABEL | INTERPRET | NA . Missing codes should
#'                                  be replaced by the missing labels, the
#'                                  `AAPOR` codes from the missing table or
#'                                  by `NA`. Can also be an empty string to keep
#'                                  the codes.
#'
#' @return the result of the parsed rule
#'
#' @family redcap
#' @concept process
#' @keywords internal
util_eval_rule <- function(rule, ds1, meta_data = "item_level",
                           use_value_labels,
                           replace_missing_by = "NA",
                           replace_limits = TRUE) { # IDEA: Use the new .apply_factor_metadata options, somehow
  if (replace_limits && replace_missing_by != "NA") {
    util_message(
      c("Cannot replace hard limits, if missing codes are not deleted. I will",
        "therefore replace the missing codes by NA, too."),
      applicability_problem = TRUE)
    replace_missing_by <- "NA"
  }
  use_missing_labels <- FALSE
  if (replace_missing_by %in% c("LABEL", "INTERPRET")) {
    use_missing_labels <- TRUE
    use_value_labels <- TRUE
  }
  if (missing(meta_data)) { # TODO: Have a default, now?!
    use_value_labels <- FALSE
    meta_data <- data.frame()
  }
  util_expect_data_frame(meta_data)

  if (replace_missing_by %in% c("LABEL", "INTERPRET")) {
    # missing label and AAPOR codes currently only supported for
    # categorical variables:
    vars_data_type <- setNames(nm = all.vars(rule),
                               util_find_var_by_meta(
                                 resp_vars = all.vars(rule),
                                 target = "DATA_TYPE",
                                 meta_data = meta_data))
    if (!VALUE_LABELS %in% colnames(meta_data)) {
      meta_data[[VALUE_LABELS]] <- NA_character_
    }
    if (!VALUE_LABEL_TABLE %in% colnames(meta_data)) {
      meta_data[[VALUE_LABEL_TABLE]] <- NA_character_
    }

    # TODO: Support:
    # if (!STANDARDIZED_VOCABULARY_TABLE %in% colnames(meta_data)) {
    #   meta_data[[STANDARDIZED_VOCABULARY_TABLE]] <- NA_character_
    # }
    vars_value_labels <- setNames(nm = all.vars(rule),
                                  util_find_var_by_meta(
                                    resp_vars = all.vars(rule),
                                    target = VALUE_LABELS,
                                    meta_data = meta_data))
    vars_value_label_tables <- setNames(nm = all.vars(rule),
                                  util_find_var_by_meta(
                                    resp_vars = all.vars(rule),
                                    target = VALUE_LABEL_TABLE,
                                    meta_data = meta_data))
    not_yet_supported <- intersect(names(vars_data_type)[which(vars_data_type %in% c("float", "integer"))],
                                   union(
                                     names(vars_value_labels)[which(util_empty(vars_value_labels))], # TODO: STANDARDIZED_VOCABULARY_TABLE
                                     names(vars_value_label_tables)[which(util_empty(vars_value_label_tables))]
                                   ))
    if (length(not_yet_supported) > 0) {
      util_error(paste0("Replacement of missing value codes by labels or ",
                        "AAPOR codes is not yet supported for numerical ",
                        "variables (",
                        paste0(not_yet_supported, collapse = ", "),
                        ")."),
                        applicability_problem = TRUE)
    }
  }
  if (!missing(ds1)) {
    problem <-
      isTRUE(attr(ds1, ("Codes_to_NA"))) &&
         replace_missing_by != "NA" # then, an unsuitable ds1 has been passed
    util_stop_if_not(!problem) # an unsuitable ds1 has been passed
    if (!!prod(dim(meta_data))) {
      prep_prepare_dataframes(.replace_hard_limits = replace_limits,
                              .replace_missings = (replace_missing_by == "NA"),
                              .study_data = ds1,
                              .label_col = attr(ds1, "label_col"))
    }
  }
  if (missing(use_value_labels)) {
    use_value_labels <- VALUE_LABELS %in% colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABELS]]))
    use_value_labels <- use_value_labels || (
      VALUE_LABEL_TABLE %in% colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABEL_TABLE]])))
    # TODO: STANDARDIZED_VOCABULARY_TABLE
  }
  if (missing(ds1)) {
    ds1 <- parent.frame()
  }
  if (is.environment(ds1)) {
    ds1 <- as.list(ds1)
  }
  if (!is.list(ds1)) {
    util_error("%s must be an environment or a list (or a dataframe)",
               dQuote("ds1"))
  }
  redcap_rule_env <- util_get_redcap_rule_env()
  label_col <- attr(ds1, "label_col")
  if (use_value_labels || use_missing_labels) {
    util_stop_if_not(attr(ds1, "MAPPED"))
    util_stop_if_not(!util_empty(label_col))
    if (!(VALUE_LABEL_TABLE %in% colnames(meta_data))) {
      meta_data[[VALUE_LABEL_TABLE]] <- NA_character_
    }
    if (!(VALUE_LABELS %in% colnames(meta_data))) {
      meta_data[[VALUE_LABELS]] <- NA_character_
    }
    cols_with_valuelabels <- meta_data[!util_empty(meta_data[[VALUE_LABELS]]) |
                                    !util_empty(meta_data[[VALUE_LABEL_TABLE]]), # TODO: address VALUE_LABELS = c("m|f", "") ...
                                       label_col, drop = TRUE] # TODO: STANDARDIZED_VOCABULARY_TABLE
    ds1_with_labels <- ds1
    if (!MISSING_LIST_TABLE %in% colnames(meta_data)) {
      meta_data[[MISSING_LIST_TABLE]] <- NA_character_
    }
    ds1_with_labels[, cols_with_valuelabels] <- lapply(cols_with_valuelabels,
      function(cn) {
        if (replace_missing_by == "INTERPRET") {
          mltab_name <- meta_data[
            meta_data[[label_col]] == cn,
            MISSING_LIST_TABLE,
            drop = TRUE]
          if (!is.na(mltab_name))
            mltab <-
              try(prep_get_data_frame(mltab_name), silent = TRUE)
          else
            mltab <- data.frame(
              CODE_INTERPRET = character(0),
              CODE_VALUE = character(0)
            )
          if (inherits(mltab, "try-error") ||
              !(CODE_INTERPRET %in% colnames(mltab)) ||
              !(CODE_VALUE %in% colnames(mltab))) {
            util_warning(
              c("For %s, I have no %s assignments from %s, so I will",
                "not replace missing codes."),
              dQuote(cn),
              sQuote(CODE_INTERPRET),
              sQuote(CODE_VALUE)
            )
            CODE_INTERPRET <- NULL;
          } else {
            mltab <-
              util_filter_missing_list_table_for_rv(mltab,
                                                    cn,
                                                    prep_map_labels( # TODO: Do the mapping outside the apply function loop
                                                      cn,
                                                      meta_data = meta_data,
                                                      to = VAR_NAMES,
                                                      from = label_col,
                                                      ifnotfound = cn,
                                                      warn_ambiguous = FALSE
                                                    ))
            CODE_INTERPRET <-
              setNames(mltab[[CODE_VALUE]], nm = mltab[[CODE_INTERPRET]])
          }
        }

        labs <- # TODO: discard, we have now VALUE_LABEL_TABLE, mostly always
          meta_data[
            meta_data[[label_col]] == cn,
            VALUE_LABELS,
            drop = TRUE]

        util_stop_if_not(
`Internal error, sorry, please report. VALUE_LABELS still in util_eval_rule()` =
          length(labs) == 0 ||
            all(util_empty(labs))
        )

        labtabs <- meta_data[
          meta_data[[label_col]] == cn,
          VALUE_LABEL_TABLE,
          drop = TRUE]

        labs <- vapply(labtabs, # FIXME: This is a slow and ugly work around.
                       function(vlt) {
                         vltdf <- try(prep_get_data_frame(vlt))
                         if (is.data.frame(vltdf)) {
                           if (CODE_VALUE %in% colnames(vltdf)) {
                             if (!CODE_LABEL %in% colnames(vltdf)) {
                               vltdf[[CODE_LABEL]] <- vltdf[[CODE_VALUE]]
                             }
                             return(prep_deparse_assignments(
                               mode = "string_codes",
                               codes = vltdf[[CODE_VALUE]],
                               labels = vltdf[[CODE_LABEL]]))
                           }
                         }
                         return(SPLIT_CHAR)
                       }, FUN.VALUE = character(1))


        if (!use_value_labels) {
          labs <- paste(unique(ds1[[cn]]), # TODO: This can definitely be bad in terms of performance
                        collapse = sprintf(" %s ", SPLIT_CHAR))
        }
        # if (all(util_empty(labs))) {
        #   labs <- paste(unique(ds1[[cn]]), # TODO: This can definitely be bad in terms of performance
        #                 collapse = sprintf(" %s ", SPLIT_CHAR))
        # }
        labs <- as.character(labs)
        ml <- util_parse_assignments(multi_variate_text = TRUE,
          meta_data[
            meta_data[[label_col]] == cn,
            MISSING_LIST,
            drop = TRUE])[[1]]
        ml <- ml[!util_empty(gsub(SPLIT_CHAR, "", ml))]
        mcodes <- suppressWarnings(trimws(names(ml)))
        jl <- util_parse_assignments(multi_variate_text = TRUE,
          meta_data[
            meta_data[[label_col]] == cn,
            JUMP_LIST,
            drop = TRUE])[[1]]
        jl <- jl[!util_empty(gsub(SPLIT_CHAR, "", jl))]
        jcodes <- suppressWarnings(trimws(names(jl)))

        if (replace_missing_by == "") { # keep the codes
          jl <-
            prep_deparse_assignments(codes = jcodes,
                                     labels = jcodes)
          ml <-
            prep_deparse_assignments(codes = mcodes,
                                     labels = mcodes)
        } else if (replace_missing_by == "LABEL") { # use the missing/jump labels
          jl <-
            prep_deparse_assignments(codes = jcodes,
                                     labels = jl)
          ml <-
            prep_deparse_assignments(codes = mcodes,
                                     labels = ml)
        } else if (replace_missing_by == "INTERPRET") { # use AAPOR codes
          if (!is.null(CODE_INTERPRET)) {
            jl <- ""
            ml <-
              prep_deparse_assignments(codes = CODE_INTERPRET,
                                       labels = names(CODE_INTERPRET))
          } else { # keep the codes
            util_warning("I do not have a %s with %s to use them for rules",
                         sQuote(MISSING_LIST_TABLE),
                         sQuote(CODE_INTERPRET))
            jl <-
              prep_deparse_assignments(codes = jcodes,
                                       labels = jcodes)
            ml <-
              prep_deparse_assignments(codes = mcodes,
                                       labels = mcodes)
          }
        } else {
          # "NA" is already handled before by prep_prepare_dataframes with .replace_missings
          jl <- ""
          ml <- ""
        }

        jl <- jl[!is.na(jl)]
        ml <- ml[!is.na(ml)]

        if (!any(grepl(SPLIT_CHAR, labs, fixed = TRUE)) &&
            (any(grepl("<", labs, fixed = TRUE)))) {
          labs <- gsub("<", SPLIT_CHAR, labs, fixed = TRUE)
        }

        labs <- paste(c(labs, jl, ml), collapse = sprintf(" %s ", SPLIT_CHAR))

        col_as_factor <-
          util_assign_levlabs(variable = ds1[[cn]],
                            string_of_levlabs = labs,
                            splitchar = SPLIT_CHAR,
                            assignchar = "=",
                            variable_name = cn,
                            warn_if_inadmissible = FALSE) # inadmissible categorical values are checked in an indicator, already
        if (!is.factor(col_as_factor)) {
          col_as_factor <- as.factor(col_as_factor)
        }
        col_as_factor
      }
    )
    ds2 <- ds1_with_labels
  } else {
    ds2 <- ds1
  }
  ds2_0 <- ds2
  if (length(label_col) == 1 && label_col != VAR_NAMES) {
    ds2_varnames <- ds2_0
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = VAR_NAMES,
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  if (length(label_col) == 1 && label_col != LABEL) {
    ds2_varnames <- ds2_0
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = LABEL,
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  if (LONG_LABEL %in% colnames(meta_data) &&
      length(label_col) == 1 && label_col != LONG_LABEL) {
    ds2_varnames <- ds2_0
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = LONG_LABEL,
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  if ("ORIGINAL_VAR_NAMES" %in% colnames(meta_data)) {
    ds2_varnames <- ds2_0
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = "ORIGINAL_VAR_NAMES",
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  if ("ORIGINAL_LABEL" %in% colnames(meta_data)) {
    ds2_varnames <- ds2_0
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = "ORIGINAL_LABEL",
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  # TODO: replace missing value codes only for
  # intersect(colnames(ds2), all.vars(rule))
  # to reduce the number of warning messages (i.e., do not throw a warning for
  # variables which are not used in the rule)
  res <- eval(expr = rule,
              envir = ds2,
              enclos = redcap_rule_env)
  if (is.character(res)) {
    res <- readr::parse_guess(res)
  }
  res
}
