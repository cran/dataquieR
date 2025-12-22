#' Insert missing codes for `NA`s based on rules
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param rules [data.frame] with the columns:
#'   - `VAR_NAMES`: [VAR_NAMES] of the variable to compute
#'   - `COMPUTATION_RULE`: A rule in `REDcap` style (see, e.g.,
#'   [`REDcap` help](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules),
#'   [`REDcap` how-to](https://docs.google.com/document/d/1l3nGBgqqPKi5PtMe75g7q0dny8QzGMd_/edit?tab=t.0)), and
#'   [`REDcap` branching logic](https://www.iths.org/wp-content/uploads/REDCap-Branching-Logic-2017-202.pdf)
#'   that defines, how to compute the new values
#' @param use_value_labels [logical] In rules for factors, use the value labels,
#'                                   not the codes. Defaults to `TRUE`, if any
#'                                   `VALUE_LABELS` are given in the metadata.
#'
#' @return a `list` with the entry:
#'   - `ModifiedStudyData`: Study data with the new variables
#'
#' @export
#'
#' @examples
#' \dontrun{
#' study_data <- prep_get_data_frame("ship")
#' prep_load_workbook_like_file("ship_meta_v2")
#' meta_data <- prep_get_data_frame("item_level")
#' rules <- tibble::tribble(
#'   ~VAR_NAMES,  ~RULE,
#'   "BMI", '[BODY_WEIGHT_0]/(([BODY_HEIGHT_0]/100)^2)',
#'   "R", '[WAIST_CIRC_0]/2/[pi]', # in m^3
#'   "VOL_EST", '[pi]*([WAIST_CIRC_0]/2/[pi])^2*[BODY_HEIGHT_0] / 1000', # in l
#'  )
#'  r <- prep_add_computed_variables(study_data, meta_data,
#'    label_col = "LABEL", rules, use_value_labels = FALSE)
#' }
prep_add_computed_variables <- function(
                                   study_data,
                                   meta_data,
                                   label_col,
                                   rules,
                                   use_value_labels) {

  prep_prepare_dataframes(.replace_missings = FALSE,
                          .replace_hard_limits = FALSE,
                          .adjust_data_type = FALSE,
                          .amend_scale_level = FALSE)

  if (!is.data.frame(rules) || !nrow(rules)) {
    return(list(ModifiedStudyData = ds1))
  }

  util_expect_data_frame(rules, list(VAR_NAMES = function(x) {
    !any(x %in% colnames(study_data))
  },
  COMPUTATION_RULE = is.character))
  if (DATA_PREPARATION %in% colnames(rules)) {
    rules[[DATA_PREPARATION]] <-
      vapply(lapply(lapply(lapply(lapply(
        util_parse_assignments(rules[[DATA_PREPARATION]],
                               multi_variate_text = TRUE),
        toupper),
        trimws),
        sort),
        unique), FUN =
          prep_deparse_assignments, mode = "string_codes",
        FUN.VALUE = character(1))
    rules_split <- split(rules, rules[[DATA_PREPARATION]])
    res <- ds1
    for (my_rules in names(rules_split)) {
      to_apply <- util_parse_assignments(my_rules)
      curr_rules <- rules_split[[my_rules]]
      curr_rules[[DATA_PREPARATION]] <- NULL
      replace_missing_by <- ""
      if (sum("MISSING_INTERPRET" %in% to_apply,
              "MISSING_LABEL" %in% to_apply,
              "MISSING_NA" %in% to_apply) > 1) {
        util_warning("Invalid %s in computation rules. Falling back to %s",
                     sQuote(DATA_PREPARATION),
                     dQuote("MISSING_NA"))
        to_apply <- to_apply[!startsWith(to_apply, "MISSING_")]
        to_apply <- c(to_apply, "MISSING_NA")
      }
      if ("MISSING_NA" %in% to_apply) {
        replace_missing_by <- "NA"
      }
      if ("MISSING_INTERPRET" %in% to_apply) {
        replace_missing_by <- "INTERPRET"
      }
      if ("MISSING_LABEL" %in% to_apply) {
        replace_missing_by <- "LABEL"
      }
      res <- .util_add_computed_variables(ds1 = res,
                                          meta_data = meta_data,
                                          label_col = label_col,
                                          rules = curr_rules,
                                          use_value_labels =
                                            ("LABEL" %in% to_apply),
                                          replace_missing_by =
                                            replace_missing_by,
                                          replace_limits =
                                            ("LIMITS" %in% to_apply)
                                          )$ModifiedStudyData
    }
    return(list(ModifiedStudyData = res))
  } else {
    return(.util_add_computed_variables(ds1 = ds1,
                                        meta_data = meta_data,
                                        label_col = label_col,
                                        rules = rules,
                                        use_value_labels =
                                          use_value_labels))
  }

}

.util_add_computed_variables <- function(ds1,
                                         meta_data,
                                         label_col,
                                         rules,
                                         use_value_labels,
                                         replace_missing_by = "NA",
                                         replace_limits = TRUE) {

  if (missing(use_value_labels)) {
    use_value_labels <- VALUE_LABELS %in% colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABELS]]))
  }

  util_expect_scalar(use_value_labels, check_type = is.logical)

  compiled_rules <- lapply(setNames(nm = rules[[VAR_NAMES]],
                                    rules[[COMPUTATION_RULE]]),
                           util_parse_redcap_rule)

  rule_res <- lapply(compiled_rules, function(rule) {
    r <- try(util_eval_rule(rule = rule,
                   ds1 = ds1,
                   meta_data = meta_data,
                   use_value_labels = use_value_labels,
                   replace_missing_by = replace_missing_by,
                   replace_limits = replace_limits))
    if (util_is_try_error(r)) {
      rl <- "?"
      try(rl <- attr(rule, "src"), silent = TRUE)
      if (length(rl) != 1)
        rl <- "?"
      util_warning("Could not evaluate rule %s: %s, results are all NA",
                   sQuote(rl),
                   dQuote(conditionMessage(attr(r, "condition"))),
                   applicability_problem =
                     TRUE)
      NA
    } else {
      r
    }
  })

  ModifiedStudyData <- ds1

  ModifiedStudyData[, names(rule_res)] <- rule_res

  return(list(ModifiedStudyData = ModifiedStudyData))
}
