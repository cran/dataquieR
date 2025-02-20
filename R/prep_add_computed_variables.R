#' Insert missing codes for `NA`s based on rules
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param rules [data.frame] with the columns:
#'   - `VAR_NAMES`: [VAR_NAMES] of the variable to compute
#'   - `RULE`: A rule in `REDcap` style (see, e.g.,
#'   [`REDcap` help](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules),
#'   [`REDcap` how-to](https://www.ctsi.ufl.edu/files/2017/06/Calculated-Fields-%E2%80%93-REDCap-How.pdf)), and
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

  if (missing(use_value_labels)) {
    use_value_labels <- VALUE_LABELS %in% colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABELS]]))
  }

  util_expect_scalar(use_value_labels, check_type = is.logical)

  util_expect_data_frame(rules, list("VAR_NAMES" = function(x) {
                                        !any(x %in% colnames(study_data))
                                      },
                                     "COMPUTATION_RULE" = is.character))

  compiled_rules <- lapply(setNames(nm = rules$VAR_NAMES,
                                    rules$COMPUTATION_RULE),
                           util_parse_redcap_rule)

  rule_res <- lapply(compiled_rules, function(rule) {
    util_eval_rule(rule = rule,
                   ds1 = ds1,
                   meta_data = meta_data,
                   use_value_labels = use_value_labels)
  })

  ModifiedStudyData <- ds1

  ModifiedStudyData[, names(rule_res)] <- rule_res

  return(list(ModifiedStudyData = ModifiedStudyData))

}
