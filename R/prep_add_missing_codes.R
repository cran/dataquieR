#' Insert missing codes for `NA`s based on rules
#'
#' @param resp_vars [variable list] the name of the measurement variables to be
#'   modified, all from `rules`, if omitted
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param rules [data.frame] with the columns:
#'   - `resp_vars`: Variable, whose `NA`-values should be replaced by jump codes
#'   - `CODE_CLASS`: Either `MISSING` or `JUMP`: Is the currently described case
#'                   an expected missing value (`JUMP`) or not (`MISSING`)
#'   - `CODE_VALUE`: The jump code or missing code
#'   - `CODE_LABEL`: A label describing the reason for the missing value
#'   - `RULE`: A rule in `REDcap` style (see, e.g.,
#'   [`REDcap` help](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules),
#'   [`REDcap` how-to](https://www.ctsi.ufl.edu/files/2017/06/Calculated-Fields-%E2%80%93-REDCap-How.pdf)), and
#'   [`REDcap` branching logic](https://www.iths.org/wp-content/uploads/REDCap-Branching-Logic-2017-202.pdf)
#'   that describes cases for the missing
#' @param overwrite [logical] Also insert missing codes, if the values are not
#'                            `NA`
#' @param use_value_labels [logical] In rules for factors, use the value labels,
#'                                   not the codes. Defaults to `TRUE`, if any
#'                                   `VALUE_LABELS` are given in the metadata.
#'
#' @return a `list` with the entries:
#'   - `ModifiedStudyData`: Study data with `NA`s replaced by the `CODE_VALUE`
#'   - `ModifiedMetaData`: Metadata having the new codes amended in the columns
#'                         `JUMP_LIST` or `MISSING_LIST`, respectively
#' @export
#'
#' @examples
#' \dontrun{
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' vn <- subset(r$ModifiedMetaData, LABEL == "PREGNANT_0", VAR_NAMES)[[1]]
#' rules <- tibble::tribble(
#'   ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
#'   "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[SEX_0]=1',
#'  )
#'  r <- prep_add_missing_codes(NA, study_data, meta_data,
#'    label_col = "LABEL", rules, use_value_labels = FALSE)
#'  subset(r$ModifiedMetaData, LABEL == "PREGNANT_0", JUMP_LIST)
#'  subset(meta_data, LABEL == "PREGNANT_0", JUMP_LIST)
#'  table(study_data[[vn]])
#'  table(r$ModifiedStudyData[[vn]])
#'  r <- prep_add_missing_codes(NA, study_data, meta_data,
#'    label_col = "LABEL", rules, use_value_labels = FALSE, overwrite = TRUE)
#'  table(study_data[[vn]])
#'  table(r$ModifiedStudyData[[vn]])
#'
#' rules <- tibble::tribble(
#'   ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
#'   "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[SEX_0]="males"',
#'  )
#'  r <- prep_add_missing_codes(NA, study_data, meta_data,
#'    label_col = "LABEL", rules, use_value_labels = TRUE, overwrite = FALSE)
#'  table(study_data[[vn]])
#'  table(r$ModifiedStudyData[[vn]])
#'
#' rules <- tibble::tribble(
#'   ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
#'   "PREGNANT_0", "JUMP", "No pregs in males", "9999", '[v00002]="males"',
#'  )
#'  r <- prep_add_missing_codes(NA, study_data, meta_data,
#'    label_col = "LABEL", rules, use_value_labels = TRUE, overwrite = FALSE)
#'  table(study_data[[vn]])
#'  table(r$ModifiedStudyData[[vn]])
#'  devtools::load_all(".")
#'
#' study_data$v00002 <- ifelse(study_data$v00002 == "0", "females", "males")
#' meta_data[meta_data$LABEL == "SEX_0", "VALUE_LABELS"] <- "females|males"
#' rules <- tibble::tribble(
#'   ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
#'   "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[v00002]="males"',
#' )
#' r <- prep_add_missing_codes(NA, study_data, meta_data,
#'                             label_col = "LABEL", rules, use_value_labels = TRUE, overwrite = FALSE)
#' table(study_data[[vn]])
#' table(r$ModifiedStudyData[[vn]])
#' }
prep_add_missing_codes <- function(resp_vars,
                                   study_data,
                                   meta_data,
                                   label_col,
                                   rules,
                                   use_value_labels,
                                   overwrite = FALSE) {

  util_expect_scalar(overwrite, check_type = is.logical)

  prep_prepare_dataframes(.replace_missings = FALSE,
                          .replace_hard_limits = FALSE,
                          .adjust_data_type = FALSE,
                          .amend_scale_level = FALSE)

  if (missing(use_value_labels)) {
    use_value_labels <- VALUE_LABELS %in% colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABELS]]))
  }

  util_expect_scalar(use_value_labels, check_type = is.logical)

  util_expect_data_frame(rules, list("resp_vars" = function(x) {
                                        all(x %in% colnames(ds1))
                                      },
                                     "CODE_CLASS" = function(x) {
                                       all(x %in% c("MISSING", "JUMP"))
                                     },
                                     "CODE_LABEL" = is.character,
                                     "CODE_VALUE" = util_is_valid_missing_codes,
                                     "RULE" = is.character))

  if (missing(resp_vars) || identical(resp_vars, NA)) {
    resp_vars <- rules$resp_vars
  }

  util_correct_variable_use(resp_vars, allow_more_than_one = TRUE)

  rules <- rules[rules[["resp_vars"]] %in% resp_vars]

  rules$CODE_VALUE <- util_as_valid_missing_codes(rules$CODE_VALUE)

  compiled_rules <- lapply(setNames(nm = rules$RULE), util_parse_redcap_rule)

  rule_match <- lapply(compiled_rules, function(rule) {
    util_eval_rule(rule = rule,
                   ds1 = ds1,
                   meta_data = meta_data,
                   use_value_labels = use_value_labels)
  })

  na <- lapply(setNames(rules$resp_vars, nm = rules$RULE), function(rule) {
    r <- util_empty(ds1[[rules$resp_vars]])
    if (overwrite) {
      r[] <- TRUE
    }
    r
  })

  util_stop_if_not(length(rule_match) == length(na))

  add_miss <- lapply(setNames(nm = rules$RULE),
                     function(rl) {
                       util_stop_if_not(length(na[[rl]]) == length(rule_match[[rl]]))
                       na[[rl]] & rule_match[[rl]]
                      })

  workload <- as.data.frame(add_miss,
                            stringsAsFactors = FALSE,
                            check.names = FALSE)

  target_variable <- function(nm) {
    rules[rules$RULE == nm, "resp_vars", TRUE]
  }

  target_code <- function(nm) {
    rules[rules$RULE == nm, "CODE_VALUE", TRUE]
  }

  for (rulenm in rules$RULE) {
    ds1[[target_variable(rulenm)]][workload[[rulenm]]] <-
      target_code(rulenm)
  }

  cause_label_df_list <- prep_extract_cause_label_df(meta_data = meta_data,
                                                     label_col = label_col)

  meta_data <- cause_label_df_list$meta_data
  cause_label_df <- cause_label_df_list$cause_label_df

  cause_label_df <- rbind(cause_label_df,
                          rules[, colnames(cause_label_df)])

  meta_data <-
    prep_add_cause_label_df(meta_data = meta_data,
                            cause_label_df = cause_label_df,
                            label_col = label_col)

  ModifiedStudyData <- ds1
  colnames(ModifiedStudyData) <-
    prep_map_labels( colnames(ds1),
                     to = VAR_NAMES,
                     from = label_col,
                     meta_data = meta_data)

  ModifiedMetaData <- meta_data

  return(list(ModifiedStudyData = ModifiedStudyData,
              ModifiedMetaData = ModifiedMetaData))

}
