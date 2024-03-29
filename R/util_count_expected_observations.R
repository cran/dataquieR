#' Count Expected Observations
#'
#' Count participants, if an observation was expected, given the
#' `PART_VARS` from item-level metadata
#'
#' @param resp_vars [character] the response variables, for that a value may be
#'                              expected
#' @param study_data [study_data]
#' @param meta_data [meta_data]
#' @param label_col [character] mapping attribute `colnames(study_data)` vs.
#'                              `meta_data[label_col]`
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. How should
#'                                     `PART_VARS` be handled:
#'                                     - `ALL`: Ignore, all observations are
#'                                       expected
#'                                     - `SEGMENT`: if `PART_VAR` is 1, an
#'                                       observation is expected
#'                                     - `HIERARCHY`: the default, if the
#'                                       `PART_VAR` is 1 for this variable and
#'                                       also for all `PART_VARS` of `PART_VARS`
#'                                       up in the hierarchy, an observation is
#'                                       expected.
#'
#' @return a vector with the number of expected observations for each
#'         `resp_vars`.
#'
#' @family missing_functions
#' @concept process
#' @keywords internal
util_count_expected_observations <- function(resp_vars, study_data, meta_data,
                                             label_col = LABEL,
                                             expected_observations =
                                               c("HIERARCHY",
                                                 "ALL",
                                                 "SEGMENT")) {
  vapply(resp_vars, function(rv) {
    sum(util_observation_expected(rv = rv,
                                  study_data = study_data,
                                  meta_data = meta_data,
                                  label_col = label_col,
                                  expected_observations =
                                    expected_observations),
        na.rm = TRUE)
  },
  FUN.VALUE = integer(1))
}
