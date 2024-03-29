#' Detect Expected Observations
#'
#' For each participant, check, if an observation was expected, given the
#' `PART_VARS` from item-level metadata
#'
#' @param rv [character] the response variable, for that a value may be expected
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
#' @return a vector with `TRUE` or `FALSE` for each row of `study_data`, if for
#'         `study_data[rv]` a value is expected.
#'
#' @family missing_functions
#' @concept missing
#' @keywords internal
util_observation_expected <- # TODO: Support segment level metadata links to SEGMENT_PART_VARS
  function(rv, study_data, meta_data, label_col = LABEL,
           expected_observations =
             c("HIERARCHY",
               "ALL",
               "SEGMENT")) { # TODO: make prep

    util_expect_scalar(expected_observations, allow_more_than_one = TRUE)
    expected_observations <- match.arg(expected_observations)
    util_expect_scalar(expected_observations)

    all_need_to_be_1 <- # and the order is from root to leaf in the PART_VAR
                        # hierarchy
      util_all_intro_vars_for_rv(rv, study_data, meta_data, label_col,
                                 expected_observations = expected_observations)

    sd <- study_data
    missing_vars <- setdiff(all_need_to_be_1, colnames(study_data))
    if (any(missing_vars)) {
      util_warning(c("Missing %s from %s, I fill it with NA.",
                     "This may cause inconsistencies, if below in the",
                     "hierarchy, something is expected"),
                   paste0(dQuote(missing_vars), collapse = ", "),
                   sQuote("meta_data"))
      sd[, missing_vars] <- NA
    }
    sd <- sd[, all_need_to_be_1, drop = FALSE]

    sd[] <- !sapply(sd, util_is_na_0_empty_or_false)
    return(
      rowSums(sd, na.rm = TRUE) ==
        length(all_need_to_be_1)
    )
  }
