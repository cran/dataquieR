#' Get all `PART_VARS` for a response variable (from item-level metadata)
#'
#' @param rv [character] the response variable's name
#' @param study_data [study_data]
#' @param meta_data [meta_data]
#' @param label_col [character] the metadata attribute to map `meta_data` on
#'                              `study_data` based on `colnames(study_data)`
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
#' @return [character] all `PART_VARS` for `rv` from item level metadata.
#'    For `expected_observations = HIERARCHY`, the more general `PART_VARS`
#'    (i.e., up, in the hierarchy) are more left in the vector, e.g.:
#'    `PART_STUDY, PART_PHYSICAL_EXAMINATIONS, PART_BLOODPRESSURE`
util_all_intro_vars_for_rv <- function(rv, study_data, meta_data,
                                       label_col = LABEL,
                                       expected_observations =
                                         c("HIERARCHY",
                                           "ALL",
                                           "SEGMENT")) {
  r <- .util_all_intro_vars_for_rv(rv, study_data, meta_data, label_col,
                                   expected_observations)
  util_map_labels(r, meta_data, to = label_col, ifnotfound = r)

}

.util_all_intro_vars_for_rv <-
  function(rv, study_data, meta_data, label_col = LABEL,
           expected_observations =
             c("HIERARCHY",
               "ALL",
               "SEGMENT")) {

    meta_data <- prep_meta_data_v1_to_item_level_meta_data(meta_data,
                                                           verbose = FALSE,
                                                           label_col =
                                                             label_col)

    util_expect_scalar(expected_observations, allow_more_than_one = TRUE,
                       check_type = is.character)
    expected_observations <- match.arg(expected_observations)
    util_expect_scalar(expected_observations)

    if (expected_observations == "ALL" ||
        !(PART_VAR %in% names(meta_data))) {
      return(NULL)
    }
    if (label_col != VAR_NAMES) {
      rv <-
        prep_map_labels(
          rv,
          from = label_col,
          to = VAR_NAMES,
          meta_data =  meta_data
        )
    }
    kss <-
      prep_map_labels(
        rv,
        from = VAR_NAMES,
        to = PART_VAR,
        meta_data =  meta_data
      )
    rv <- unname(rv)
    kss <- unname(kss)
    if (identical(rv, kss)) {
      return(NULL)
    }
    if (expected_observations == "HIERARCHY" &&
        kss %in% meta_data$VAR_NAMES) {
      return(c(Recall(kss, study_data, meta_data, VAR_NAMES), kss))
    }
    if (expected_observations != "ALL") {
      var_kss <- kss %in% meta_data[[VAR_NAMES]]
      if (any(!var_kss)) {
        util_warning(c("For %s = %s, %s in %s must contain names of",
                       "study variables inidcating participation in a",
                       "study segment. %s is/are not in %s in %s."),
                     sQuote("expected_observations"),
                     dQuote(expected_observations),
                     dQuote(PART_VAR),
                     sQuote("meta_data"),
                     paste(dQuote(kss[!var_kss]), collapse = ", "),
                     dQuote(VAR_NAMES),
                     sQuote("meta_data")
                     )
      }
    }
    intersect(kss, meta_data[[VAR_NAMES]])
  }
