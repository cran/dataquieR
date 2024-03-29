#' Get the thresholds for grading
#'
#' @param indicator_metric which indicator metric to be classified
#' @param meta_data the item level metadata
#'
#' @return named list (names are `VAR_NAMES`, values are named vectors of
#'         intervals, names in the vectors are the category numbers)
#' @family summary_functions
#' @keywords internal
util_get_thresholds <- function(indicator_metric, meta_data) {

  vars <- meta_data[[VAR_NAMES]]

  if (!(GRADING_RULESET %in% colnames(meta_data))) {
    meta_data[[GRADING_RULESET]] <- 0
  }
  meta_data[
    util_empty(meta_data[[GRADING_RULESET]]),
    GRADING_RULESET] <-
    0

  rsts <- util_get_rule_sets()
  availab_rulesets <- meta_data[[GRADING_RULESET]] %in% names(rsts)
  if (any(!availab_rulesets)) {
    util_warning(c("The following rulesets are used by item-level metadata",
                   "but missing from the ruleset file: %s. Falling back to",
                   "default (0) for them"),
                 util_pretty_vector_string(
                   unique(meta_data[[GRADING_RULESET]][!availab_rulesets])))
    meta_data[[GRADING_RULESET]][!availab_rulesets] <- "0"
  }
  trs <- lapply(setNames(nm = vars), function(vn) {
    rs <-
      as.character(
        meta_data[meta_data[[VAR_NAMES]] == vn, GRADING_RULESET, TRUE]
      )
    rst <- rsts[[rs]][rsts[[rs]]$indicator_metric == indicator_metric, , FALSE]
    if (nrow(rst) > 1) {
      util_message("More than one ruleset for %s / %s, using the first one",
                   dQuote(vn),
                   dQuote(indicator_metric),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = FALSE)
      rst <- head(rst, 1)
    } else if (nrow(rst) == 0) {
      return(setNames(nm = character(0)))
    }
    cats <- seq_len(as.integer(rst[["dqi_catnum"]]))
    cat_names <- as.character(cats)
    breaks <- paste0("dqi_cat_", cats)
    breaks <- unlist(rst[, breaks])
    setNames(breaks, nm = cat_names)
  })

  trs
}
