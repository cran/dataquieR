#' Get rule sets for `DQ` grading
#'
#' @return names lists, names are the ruleset names, values are [data.frame]s
#'         featuring the columns `GRADING_RULESET`, `dqi_parameterstub`,
#'         `indicator_metric`, `dqi_catnum` and `dqi_cat_1` to
#'         `dqi_cat_<dqi_catnum>`
#' @family summary_functions
#' @keywords internal
util_get_rule_sets <- function() {
  shipped_rulesets <-
    prep_get_data_frame(system.file("grading_rulesets.xlsx",
                                    package = "dataquieR"))

  reftab <- try(
    prep_get_data_frame(getOption("dataquieR.grading_rulesets",
                                  dataquieR.grading_rulesets_default)),
    silent = TRUE
  ) # https://gitlab.com/libreumg/dataquier/-/issues/34#note_1597720844
  if (!inherits(reftab, "data.frame")) {
    if (getOption("dataquieR.grading_rulesets",
                  dataquieR.grading_rulesets_default) !=
        dataquieR.grading_rulesets_default)
      util_message("Could not find rulesets %s, using the default rulesets.",
                   dQuote(getOption("dataquieR.grading_rulesets",
                                    dataquieR.grading_rulesets_default)),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = FALSE)
    reftab <- shipped_rulesets
  }
  util_expect_data_frame(reftab, col_names = c("indicator_metric", GRADING_RULESET))
  reftab <- reftab[!util_empty(reftab$indicator_metric), , FALSE]
  reftab <- reftab[!util_empty(reftab[[GRADING_RULESET]]), , FALSE]
  reftab$GRADING_RULESET <- suppressWarnings(trimws(reftab[[GRADING_RULESET]]))
  GRADING_RULESET_names <- setNames(nm = setdiff(unique(reftab[[GRADING_RULESET]]), NA))

  if (!("0" %in% GRADING_RULESET_names)) {
    util_error("No default GRADING_RULESET found in grading_rule",
               applicability_problem = TRUE)
  }

  GRADING_RULESETs <- lapply(GRADING_RULESET_names, function(rs) {
    reftab[!is.na(reftab$GRADING_RULESET) & reftab$GRADING_RULESET == rs, , FALSE]
  })

  default_rs <- GRADING_RULESETs[["0"]]

  to_amend <- setNames(nm = setdiff(GRADING_RULESET_names, "0"))

  GRADING_RULESETs[as.character(to_amend)] <- lapply(to_amend, function(rs) {
    rst <- GRADING_RULESETs[[as.character(rs)]]
    dst <- default_rs
    dst <- dst[!(dst$indicator_metric %in% rst$indicator_metric), , # only deafult rules, if not overwritte in current
               FALSE]
    dst$GRADING_RULESET <- rep(rs, nrow(dst))
    util_rbind(
      dst,
      rst
    )
  })

  GRADING_RULESETs
}
