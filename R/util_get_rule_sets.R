#' Get rule sets for `DQ` grading
#'
#' @return names lists, names are the ruleset names, values are [data.frame]s
#'         featuring the columns `GRADING_RULESET`, `dqi_parameterstub`,
#'         `indicator_metric`, `dqi_catnum` and `dqi_cat_1` to
#'         `dqi_cat_<dqi_catnum>`
#' @family summary_functions
#' @noRd
util_get_rule_sets <- function() {
  shipped_rulesets <- system.file("grading_rulesets.xlsx",
                                  package = "dataquieR")
  if (!nzchar(shipped_rulesets) &&
      suppressWarnings(util_ensure_suggested("pkgload", err = FALSE)) &&
      pkgload::is_dev_package("dataquieR")) {
        if (util_is_try_error(# comes from a quosure featuring an older dq version.
          try(silent = TRUE, shipped_rulesets <- pkgload::package_file("inst",
                                                  "grading_rulesets.xlsx",
                                                  path =
                                                  find.package(
                                                    "dataquieR"))))) {
          rlang::warn(sprintf(
            "Could not find package source, trying to use %s from installed package",
            sQuote("grading_rulesets.xlsx")
          ),
                      .frequency_id =
                        "pkgload_confusion",
                      .frequency = "once")

          shipped_rulesets <-
            names(head(which(vapply(
              setNames(nm = file.path(.libPaths(), "dataquieR",
                                      "grading_rulesets.xlsx")), file.exists,
              FUN.VALUE = logical(1))), 1))
          if (length(shipped_rulesets) != 1) {
            shipped_rulesets <- ""
          }
        }
  }
  if (!nzchar(shipped_rulesets)) {
    util_error(
      "Internal error with pkgload, please report, sorry: Could not find %s.",
      sQuote("shipped_rulesets"))
  }
  shipped_rulesets <- prep_get_data_frame(shipped_rulesets)

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

  for (needed in setdiff(c("CAT_applicability", "CAT_error",
                   "CAT_anamat", "CAT_indicator_or_descriptor"),
                   unique(default_rs$indicator_metric))) {
    add_row <- shipped_rulesets[!is.na(shipped_rulesets$GRADING_RULESET) &
                       shipped_rulesets$GRADING_RULESET == "0" &
                       !is.na(shipped_rulesets$indicator_metric) &
                       shipped_rulesets$indicator_metric == needed, , FALSE]
    default_rs <- util_rbind(default_rs,
                              add_row)
  }

  GRADING_RULESETs[["0"]] <- default_rs

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
