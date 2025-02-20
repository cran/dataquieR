#' Get formats for `DQ` categories
#'
#' @family summary_functions
#' @keywords internal
#'
#' @return [data.frame] columns: `categories` (e.g., "1" to "5"),
#'   `color` (e.g., "33 102 172", "67 147 195", "227 186 20", "214 96 77",
#'   178 23 43"), `label` (e.g., "OK", "unclear", "moderate", "important",
#'    "critical" )
util_get_ruleset_formats <- function() {

  rs <- util_get_rule_sets()

  if(!"dqi_catnum" %in% colnames(rs)) {
    max_cats <- 5
  } else {
    max_cats <- suppressWarnings(
      vapply(lapply(lapply(rs, `[[`, "dqi_catnum"), as.integer), max,
             na.rm = TRUE, FUN.VALUE = integer(1)))
  }

  shipped_ruleset_formats <- system.file("grading_formats.xlsx",
                                  package = "dataquieR")
  if (!nzchar(shipped_ruleset_formats) &&
      suppressWarnings(util_ensure_suggested("pkgload", err = FALSE)) &&
      pkgload::is_dev_package("dataquieR")) {
    if (util_is_try_error(try(silent = TRUE,
      shipped_ruleset_formats <- pkgload::package_file("inst",
                                                    "grading_formats.xlsx")))) {
      rlang::warn(sprintf(
        "Could not find package source, trying to use %s from installed package",
        sQuote("grading_formats.xlsx")
      ),
      .frequency_id =
        "pkgload_confusionfmts",
      .frequency = "once")
      shipped_ruleset_formats <-
        names(head(which(vapply(
          setNames(nm = file.path(.libPaths(), "dataquieR",
                                  "grading_formats.xlsx")), file.exists,
          FUN.VALUE = logical(1))), 1))
      if (length(shipped_ruleset_formats) != 1) {
        shipped_ruleset_formats <- ""
      }
    }
  }
  if (!nzchar(shipped_ruleset_formats)) {
    util_error(
      "Internal error with pkgload, please report, sorry: Could not find %s.",
      sQuote("shipped_ruleset_formats"))
  }
  shipped_ruleset_formats <- prep_get_data_frame(shipped_ruleset_formats)

  reftab <- try(
    prep_get_data_frame(getOption("dataquieR.grading_formats", dataquieR.grading_formats_default)),
    silent = TRUE
  ) # https://gitlab.com/libreumg/dataquier/-/issues/34#note_1597720844
  if (!inherits(reftab, "data.frame")) {
    if (getOption("dataquieR.grading_formats", dataquieR.grading_formats_default) != dataquieR.grading_formats_default) {
      util_message("Could not find ruleset formats %s, using the default formats.",
                   dQuote(getOption("dataquieR.grading_formats",
                                    dataquieR.grading_formats_default)),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = FALSE)
    }
    reftab <- shipped_ruleset_formats
  }

  util_expect_data_frame(reftab, col_names = c("category", "label", "color"))

  need <- seq_len(max_cats)
  have <- unique(reftab$category)

  if (!all(need %in% have)) {
    util_error(
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE,
      c("Did not find formats for all categories, need %s, found %s.",
        "%s may not match %s or one of these is not available."
      ),
      util_pretty_vector_string(need),
      util_pretty_vector_string(have),
      dQuote(getOption("dataquieR.grading_formats",
                       dataquieR.grading_formats_default)),
      dQuote(getOption("dataquieR.grading_rulesets",
                       dataquieR.grading_rulesets_default))
    )
  }

  reftab <- reftab[!util_empty(reftab$category), , FALSE]
  reftab[["category"]] <-
    suppressWarnings(as.integer(reftab[["category"]]))

  reftab
}
