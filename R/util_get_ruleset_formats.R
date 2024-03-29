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
  max_cats <- suppressWarnings(
    vapply(lapply(lapply(rs, `[[`, "dqi_catnum"), as.integer), max,
           na.rm = TRUE, FUN.VALUE = integer(1)))

  shipped_ruleset_formats <-
    prep_get_data_frame(system.file("grading_formats.xlsx",
                                    package = "dataquieR"))

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
