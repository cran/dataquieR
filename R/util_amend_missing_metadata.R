#' Amend missing item-level metadata, only
#'
#' @inheritParams .template_function_indicator
#' @inheritParams prep_study2meta
#' @returns [data.frame] fixed `meta_data`
#' @noRd
util_amend_missing_metadata <- function(study_data,
                                        meta_data,
                                        level = c(
                                          VARATT_REQUIRE_LEVELS$REQUIRED,
                                          VARATT_REQUIRE_LEVELS$RECOMMENDED
                                        ),
                                        cumulative = TRUE,
                                        guess_missing_codes =
                                          getOption("dataquieR.guess_missing_codes",
                                                    dataquieR.guess_missing_codes_default)) {
  to_amend <- !(colnames(study_data) %in% meta_data[[VAR_NAMES]])
  if (any(to_amend)) {
    util_message(
      "Missing %s from %s, amending guessed %s for these items/variables...",
      util_pretty_vector_string(colnames(study_data)[to_amend]),
      sQuote("meta_data"),
      "item-level metadata",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )
    am_md <- prep_study2meta(
      study_data =
        study_data[, colnames(study_data)[to_amend], FALSE],
      level = level,
      cumulative = cumulative,
      convert_factors = FALSE,
      guess_missing_codes = guess_missing_codes
    )
    meta_data <- util_rbind(meta_data, am_md)
  }
  return(meta_data)
}
