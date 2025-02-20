test_that("prep_check_meta_names works", {
  skip_on_cran()
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                  dataquieR.ERRORS_WITH_CALLER = TRUE,
                  dataquieR.WARNINGS_WITH_CALLER = TRUE,
                  dataquieR.MESSAGES_WITH_CALLER = TRUE)
 expect_silent(prep_check_meta_names(data.frame(), level = NULL))

 expect_silent( prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
                        MISSING_LIST = 3)))

  expect_silent(prep_check_meta_names(
    data.frame(
      VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
      SCALE_LEVEL = NA_character_,
      UNIT = NA_character_,
      LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
      JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
      GROUP_VAR_OBSERVER = "GROUP_VAR_OBSERVER",
      GROUP_VAR_DEVICE = "GROUP_VAR_DEVICE",
      TIME_VAR = "TIME_VAR",
      STUDY_SEGMENT = "STUDY_SEGMENT",
      PART_VAR = "PART_VAR",
      LOCATION_RANGE = "LOCATION_RANGE",
      LOCATION_METRIC = "LOCATION_METRIC",
      PROPORTION_RANGE = "PROPORTION_RANGE",
      MISSING_LIST_TABLE = "MISSING_LIST_TABLE",
      LONG_LABEL = "LONG_LABEL",
      CO_VARS = "CO_VARS"
    ),
    RECOMMENDED
  ))

  expect_silent(prep_check_meta_names(
    data.frame(
      VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
      SCALE_LEVEL = NA_character_,
      UNIT = NA_character_,
      LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
      VALUE_LABEL_TABLE = "VALUE_LABEL_TABLE",
      STANDARDIZED_VOCABULARY_TABLE = "STANDARDIZED_VOCABULARY_TABLE",
      JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
      GROUP_VAR_OBSERVER = "GROUP_VAR_OBSERVER",
      GROUP_VAR_DEVICE = "GROUP_VAR_DEVICE",
      TIME_VAR = "TIME_VAR",
      STUDY_SEGMENT = "STUDY_SEGMENT",
      PART_VAR = "PART_VAR",
      DETECTION_LIMITS = "DETECTION_LIMITS", SOFT_LIMITS = "SOFT_LIMITS",
      CONTRADICTIONS = "CONTRADICTIONS", DISTRIBUTION = "DISTRIBUTION",
      DECIMALS = "DECIMALS", VARIABLE_ROLE = "VARIABLE_ROLE",
      DATA_ENTRY_TYPE = "DATA_ENTRY_TYPE",
      END_DIGIT_CHECK = "END_DIGIT_CHECK",
      VARIABLE_ORDER = "VARIABLE_ORDER", LONG_LABEL =
        "LONG_LABEL", recode = "recode",
      LOCATION_RANGE = "LOCATION_RANGE",
      LOCATION_METRIC = "LOCATION_METRIC",
      PROPORTION_RANGE = "PROPORTION_RANGE",
      MISSING_LIST_TABLE = "MISSING_LIST_TABLE",
      CO_VARS = "CO_VARS",
      GRADING_RULESET = "GRADING_RULESET",
      RECODE_CASES = "RECODE_CASES",
      RECODE_CONTROL = "RECODE_CONTROL",
      DATAFRAMES = "DATAFRAMES",
      ENCODING = "ENCODING"
    ),
    OPTIONAL
  ))

  expect_error(
    prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
    MISSING_LIST = 3), TECHNICAL),
    regexp = "Not all variable attributes of requirement level .+technical.+",
    perl = TRUE
  )

  expect_error(
    prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
                                     MISSING_LIST = 3), "TECHNICAL",
                          character.only = TRUE),
    regexp = "Not all variable attributes of requirement level .+technical.+",
    perl = TRUE
  )

  expect_error(
    prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
                                     MISSING_LIST = 3), "TEXCHNICAL",
                          character.only = TRUE),
    regexp = paste("Error regarding argument .+level.+: .+arg.+ should be",
                   "one of .+COMPATIBILITY.+, .+REQUIRED.+, .+RECOMMENDED.+,",
                   ".+OPTIONAL.+, .+TECHNICAL.+"),
    perl = TRUE
  )

  expect_error(
    prep_check_meta_names(meta_data = list(VAR_NAMES = 1, DATA_TYPE = 2,
                                           MISSING_LIST = 3), "TECHNICAL",
                          character.only = TRUE),
    regexp = paste(".+meta_data.+",
                   "is not a data frame"),
    perl = TRUE
  )

  expect_warning(
    prep_check_meta_names(
      data.frame(
        VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
        LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
        JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
        GROUP_VAR_OBSERVER = "GROUP_VAR_OBSERVER",
        GROUP_VAR_DEVICE = "GROUP_VAR_DEVICE",
        TIME_VAR = "TIME_VAR",
        STUDY_SEGMENT = "STUDY_SEGMENT",
        PART_VAR = "PART_VAR",
        DETECTION_LIMITS = "DETECTION_LIMITS", SOFT_LIMITS = "SOFT_LIMITS",
        CONTRADICTIONS = "CONTRADICTIONS", DISTRIBUTION = "DISTRIBUTION",
        DECIMALS = "DECIMALS", VARIABLE_ROLE = "VARIABLE_ROLE",
        DATA_ENTRY_TYPE = "DATA_ENTRY_TYPE",
        END_DIGIT_CHECK = "END_DIGIT_CHECK",
        VARIABLE_ORDER = "VARIABLE_ORDER", LONG_LABEL =
          "LONG_LABEL", recode = "recode", VARNAMES = 19
      ),
      REQUIRED
    ),
    regexp = paste(
      "Found the following addtional metadata columns,",
      "which look like typos of defined names: .+VARNAMES.+ -> .+VAR_NAMES.+"
    ),
    perl  = TRUE
  )
})
