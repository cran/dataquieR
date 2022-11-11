test_that("prep_check_meta_names works", {
 expect_silent(prep_check_meta_names(data.frame(), level = NULL))

 expect_silent( prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
                        MISSING_LIST = 3)))

  expect_silent(prep_check_meta_names(
    data.frame(
      VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
      LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
      JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
      KEY_OBSERVER = "KEY_OBSERVER", KEY_DEVICE = "KEY_DEVICE",
      KEY_DATETIME = "KEY_DATETIME",
      KEY_STUDY_SEGMENT = "KEY_STUDY_SEGMENT"
    ),
    RECOMMENDED
  ))

  expect_silent(prep_check_meta_names(
    data.frame(
      VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
      LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
      JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
      KEY_OBSERVER = "KEY_OBSERVER", KEY_DEVICE = "KEY_DEVICE",
      KEY_DATETIME = "KEY_DATETIME", KEY_STUDY_SEGMENT =
        "KEY_STUDY_SEGMENT",
      DETECTION_LIMITS = "DETECTION_LIMITS", SOFT_LIMITS = "SOFT_LIMITS",
      CONTRADICTIONS = "CONTRADICTIONS", DISTRIBUTION = "DISTRIBUTION",
      DECIMALS = "DECIMALS", VARIABLE_ROLE = "VARIABLE_ROLE",
      DATA_ENTRY_TYPE = "DATA_ENTRY_TYPE",
      VARIABLE_ORDER = "VARIABLE_ORDER", LONG_LABEL =
        "LONG_LABEL", recode = "recode"
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
    regexp = paste("In prep_check_meta_names: meta data",
                   "is not a data frame at all"),
    perl = TRUE
  )

  expect_warning(
    prep_check_meta_names(
      data.frame(
        VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
        LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
        JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
        KEY_OBSERVER = "KEY_OBSERVER", KEY_DEVICE = "KEY_DEVICE",
        KEY_DATETIME = "KEY_DATETIME", KEY_STUDY_SEGMENT =
          "KEY_STUDY_SEGMENT",
        DETECTION_LIMITS = "DETECTION_LIMITS", SOFT_LIMITS = "SOFT_LIMITS",
        CONTRADICTIONS = "CONTRADICTIONS", DISTRIBUTION = "DISTRIBUTION",
        DECIMALS = "DECIMALS", VARIABLE_ROLE = "VARIABLE_ROLE",
        DATA_ENTRY_TYPE = "DATA_ENTRY_TYPE",
        VARIABLE_ORDER = "VARIABLE_ORDER", LONG_LABEL =
          "LONG_LABEL", recode = "recode", VARNAMES = 19
      ),
      REQUIRED
    ),
    regexp = paste(
      "Found the following addtional metadata columns,",
      "which look like typos of defined names: .+VARNAMES.+ -> .+VAR_NAMES.+"
    ),
    perl  = TRUE,
    all = TRUE
  )
})
