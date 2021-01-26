test_that("prep_study2meta works", {
  meta_data <- prep_study2meta(cars)
  expect_equal(meta_data,
               data.frame(VAR_NAMES = colnames(cars),
                          DATA_TYPE = "integer",
                          MISSING_LIST = "",
                          DETECTION_LIMITS = NA_character_,
                          SOFT_LIMITS = NA_character_,
                          CONTRADICTIONS = NA_character_,
                          DISTRIBUTION = NA_character_,
                          DECIMALS = NA_character_,
                          DATA_ENTRY_TYPE = NA_character_,
                          VARIABLE_ROLE = NA_character_,
                          VARIABLE_ORDER = NA_character_,
                          LONG_LABEL = NA_character_,
                          recode = NA_character_,
                          stringsAsFactors = FALSE,
                          row.names = colnames(cars))
  )
  expect_error(prep_study2meta(42),
               regexp = "Need study data as a data frame")
  expect_error(prep_study2meta(NULL),
               regexp = "Need study data as a data frame")
  expect_error(prep_study2meta(),
               regexp = "Need study data as a data frame")
  expect_error(prep_study2meta(cars[, c()]),
               regexp = "No study variables found -- cannot proceed.")
})

test_that("prep_study2meta handles tibbles correctly", {
  skip_if_not_installed("tibble") # should never be skipped, since dplyr is a
                                  # dependency of dataquieR and dplr depends on
                                  # tibble
  meta_data <- prep_study2meta(tibble::as_tibble(cars))
  expect_equal(meta_data,
               data.frame(VAR_NAMES = colnames(cars),
                          DATA_TYPE = "integer",
                          MISSING_LIST = "",
                          DETECTION_LIMITS = NA_character_,
                          SOFT_LIMITS = NA_character_,
                          CONTRADICTIONS = NA_character_,
                          DISTRIBUTION = NA_character_,
                          DECIMALS = NA_character_,
                          DATA_ENTRY_TYPE = NA_character_,
                          VARIABLE_ROLE = NA_character_,
                          VARIABLE_ORDER = NA_character_,
                          LONG_LABEL = NA_character_,
                          recode = NA_character_,
                          stringsAsFactors = FALSE,
                          row.names = colnames(cars))
  )
  meta_data <-
    expect_warning(
      with_mock(requireNamespace = function(...) {
        return(FALSE)
      },
        prep_study2meta(tibble::as_tibble(cars))
      ),
    regexp =
      sprintf("%s",
              paste(".+study_data.+ looks like a tibble. However, the package",
                    ".+tibble.+ seems not to be available,")),
    perl = TRUE,
    all = TRUE
  )
  expect_equal(meta_data,
               data.frame(VAR_NAMES = colnames(cars),
                          DATA_TYPE = "integer",
                          MISSING_LIST = "",
                          DETECTION_LIMITS = NA_character_,
                          SOFT_LIMITS = NA_character_,
                          CONTRADICTIONS = NA_character_,
                          DISTRIBUTION = NA_character_,
                          DECIMALS = NA_character_,
                          DATA_ENTRY_TYPE = NA_character_,
                          VARIABLE_ROLE = NA_character_,
                          VARIABLE_ORDER = NA_character_,
                          LONG_LABEL = NA_character_,
                          recode = NA_character_,
                          stringsAsFactors = FALSE,
                          row.names = colnames(cars))
  )
})

test_that("prep_study2meta works on study_data", {
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  study_data <- study_data[, order(colnames(study_data)), FALSE]
  meta_data <- meta_data[order(meta_data$VAR_NAMES), , FALSE]
  meta_data[meta_data$LABEL %in% c("SBP_0", "DBP_0", "ARM_CIRC_0", "BSG_0"),
            DATA_TYPE] <- # values are not really float
    DATA_TYPES$INTEGER
  guessed_meta_data <- prep_study2meta(study_data)
  expect_equal(
    guessed_meta_data,
    data.frame(
      VAR_NAMES = meta_data$VAR_NAMES,
      DATA_TYPE = meta_data$DATA_TYPE,
      MISSING_LIST = c(
        "",
        "",
        "",
        "",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989",
        "",
        "99980|99988|99989|99990",
        "99980",
        "",
        "",
        "",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "",
        "",
        "99980",
        "99980",
        "99980",
        "99980",
        "99980",
        "99980",
        "99980",
        "99980",
        "",
        "",
        "",
        "",
        "99980",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "99980|99988|99989|99990",
        "",
        "",
        "99980",
        "",
        "",
        "",
        "",
        ""
      ),
      DETECTION_LIMITS = NA_character_,
      SOFT_LIMITS = NA_character_,
      CONTRADICTIONS = NA_character_,
      DISTRIBUTION = NA_character_,
      DECIMALS = NA_character_,
      DATA_ENTRY_TYPE = NA_character_,
      VARIABLE_ROLE = NA_character_,
      VARIABLE_ORDER = NA_character_,
      LONG_LABEL = NA_character_,
      recode = NA_character_,
      stringsAsFactors = FALSE,
      row.names = colnames(study_data)
    )
  )
})
