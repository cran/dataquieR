test_that("prep_study2meta works", {
  skip_on_cran() # slow, rarely used, since w/o metdata, reports are not really informative
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  testthat::local_edition(3)
  expect_error(
    prep_study2meta(cars, convert_factors = 1:3),
    regexp = "Argument.+convert_factors.+must be logical.1."
  )

  expect_snapshot_value(style = "deparse",
                        prep_study2meta(iris, convert_factors = TRUE))

  meta_data <- prep_study2meta(cars, cumulative = FALSE)
  reference <- data.frame(VAR_NAMES = colnames(cars),
                          LABEL = colnames(cars),
                          DATA_TYPE = "integer",
                          VALUE_LABELS = NA_character_,
                          MISSING_LIST = SPLIT_CHAR,
                          JUMP_LIST = NA_character_,
                          MISSING_LIST_TABLE = NA_character_,
                          HARD_LIMITS = NA_character_,
                          GROUP_VAR_OBSERVER = NA_character_,
                          GROUP_VAR_DEVICE = NA_character_,
                          TIME_VAR = NA_character_,
                          PART_VAR = NA_character_,
                          STUDY_SEGMENT = NA_character_,
                          LOCATION_RANGE = NA_character_,
                          LOCATION_METRIC = NA_character_,
                          PROPORTION_RANGE = NA_character_,
                          CO_VARS = NA_character_,
                          LONG_LABEL = colnames(cars),
                          row.names = colnames(cars))
  reference <- reference[, intersect(colnames(meta_data), colnames(reference))]

  expect_equal(meta_data, reference)

  expect_error(prep_study2meta(42),
               regexp = ".+study_data.+ is not a data frame.")
  expect_error(prep_study2meta(NULL),
               regexp = ".+study_data.+ is not a data frame.")
  expect_error(prep_study2meta(),
               regexp = "Missing .+study_data.+")
  expect_error(prep_study2meta(cars[, c()]),
               regexp = "No study variables found -- cannot proceed.")
})

test_that("prep_study2meta handles tibbles correctly", {
  skip_on_cran() # slow, rarely used, since w/o metdata, reports are not really informative
  skip_if_not_installed("tibble") # should never be skipped, since dplyr is a
                                  # dependency of dataquieR and dplr depends on
                                  # tibble
  meta_data <- prep_study2meta(tibble::as_tibble(cars), cumulative = FALSE)
  expect_equal(meta_data,
               data.frame(VAR_NAMES = colnames(cars),
                          LABEL = colnames(cars),
                          DATA_TYPE = "integer",
                          VALUE_LABELS = NA_character_,
                          MISSING_LIST = SPLIT_CHAR,
                          JUMP_LIST = NA_character_,
                          MISSING_LIST_TABLE = NA_character_,
                          HARD_LIMITS = NA_character_,
                          CO_VARS = NA_character_,
                          GROUP_VAR_OBSERVER = NA_character_,
                          GROUP_VAR_DEVICE = NA_character_,
                          TIME_VAR = NA_character_,
                          PART_VAR = NA_character_,
                          STUDY_SEGMENT = NA_character_,
                          LONG_LABEL = colnames(cars),
                          LOCATION_RANGE = NA_character_,
                          LOCATION_METRIC = NA_character_,
                          PROPORTION_RANGE = NA_character_,
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
                          LABEL = colnames(cars),
                          DATA_TYPE = "integer",
                          VALUE_LABELS = NA_character_,
                          MISSING_LIST = SPLIT_CHAR,
                          JUMP_LIST = NA_character_,
                          MISSING_LIST_TABLE = NA_character_,
                          HARD_LIMITS = NA_character_,
                          CO_VARS = NA_character_,
                          GROUP_VAR_OBSERVER = NA_character_,
                          GROUP_VAR_DEVICE = NA_character_,
                          TIME_VAR = NA_character_,
                          PART_VAR = NA_character_,
                          STUDY_SEGMENT = NA_character_,
                          LONG_LABEL = colnames(cars),
                          LOCATION_RANGE = NA_character_,
                          LOCATION_METRIC = NA_character_,
                          PROPORTION_RANGE = NA_character_,
                          stringsAsFactors = FALSE,
                          row.names = colnames(cars))
  )
})

test_that("prep_study2meta works on study_data", {
  skip_on_cran() # slow, rarely used, since w/o metdata, reports are not really informative
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
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
      LABEL = meta_data$VAR_NAMES,
      DATA_TYPE = meta_data$DATA_TYPE,
      VALUE_LABELS = NA_character_,
      MISSING_LIST = c(
        SPLIT_CHAR,
        SPLIT_CHAR,
        SPLIT_CHAR,
        SPLIT_CHAR,
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989",
        SPLIT_CHAR,
        "99980|99988|99989|99990",
        "99980",
        SPLIT_CHAR,
        SPLIT_CHAR,
        SPLIT_CHAR,
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        SPLIT_CHAR,
        SPLIT_CHAR,
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "88880|99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "88880|99980|99988|99989|99990",
        "99980|99988|99989|99990",
        "99980|99988|99989|99990",
        SPLIT_CHAR,
        SPLIT_CHAR,
        "99980|99988|99989",
        "99980|99988|99989",
        "99980|99988|99989",
        "99980|99988|99989",
        "99980|99988|99989",
        "99980|99988|99989",
        "99980|99988|99989",
        "99980|99988|99989",
        SPLIT_CHAR,
        SPLIT_CHAR,
        "99980|99988|99989|99990",
        SPLIT_CHAR,
        SPLIT_CHAR,
        "99980|99988|99989|99990",
        SPLIT_CHAR,
        SPLIT_CHAR,
        SPLIT_CHAR,
        SPLIT_CHAR,
        SPLIT_CHAR
      ),
      JUMP_LIST = NA_character_,
      MISSING_LIST_TABLE = NA_character_,
      HARD_LIMITS = NA_character_,
      CO_VARS = NA_character_,
      GROUP_VAR_OBSERVER = NA_character_,
      GROUP_VAR_DEVICE = NA_character_,
      TIME_VAR = NA_character_,
      PART_VAR = NA_character_,
      STUDY_SEGMENT = NA_character_,
      LONG_LABEL = meta_data$VAR_NAMES,
      LOCATION_RANGE = NA_character_,
      LOCATION_METRIC = NA_character_,
      PROPORTION_RANGE = NA_character_,
      stringsAsFactors = FALSE,
      row.names = colnames(study_data)
    )
  )
})
