test_that("prep_create_meta works", {
  meta_data <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = DATA_TYPES$INTEGER,
    LABEL = LETTERS,
    MISSING_LIST = "999|998"
  )
  expc <- data.frame(
    stringsAsFactors = FALSE,
    VAR_NAMES = letters,
    DATA_TYPE = DATA_TYPES$INTEGER,
    LABEL = LETTERS,
    MISSING_LIST = "999|998"
  )
  expect_identical(meta_data, expc)
  meta_data2 <- prep_create_meta(meta_data)
  expect_identical(meta_data2, expc)
  expect_error(
    meta_data <- prep_create_meta(
      VAR_NAMES = letters,
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = LETTERS,
      MISSING_LIST = "999|998",
      MISSING_LIST = "999|998"
    ),
    regexp = "Found duplicated meta columns: .+MISSING_LIST.+",
    perl = TRUE
  )
})
