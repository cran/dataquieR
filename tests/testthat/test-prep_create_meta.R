test_that("prep_create_meta works", {
  l <- VARATT_REQUIRE_LEVELS$REQUIRED
  expect_warning(
    meta_data1 <- prep_create_meta(
      VAR_NAMES = letters,
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = LETTERS,
      MISSING_LIST = "999|998",
      XYZ = character(0),
      level = l,
      character.only = TRUE
    ),
    regexp =
      "The following variable attributes are NULL, will ignore these: .+XYZ.+",
    perl = TRUE,
    all = TRUE
  )

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
  expect_identical(meta_data1, expc)
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
  expect_error(
    meta_data <- prep_create_meta(
      VAR_NAMES = letters,
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = LETTERS,
      XYZ = 1:3,
      MISSING_LIST = "999|998"
    ),
    regexp = paste("The given variable attributes have different lengths and",
                   "cannot be equalized by repeating the shorter ones to",
                   "create a data frame with as 26 rows .the longest variable",
                   "attribute vector provided.. R would say:",
                   "arguments imply differing number of rows: 1, 3, 26"),
    perl = TRUE
  )
})
