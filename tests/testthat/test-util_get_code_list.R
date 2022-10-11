test_that("util_get_code_list works", {
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(NA, "-1|-2|-5"),
    JUMP_LIST = c("999", "")
  )
  expect_warning(expect_equal(
    util_get_code_list(c("age"), "MISSING_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    numeric(0)
  ),
  perl = TRUE,
  regexp = paste0("In util_get_code_list: Could not find .MISSING_LIST. for",
                  " .age. in the meta_data for replacing codes with NAs.")
  )
  expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    c(-1, -2, -5)
  )
  expect_equal(
    util_get_code_list(c("sex"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    numeric(0)
  )
  expect_equal(
    util_get_code_list(c("age"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    999
  )
  expect_warning(
    util_get_code_list(c("age"), "XJUMP_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = VAR_NAMES,
                       warning_if_no_list = TRUE
    ),
    regexp = paste(
      "Meta data does not provide a column called .*XJUMP_LIST.*",
      "for replacing codes with NAs."
    ),
    all = TRUE,
    perl = TRUE
  )
  expect_warning(
    util_get_code_list(c("age"), "JUMP_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = "xx",
                       warning_if_no_list = TRUE
    ),
    regexp = paste(
      "Meta data does not provide a column called .+xx.+",
      "for the labels."
    ),
    all = TRUE,
    perl = TRUE
  )
  # util_get_code_list(c("sex", "age"), "JUMP_LIST",
  #                    split_char = SPLIT_CHAR,
  #                    mdf = mdf,
  #                    label_col = VAR_NAMES,
  #                    warning_if_no_list = TRUE)
})

test_that("util_get_code_list warns about non-numeric codes", {
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(NA, NA),
    JUMP_LIST = c("999", "")
  )
  mdf$MISSING_LIST[2] <- "-1|-2|-5|x"
  expect_warning(expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = VAR_NAMES,
                       warning_if_no_list = TRUE
    ),
    c(-1, -2, -5, NA)
  ),
  perl = TRUE,
  regexp = paste("In util_get_code_list: Some codes ..MISSING_LIST.. were",
                  "not numeric for .sex.: .x., these will be ignored")
  )
})

test_that("util_get_code_list recognizes no code on purpose", {
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c("999", "")
  )
  expect_silent(expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = VAR_NAMES,
                       warning_if_no_list = TRUE
    ),
    NA_real_
  ))
})
