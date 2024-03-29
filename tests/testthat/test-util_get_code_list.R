test_that("util_get_code_list works", {
  skip_on_cran()
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(NA, "-1 = -|-2 = -|-5 = -"),
    JUMP_LIST = c("999 = -", "")
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
  regexp = paste0("Could not find .MISSING_LIST. for",
                  " .age. in the meta_data for replacing codes with NAs.")
  )
  expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    c(`-` = -1,`-` =  -2, `-` = -5)
  )
  expect_equal(
    util_get_code_list(c("sex"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    setNames(numeric(0), character(0))
  )
  expect_equal(
    util_get_code_list(c("age"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    c(`-` = 999)
  )
  expect_warning(
    util_get_code_list(c("age"), "XJUMP_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = VAR_NAMES,
                       warning_if_no_list = TRUE
    ),
    regexp = paste(
      "Metadata does not provide a column called .*XJUMP_LIST.*",
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
      "Metadata does not provide a column called .+xx.+",
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
  skip_on_cran()
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(NA, NA),
    JUMP_LIST = c("999=-", "")
  )
  mdf$MISSING_LIST[2] <- "-1|-2|-5|x"
  expect_warning(expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = VAR_NAMES,
                       warning_if_no_list = TRUE
    ),
    c(`-1` = -1, `-2` = -2, `-5` = -5)
  ),
  perl = TRUE,
  regexp = paste("Some codes ..MISSING_LIST.. were",
                  "not numeric/assignment for .sex.: .x., these will be ignored")
  )
})

test_that("util_get_code_list recognizes no code on purpose", {
  skip_on_cran()
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c("999 = ", "")
  )
  expect_silent(expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
                       split_char = SPLIT_CHAR,
                       mdf = mdf,
                       label_col = VAR_NAMES,
                       warning_if_no_list = TRUE
    ),
    setNames(numeric(0), character(0))
  ))
})

