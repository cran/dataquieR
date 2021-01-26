test_that("util_check_data_type works", {
  expect_true(util_check_data_type(42, DATA_TYPES$INTEGER))
  expect_true(util_check_data_type(1:42, DATA_TYPES$INTEGER))
  expect_true(util_check_data_type(NA, DATA_TYPES$INTEGER))
  expect_false(util_check_data_type(c(TRUE, NA), DATA_TYPES$INTEGER))
  expect_true(util_check_data_type(c("text", NA), DATA_TYPES$STRING))
  expect_false(util_check_data_type(c(42.5, as.integer(12)),
                                    DATA_TYPES$INTEGER))
  expect_true(util_check_data_type(c(42.5, as.integer(12)), DATA_TYPES$FLOAT))
  expect_true(util_check_data_type(c(Sys.time(), as.integer(12)),
                                   DATA_TYPES$DATETIME))
  expect_false(util_check_data_type(c(as.integer(Sys.time()), as.integer(12)),
                                    DATA_TYPES$DATETIME))
  expect_false(util_check_data_type("2020-11-11 17:10:07 CET",
                                    DATA_TYPES$DATETIME))
  expect_true(util_check_data_type(as.POSIXct("2020-11-11 17:10:07 CET"),
                                   DATA_TYPES$DATETIME))
})
