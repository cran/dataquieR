test_that("util_check_data_type works", {
  skip_on_cran()
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
  expect_error(util_check_data_type(as.POSIXct("2020-11-11 17:10:07 CET"),
                                   "xyz"),
               regexp = ".+xyz.+ is not a known data type.")

  # 0 = Mismatch, not convertible
  # 1 = Match
  # 2 = Mismatch, but convertible

  expect_true(all(2 == util_check_data_type("2020-11-11 17:10:07 CET",
                                    DATA_TYPES$DATETIME,
                                    check_convertible = TRUE)))
  expect_true(all(0 == util_check_data_type("not a date",
                                            DATA_TYPES$DATETIME,
                                            check_convertible = TRUE)))

  expect_equal(util_check_data_type(42, DATA_TYPES$INTEGER,
                                   return_percentages = TRUE),
               util_attach_attr(0, which = FALSE))
  expect_equal(util_check_data_type(42.5, DATA_TYPES$INTEGER,
                                   return_percentages = TRUE),
               util_attach_attr(100, which = TRUE))
  expect_equal(util_check_data_type(c("42", "x"), DATA_TYPES$INTEGER,
                                    return_percentages = TRUE),
               util_attach_attr(100, which = c(`42` = TRUE, x = TRUE)))
  expect_equal(util_check_data_type(c("42", "x"), DATA_TYPES$INTEGER,
                                    check_convertible = TRUE,
                                    return_percentages = FALSE), 0)
  expect_equal(util_check_data_type(c(42, 2), DATA_TYPES$INTEGER,
                                    check_convertible = TRUE,
                                    return_percentages = FALSE), 1)
  expect_equal(util_check_data_type(c("42", "2"), DATA_TYPES$INTEGER,
                                    check_convertible = TRUE,
                                    return_percentages = FALSE), 2)
  expect_equal(util_check_data_type(c(42, 42.6, 42L), DATA_TYPES$INTEGER,
                                    check_convertible = TRUE,
                                    check_conversion_stable = TRUE,
                                    return_percentages = TRUE),
                   util_attach_attr(c(match = 200/3,
                     convertible_mismatch_stable = 0,
                     convertible_mismatch_unstable = 100/3,
                     nonconvertible_mismatch = 0), which = list(
                       match = c(FALSE, TRUE, FALSE),
                       convertible_mismatch_stable = c(FALSE, FALSE, FALSE),
                       convertible_mismatch_unstable = c(FALSE, TRUE, FALSE),
                       nonconvertible_mismatch = c(FALSE, FALSE, FALSE)
                     )))

})
