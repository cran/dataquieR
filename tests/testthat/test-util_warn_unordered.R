test_that("util_warn_unordered works", {
  skip_if_translated()
  expect_warning(util_warn_unordered("Hallo", "x"),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+x.+ (.+character.+)")
  expect_silent(util_warn_unordered(17, "xyz"))
  expect_warning(util_warn_unordered("17", "x"),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+x.+ (.+character.+)")
  expect_warning(util_warn_unordered(complex(1, 2), "x"),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+x.+ (.+complex.+)")
  expect_warning(util_warn_unordered(complex(1, 2), "xya"),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+xya.+ (.+complex.+)")
  expect_warning(util_warn_unordered(complex(1, 2), "xyz"),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+xyz.+ (.+complex.+)")
  expect_error(util_warn_unordered(cars, "xyz"),
               regexp = "only works on effectively one-dimensional input")
  expect_warning(util_warn_unordered(c, "xyz"),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+xyz.+ (.+builtin+)")
  expect_warning(util_warn_unordered(c),
                 perl = TRUE,
                 regexp =
                   "Don't know, how to compare values of .+c.+ (.+builtin+)")
  expect_error(util_warn_unordered(matrix(1:9, nrow = 3)),
               regexp = "only works on effectively one-dimensional input")
  expect_silent(util_warn_unordered(matrix(1:9, nrow = 1)))
  expect_error(util_warn_unordered(),
               regexp = "argument .+x.+ is missing, with no default")
})
