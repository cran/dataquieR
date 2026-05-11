test_that("util_fix_encoding works", {
  x <- rawToChar(as.raw(c(0xC3, 0x28)), multiple = FALSE)
  Encoding(x) <- "UTF-8"
  # x has invalid encoding
  expect_warning(xfix <- util_fix_encoding(x), regexp = "deleted invalid")
  expect_no_warning(with_pipeline(util_fix_encoding(x)))
  expect_equal(xfix, "(")
})

test_that("util_fix_encoding_cols", {
  x <- data.frame(a = letters, b = 1:26, c = LETTERS)
  expect_no_warning(expect_equal(util_fix_encoding_cols(x), x))
  x$a[c(1, 4)] <- rawToChar(as.raw(c(0xC3, 0x28)), multiple = FALSE)
  x$c[c(1, 4)] <- rawToChar(as.raw(c(0xC3, 0x28)), multiple = FALSE)
  Encoding(x$a) <- "UTF-8"
  Encoding(x$c) <- "UTF-8"
  expect_warning(expect_warning(xfix <- util_fix_encoding_cols(x),
                                regexp = "deleted invalid"),
                 regexp = "deleted invalid")
})
