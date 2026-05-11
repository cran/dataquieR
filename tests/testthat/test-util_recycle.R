test_that("util_recycle() returns empty list for no inputs", {
  expect_identical(util_recycle(), list())
})

test_that("util_recycle() leaves equally sized vectors unchanged", {
  x <- 1:3
  y <- c("a", "b", "c")

  res <- util_recycle(x, y)

  expect_length(res, 2L)
  expect_identical(res[[1L]], x)
  expect_identical(res[[2L]], y)
})

test_that("util_recycle() recycles length-1 vectors to common size", {
  x <- 1
  y <- 1:4

  res <- util_recycle(x, y)

  expect_length(res, 2L)
  expect_identical(res[[1L]], c(1, 1, 1, 1))
  expect_identical(res[[2L]], y)
})

test_that("util_recycle() recycles shorter vectors to maximum size", {
  x <- 1:2
  y <- 1:4

  res <- util_recycle(x, y)

  expect_length(res, 2L)
  expect_identical(res[[1L]], c(1L, 2L, 1L, 2L))
  expect_identical(res[[2L]], y)
})

test_that("util_recycle() respects explicit .size", {
  x <- 1:2
  y <- 9

  res <- util_recycle(x, y, .size = 4L)

  expect_length(res, 2L)
  expect_identical(res[[1L]], c(1L, 2L, 1L, 2L))
  expect_identical(res[[2L]], c(9, 9, 9, 9))
})

test_that("util_recycle() supports zero-length vectors if target size is zero", {
  x <- integer()
  y <- character()

  res <- util_recycle(x, y)

  expect_length(res, 2L)
  expect_identical(res[[1L]], integer())
  expect_identical(res[[2L]], character())
})

test_that("util_recycle() errors for incompatible lengths", {
  expect_error(
    util_recycle(1:2, 1:3),
    "Can't recycle"
  )
})

test_that("util_recycle() errors for incompatible explicit .size", {
  expect_error(
    util_recycle(1:2, .size = 3L),
    "Can't recycle"
  )
})

test_that("util_recycle() errors for invalid .size", {
  expect_error(
    util_recycle(1:2, .size = NA_integer_),
    ".size"
  )
  expect_error(
    util_recycle(1:2, .size = -1L),
    ".size"
  )
  expect_error(
    util_recycle(1:2, .size = 1.5),
    ".size"
  )
  expect_error(
    util_recycle(1:2, .size = c(2L, 3L)),
    ".size"
  )
})

test_that("util_recycle() errors for unsupported inputs", {
  expect_error(
    util_recycle(environment()),
    "unsupported class"
  )
})
