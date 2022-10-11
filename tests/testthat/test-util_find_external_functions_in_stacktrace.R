test_that("util_find_external_functions_in_stacktrace works", {
  test1 <- function() {
    util_find_external_functions_in_stacktrace()
  }
  environment(test1) <- asNamespace("dataquieR")
  test2 <- function() {
    test1 <- function() {
      util_find_external_functions_in_stacktrace()
    }
    environment(test1) <- asNamespace("dataquieR")
    test1()
  }
  a <- test2()
  expect_equal(
    a[1:1], 3,
  )
  environment(test2) <- asNamespace("dataquieR")
  b <- test2()
  expect_equal(
    b, a[-1]
  )
})

test_that("util_find_external_functions_in_stacktrace works from emptyenv", {
  test1 <- function() {
    util_find_external_functions_in_stacktrace()
  }
  environment(test1) <- asNamespace("dataquieR")
  test2 <- function() {
    test1 <- function() {
      util_find_external_functions_in_stacktrace()
    }
    environment(test1) <- asNamespace("dataquieR")
    test1()
  }
  test3 <- function() {
    test1 <- function() {
      util_find_external_functions_in_stacktrace()
    }
    environment(test1) <- asNamespace("dataquieR")
    test1()
  }
  a <- test2()
  expect_equal(
    a[1:1], 3,
  )
  environment(test2) <- asNamespace("dataquieR")
  b <- test2()
  expect_equal(
    b, a[-1]
  )
  test3()
  # Regression test for "the empty environment has no parent"
  test4 <- function() {
    testtest <- function() {
      util_find_external_functions_in_stacktrace()
    }
    environment(testtest) <- asNamespace("dataquieR")
    testtest()
  }
  atestenv <- new.env(parent = emptyenv())
  atestenv$`{` <- `{`
  atestenv$`<-` <- `<-`
  atestenv$`function` <- `function`
  atestenv$asNamespace <- asNamespace
  atestenv$`environment<-` <- `environment<-`
  environment(test4) <- atestenv
  test4()
})
