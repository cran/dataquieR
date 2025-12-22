test_that("util_disjunct_var_sets works for disjoint sets", {
  expect_silent(util_disjunct_var_sets(
    resp_vars = c("a", "b"),
    co_vars = c("c", "d"),
    group_vars = c("e")
  ))
})

test_that("util_disjunct_var_sets throws error on overlap", {
  expect_error(
    util_disjunct_var_sets(
      resp_vars = c("a", "b"),
      co_vars = c("b", "c"),
      group_vars = c("e")
    ),
    regexp = "Overlap in the arguments .*b.*"
  )
})

test_that("util_disjunct_var_sets catches multiple overlaps", {
  expect_error(
    util_disjunct_var_sets(
      resp_vars = c("a", "b", "x"),
      co_vars = c("x", "y", "z"),
      group_vars = c("b", "w")
    ),
    regexp = "Overlap in the arguments .*resp_vars.*co_vars.*group_vars.*"
  )
})

test_that("no-dots: caller *_vars disjoint", {
  f <- function(resp_vars, co_vars) util_disjunct_var_sets()
  expect_silent(f(c("a", "b"), c("c", "d")))
})

test_that("no-dots: caller *_vars overlap", {
  f <- function(resp_vars, co_vars) util_disjunct_var_sets()
  expect_error(f(c("a", "b"), c("b", "c")),
               regexp = "Overlap in the arguments")
})

test_that("defaults are used when not passed", {
  f_ok <- function(resp_vars = c("a"), co_vars = c("b"))
    util_disjunct_var_sets()
  f_bad <- function(resp_vars = c("a"), co_vars = c("a"))
    util_disjunct_var_sets()
  expect_silent(f_ok())
  expect_error(f_bad(), regexp = "Overlap in the arguments .*a.*")
})

test_that("NULL/empty defaults => nothing to check", {
  f1 <- function(resp_vars = NULL, co_vars = NULL) util_disjunct_var_sets()
  f2 <- function(resp_vars = character(), co_vars = character())
    util_disjunct_var_sets()
  expect_silent(f1())
  expect_silent(f2())
})

test_that("caller has no *_vars formals", {
  f <- function(x, y) util_disjunct_var_sets()
  expect_silent(f(1, 2))
})

test_that("caller body overrides defaults (env wins)", {
  f_ok <- function(resp_vars = c("a"), co_vars = c("a")) {
    resp_vars <- "a"; co_vars <- "b"
    util_disjunct_var_sets()
  }
  f_bad <- function(resp_vars = c("a"), co_vars = c("b")) {
    co_vars <- "a"
    util_disjunct_var_sets()
  }
  expect_silent(f_ok())
  expect_error(f_bad(), regexp = "Overlap in the arguments .*a.*")
})

test_that("direct call without args is silent", {
  expect_silent(util_disjunct_var_sets())
})
