test_that(".onLoad works", {
  skip_if_not_installed("pkgload")
  skip_on_cran()
  try(.onLoad(), silent = TRUE)
  expect_equal(
    intersect(
      names(WELL_KNOWN_META_VARIABLE_NAMES),
      names(pkgload::pkg_env("dataquieR"))
    ),
    names(WELL_KNOWN_META_VARIABLE_NAMES)
  )
})

test_that(".set_properties works", {
  p <- as.list(dataquieR.properties)
  withr::defer(list2env(p, dataquieR.properties))
  .set_properties(list(a = "b"))
  expect_identical(dataquieR.properties$a, "b")
})

test_that("with_pipeline works", {
  expect_false(.dq2_globs$.called_in_pipeline)
  with_pipeline(expect_true(.dq2_globs$.called_in_pipeline))
})
