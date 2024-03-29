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
