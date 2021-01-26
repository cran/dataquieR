test_that(".onLoad works", {
  try(.onLoad(), silent = TRUE)
  expect_true(
    all(
      names(WELL_KNOWN_META_VARIABLE_NAMES) %in%
        getNamespaceExports("dataquieR")
    )
  )
})
