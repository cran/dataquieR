test_that("prep_pmap works", {
  df <- data.frame(
    x = c("apple", "banana", "cherry"),
    pattern = c("p", "n", "h"),
    replacement = c("P", "N", "H"),
    stringsAsFactors = FALSE
  )
  expect_equal(
    prep_pmap(df, gsub),
    list(
      "aPPle",
      "baNaNa",
      "cHerry"
    )
  )
  expect_error(
    prep_pmap(df, 42),
    regexp = "Argument .+\\.f.+ should be a function.",
    perl = TRUE
  )
})
