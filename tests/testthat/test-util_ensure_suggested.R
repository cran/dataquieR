test_that("util_ensure_suggested works", {
  existing <- installed.packages()[[1]]
  found <- FALSE
  i <- 1
  while (!found) { # look for an uninstalled package name
    unexist  <- sprintf("%s_%d", existing, i)
    i <- i + 1
    found <- !requireNamespace(found, quietly = TRUE)
  }
  expect_silent(util_ensure_suggested(existing, "test the function"))
  expect_error(util_ensure_suggested(unexist, "test the function"),
               regexp = "Missing the package\\(s\\) .+ to test the function",
               perl = TRUE)
})
