test_that("util_ensure_suggested works", {
  skip_on_cran()
  ip <- installed.packages()
  existing <- "ggplot2"
  if (!(existing %in% ip[, "Package"])) {
    fail(paste("Package", dQuote(existing),
               "is a dependency as of writing this.",
               "Therefore, it is assumed to be an installed package here.",
               "However, it is missing."))
  }
  found <- FALSE
  i <- 1
  while (!found) { # look for an uninstalled package name
    unexist  <- sprintf("%s_%d", existing, i)
    i <- i + 1
    found <- !(unexist %in% ip[, "Package"])
  }
  expect_silent(util_ensure_suggested(existing, "test the function"))
  expect_error(util_ensure_suggested(unexist, "test the function"),
               regexp = "Missing the package\\(s\\) .+ to test the function",
               perl = TRUE)
})
