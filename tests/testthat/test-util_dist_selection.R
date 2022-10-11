test_that("util_dist_selection works", {
  study_data <- data.frame(
    integer = c(1, 2, 3, 4),
    factor = factor(c(1, 2, 2, 3),
      levels = 1:3,
      labels = c("male", "female", "undefined")
    ),
    ordered = ordered(c(3, 2, 1, 3),
      levels = 3:1,
      labels = c("low", "medium", "high")
    ),
    integer = as.integer(c(1, 2, 3, 4)),
    logical = c(TRUE, FALSE, TRUE, FALSE),
    float = 1:4 * pi
  )
  dist_info <- util_dist_selection(study_data)
  expect_equal(
    dist_info$IsInteger,
    c(rep(TRUE, 4), FALSE, FALSE)
  )
  expect_equal(
    dist_info$IsMultCat,
    c(rep(1, 4), 0, 0)
  )
  expect_equal(
    dist_info$NCategory,
    c(4, 3, 3, 4, NA, NA)
  )
})
