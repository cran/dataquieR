test_that("util_map_by_largest_prefix works", {
  skip_on_cran()

  expect_equal(util_map_by_largest_prefix(
    "acc_distributions_loc_ecdf_observer_time",
    names(dataquieR:::.manual$titles)
  ), "acc_distributions_loc")

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc_observer_time",
      names(dataquieR:::.manual$titles)
    ),
    "acc_distributions_loc"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc_ecdf",
      names(dataquieR:::.manual$titles)
    ),
    "acc_distributions_loc"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distribution",
      names(dataquieR:::.manual$titles)
    ),
    NA_character_
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc",
      names(dataquieR:::.manual$titles)
    ),
    "acc_distributions_loc"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loessf",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx")),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loessf",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributsons", "xxx")),
    NA_character_
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loess",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess")
    ),
    "acc_distributions_loess"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loessf",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess")
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loess",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loessf")
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loessf",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx")),
    "acc_distributions"
  )


  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loess",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess")
    ),
    "acc_distributions_loess"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loessf",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess")
    ),
    "acc_distributions_loess"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loess",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loessf")
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loess",
      c("aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributionsR", "xxx", "acc_distributions_loessf")
    ),
    NA_character_
  )


})
