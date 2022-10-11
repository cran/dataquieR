test_that("util_observations_in_subgroups works", {
  dat <- cars[1:10, , FALSE]
  dat[1:3, 1] <- NA
  dat[2:6, 2] <- NA
  r <- util_observations_in_subgroups(dat, c("speed", "dist"))
  expect_equal(r[, "OBSERVATIONS_OUT_speed", TRUE], c(rep(FALSE, 3),
                                                      rep(TRUE, 7)))
  expect_equal(r[, "OBSERVATIONS_OUT_dist", TRUE], c(TRUE,
                                                     rep(FALSE, 5),
                                                     rep(TRUE, 4)))


  dat <- cars[FALSE, "speed", !TRUE]
  r <- util_observations_in_subgroups(dat, c("speed"))
  expect_equal(r[, "OBSERVATIONS_OUT_speed", TRUE], FALSE)

})
