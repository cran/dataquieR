test_that("int_unexp_elements works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  skip("TODO") # TODO
  sd1 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS")
  md1 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta.RDS")

  expect_error(
    int_unexp_elements(
      segments = c("INTRO", "INTERVIEW"),
      data_element_count = c(4, 11),
      meta_data = md1)
  )

  out <- int_unexp_elements(
    segments = c("INTRO", "INTERVIEW"),
    data_element_count = c(4, 11),
    study_data = sd1,
    meta_data = md1)

  expect_equal(out$SegmentData$Number.of.mistmatches[[1]], 0)

  testthat::local_edition(3)
  expect_snapshot_value(style = "deparse",

  )

})
