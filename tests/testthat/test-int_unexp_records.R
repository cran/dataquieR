test_that("int_unexp_records works", {
  skip_on_cran()
  skip("TODO") # TODO
  sd1 <- prep_get_data_frame("ship")
  md1 <- prep_get_data_frame("ship_meta")

  expect_error(
    int_unexp_records(
      segments = c("INTRO", "INTERVIEW"),
      data_record_count = c(4, 11),
      meta_data = md1)
  )

  out <- int_unexp_records(
    segments = c("INTRO", "INTERVIEW"),
    data_record_count = c(2154, 1100),
    study_data = sd1,
    meta_data = md1)

  expect_equal(out$SegmentData$Number.of.mistmatches[[1]], 0)

  })
