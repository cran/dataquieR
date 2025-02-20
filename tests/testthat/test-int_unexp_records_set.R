test_that("int_unexp_records_set works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  r <- int_unexp_records_set(
    level = "segment",
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS",
    meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx",
    label_col = LABEL
  )
  expect_snapshot(r)
})
