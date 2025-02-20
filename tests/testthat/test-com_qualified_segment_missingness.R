test_that("com_qualified_segment_missingness no meta", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            res <- com_qualified_segment_missingness(
              study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
              meta_data = "item_level",
              meta_data_segment = "segment_level"),
            regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
          ),
          regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
        ),
        regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
      ),
      regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
    ),
    regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
  )
  expect_type(res, "list")
  expect_s3_class(res$SegmentTable, "data.frame")
  expect_s3_class(res$SegmentData, "data.frame")
  expect_length(res$SegmentData, 0)
  expect_length(res$SegmentTable, 0)
})

test_that("com_qualified_segment_missingness with meta", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  res <- com_qualified_segment_missingness(
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS",
    meta_data = "item_level",
    meta_data_segment = "segment_level")
  expect_type(res, "list")
  expect_s3_class(res$SegmentTable, "data.frame")
  expect_s3_class(res$SegmentData, "data.frame")
  expect_equal(nrow(res$SegmentData), 4)
  expect_equal(nrow(res$SegmentTable), 4)
  expect_true(any(res$SegmentTable[, 2:ncol(res$SegmentTable)] > 0))
  expect_true(any(res$SegmentData[, 2:ncol(res$SegmentData)] > 0))
})
