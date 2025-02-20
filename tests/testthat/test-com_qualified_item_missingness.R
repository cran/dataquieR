test_that("com_qualified_item_missingness works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  r1 <- com_qualified_item_missingness(resp_vars = "SBP_0",
                                 study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                 label_col = LABEL)
  expect_identical(dim(r1$SummaryTable), c(1L, 15L))
  suppressWarnings(expect_warning(
    r2 <- com_qualified_item_missingness(study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                   label_col = LABEL),
    regexp = "No missing-match-table"
  ))
  expect_identical(dim(r2$SummaryTable), c(36L, 15L))

  mt <- prep_get_data_frame("missing_table")
  mt <- mt[!(mt$CODE_INTERPRET %in% c("I", "P", "PL")), , FALSE] # for qual.segment.missingness, this would trigger warnings
  prep_add_data_frames(missing_table = mt)
  r3 <- com_qualified_item_missingness(resp_vars = "SBP_0",
                                       study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                       label_col = LABEL)

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  mt <- prep_get_data_frame("missing_table")
  mt <- mt[!(mt$CODE_INTERPRET %in% c("R", "BO")), , FALSE]
  prep_add_data_frames(missing_table = mt)
  expect_warning(
    r4 <- com_qualified_item_missingness(resp_vars = "SBP_0",
                                         study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                         label_col = LABEL),
    regexp = ".*Nonresponse Rate 1.*"
  )

})

# devtools::test_coverage_active_file()
