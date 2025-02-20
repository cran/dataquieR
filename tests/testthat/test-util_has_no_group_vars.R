test_that("util_has_no_group_vars works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  expect_equal(util_has_no_group_vars("SBP_0"), FALSE)
  expect_equal(util_has_no_group_vars("DBP_0"), FALSE)
  expect_error(util_has_no_group_vars(NA))
  expect_equal(util_has_no_group_vars("SEX_0"), TRUE)
})
