test_that("test-int_sts_element_dataframe works", {
  skip_on_cran() # online, fragile
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  sd0 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", keep_types = TRUE)
  md0 <- prep_get_data_frame("item_level")
  dl0 <- prep_get_data_frame("dataframe_level")
  dl0$DF_NAME <- "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS"
  md0 <- md0[!(md0$VAR_NAMES %in% c("sex")), , FALSE] # remove a variable
  ised <- int_sts_element_dataframe(item_level = md0, meta_data_dataframe = dl0)
  expect_snapshot_value(ised, style = "deparse")
})
