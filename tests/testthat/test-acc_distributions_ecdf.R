test_that("acc_distributions_ecdf works", {
  skip_on_cran() # slow and tested elsewhere
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("colorspace")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx|item_level")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", keep_types = TRUE)
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  t1 <- acc_distributions_ecdf(group_vars = "obs_soma",
                                 study_data = study_data,
                                 meta_data = meta_data)

  expect_equal(length(names(t1$SummaryPlotList)), 13)

  expect_false(
    inherits(try(ggplot_build(t1$SummaryPlot$exdate)), "try-error"))
  expect_false(
    inherits(try(ggplot_build(t1$SummaryPlot$cholesterol)), "try-error"))
})
