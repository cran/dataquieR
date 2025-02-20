test_that("prep_acc_distributions_with_ecdf works", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")

  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  expect_silent(
    r1 <- prep_acc_distributions_with_ecdf(resp_vars = "SBP_0",
                                           group_vars = "USR_BP_0",
                                           study_data = study_data,
                                           meta_data = meta_data)
  )

  expect_false(
    inherits(try(ggplot_build(r1$SummaryPlot)), "try-error"))

})
