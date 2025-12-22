test_that("util_acc_loess_bin works", {
  skip_on_cran() # slow

  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  require_english_locale_and_berlin_tz()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  # numeric response, contrast 'intermediate' and 'low'/'high' values,
  # without co_vars
  sd0 <- study_data
  sd0$v00003[1:10] <- NA
  sd0$v00002[11:20] <- NA
  expect_message2(
    res1 <-
      util_acc_loess_bin(resp_vars = "CRP_0", study_data = sd0,
                    meta_data = meta_data, group_vars = "DEV_NO_0",
                    time_vars = "LAB_DT_0", co_vars = NULL,
                    label_col = LABEL)
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )

  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error"))

  # numeric response, contrast 'intermediate' and 'low'/'high' values,
  # with co_vars
  expect_message2(
    res2 <-
      util_acc_loess_bin(resp_vars = "CRP_0", study_data = sd0,
                    meta_data = meta_data, group_vars = "DEV_NO_0",
                    time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
                    label_col = LABEL)
    ,
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 327 observations were excluded. Due to missing values in CRP_0, N = 130 observations were excluded"
  )

  expect_false(
    inherits(try(ggplot_build(res2$SummaryPlotList$CRP_0)), "try-error"))

  # nominal response, recoding to binary should be done automatically
  # no group_var
  expect_message2(
    res3 <-
      util_acc_loess_bin(resp_vars = "CENTER_0", study_data = study_data,
                    meta_data = meta_data, time_vars = "EXAM_DT_0",
                    label_col = LABEL) # plot is not helpful
  )

  expect_false(
    inherits(try(ggplot_build(res3$SummaryPlotList$CENTER_0)), "try-error"))

  # binary response, with group_var
  expect_message2(
    res4 <-
      util_acc_loess_bin(resp_vars = "ASTHMA_0", group_vars = "CENTER_0",
                    time_vars = "EXAM_DT_0",
                    study_data = study_data, meta_data = meta_data,
                    label_col = LABEL)
  )

  expect_false(
    inherits(try(ggplot_build(res4$SummaryPlotList$ASTHMA_0)), "try-error"))
})
