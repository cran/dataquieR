test_that("test_create_descriptive_summ", {
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  target <- withr::local_tempdir("testdessummary")

  sd1 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
              20)

  expect_message(des_summary(study_data = sd1,
                             meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx"))

  sd1 <- sd1[, 4:7]
  desc1 <- des_summary(study_data = sd1)
  expect_equal(sum(as.numeric(desc1$SummaryData$Mean)),
                298.2)

  desc2 <- des_summary(resp_vars = c("v00003", "v00004"),
                   study_data = sd1)
  expect_equal(sum(as.numeric(desc2$SummaryData$Mean)),
                171.75)
  expect_equal(sum(as.numeric(desc2$SummaryData$SD)),
               10.9318)
  expect_equal(sum(as.numeric(desc2$SummaryData$CV)),
               13.2699)
  expect_equal(sum(as.numeric(desc2$SummaryData$Kurtosis)),
               -2.5730)
  expect_equal(sum(as.numeric(desc2$SummaryData$Median)),
               170.5)

})
