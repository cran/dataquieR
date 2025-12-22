test_that("test_render_report_summary_1", {

  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  target <- withr::local_tempdir("testrendersummary")


  sd0 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), n = 20)
  sd0 <- sd0[, 1:15]

  md0 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

  md0 <- md0[md0$VAR_NAMES %in% colnames(sd0), ]

  r1<- suppressWarnings(dq_report2(study_data = sd0,
                  cores = NULL,
                  meta_data = md0,
                  dimensions = c("Completeness")))

  sumrep <- summary(r1)

  skip_if_not_installed("htmltools")
  skip_if_not_installed("DT")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("markdown")

  calnames <- print(sumrep, grouped_by = "call_names")
  indic <- print(sumrep, grouped_by = "indicator_metric")

  expect_equal(names(calnames[[6]][[2]][[3]][[1]][[1]]$data)[1:9],
               c("Variables",
                 "Labels",
                 "Descr stats-Cat",
                 "Descr stats-Cont",
                 "Data type error",
                 "Invalid encoding",
                 "Miss values-Item",
                 "Resp-rates-Item",
                 "Total" ))

  expect_equal(trimws(names(indic[[6]][[2]][[3]][[1]][[1]]$data)[1:8]),
               c("Variables",
                 "Labels",
                 "Data type mismatch (%)",
                 "Data type mismatch (cat.)",
                 "Inadmissible data format (%)",
                 "Inadmissible data format (N)",
                 "Missing values (%)",
                 "Total"))

})
