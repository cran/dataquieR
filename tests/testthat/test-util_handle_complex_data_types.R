test_that("complex_data_types", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                    keep_types = TRUE)

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  meta_data <- prep_get_data_frame("item_level")

  meta_data[[DATA_TYPE]][meta_data[[LABEL]] == "N_CHILD_0"] <- "cOuNt"
  meta_data[[SCALE_LEVEL]][meta_data[[LABEL]] == "N_CHILD_0"] <- ""

  # dq_report2 ----

  r <- dq_report2(
    resp_vars = "N_CHILD_0",
    study_data = study_data, label_col = LABEL, meta_data = meta_data,
    dimensions = NULL,
    cores = NULL,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  repsum <- summary(r)
  expect_snapshot(
    attr(r, "meta_data")[attr(r, "meta_data")[[LABEL]] == "N_CHILD_0", , FALSE]
  )

  expect_snapshot(repsum)

  # Call one function, only ----
  r <- acc_distributions(
    resp_vars = "N_CHILD_0",
    study_data = study_data, label_col = LABEL, meta_data = meta_data,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  skip_if_not_installed("vdiffr")
  expect_doppelganger2("EXTENDED_DATA_TYPE cnt dist",
                       r$SummaryPlotList$N_CHILD_0)

  # use pre-filled columns, such as SCALE_LEVEL, EXTENDED_DATA_TYPE, ... ----

  meta_data[[HARD_LIMITS]][meta_data[[LABEL]] == "N_CHILD_0"] <-
    "[0; 7]"
  r <- acc_distributions(
    resp_vars = "N_CHILD_0",
    study_data = study_data, label_col = LABEL, meta_data = meta_data,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  skip_if_not_installed("vdiffr")
  expect_doppelganger2("EXTENDED_DATA_TYPE cnt dist2",
                       r$SummaryPlotList$N_CHILD_0)

})
