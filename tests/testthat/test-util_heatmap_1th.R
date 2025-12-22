test_that("util_heatmap_1th works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran() # will be remoed, currently used only by segment missingness which is also tested
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  label_col <- LABEL
  prep_prepare_dataframes(.internal = TRUE)
  hm1 <- util_heatmap_1th(
    df = ds1,
    cat_vars = c("CENTER_0", "USR_BP_0"),
    values = "SBP_0",
    threshold = 100,
    invert = FALSE
  )
  expect_lt(
    abs(suppressWarnings(
      sum(as.numeric(as.matrix(hm1$SummaryPlot$data)), na.rm = TRUE)
    ) - 1134545),
    0.8
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  hm2 <- util_heatmap_1th(
    df = ds1,
    cat_vars = c("USR_BP_0"),
    values = "SBP_0",
    threshold = 100,
    invert = TRUE,
    strata = "CENTER_0"
  )
  hm3 <- util_heatmap_1th(
    df = ds1,
    cat_vars = c("USR_BP_0"),
    values = "SBP_0",
    threshold = 100,
    invert = TRUE,
    strata = "CENTER_0",
    right_intv = TRUE
  )
  suppressWarnings({
    expect_doppelganger2("util_heatmap_1th_1",
                                hm1$SummaryPlot)
    expect_doppelganger2("util_heatmap_1th_2",
                                hm2$SummaryPlot)
    expect_doppelganger2("util_heatmap_1th_3",
                                hm3$SummaryPlot)
  })
})
