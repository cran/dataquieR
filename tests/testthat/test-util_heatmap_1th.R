test_that("util_heatmap_1th works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"),
    envir = environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"),
    envir = environment())
  label_col <- LABEL
  prep_prepare_dataframes()
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
  skip_if_not(capabilities()["long.double"])
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
    vdiffr::expect_doppelganger("util_heatmap_1th_1",
                                hm1$SummaryPlot)
    vdiffr::expect_doppelganger("util_heatmap_1th_2",
                                hm2$SummaryPlot)
    vdiffr::expect_doppelganger("util_heatmap_1th_3",
                                hm3$SummaryPlot)
  })
})
