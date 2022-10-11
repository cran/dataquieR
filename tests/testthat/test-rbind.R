test_that("rbind.ReportSummaryTable works", {

  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  md0 <- meta_data

  expect_true(all(dim(rbind.ReportSummaryTable()) == 0))

  empty <- data.frame()
  expect_error(rbind.ReportSummaryTable(empty),
               regexp = "Can only bind ReportSummaryTables")
  class(empty) <- union("ReportSummaryTable", class(empty))

  expect_true(all(dim(rbind.ReportSummaryTable(empty)) == 0))

  im1 <- com_item_missingness(resp_vars = "SBP_0", study_data, md0,
                              label_col = LABEL,
                              suppressWarnings = TRUE,
                              threshold_value = 100,
                              include_sysmiss = FALSE,
                              show_causes = TRUE)

  expect_equal(
    rbind.ReportSummaryTable(im1$ReportSummaryTable),
    structure(
      list(
        Variables = structure(1L, .Label = "SBP_0", class = "factor"),
        "MISSING 99980" = 1L,
        "MISSING 99981" = 5L,
        "MISSING 99982" = 3L,
        "MISSING 99983" = 4L,
        "MISSING 99984" = 4L,
        "MISSING 99985" = 3L,
        "MISSING 99986" = 4L,
        "MISSING 99987" = 6L,
        "MISSING 99988" = 4L,
        "MISSING 99989" = 4L,
        "MISSING 99990" = 82L,
        "MISSING 99991" = 6L,
        "MISSING 99992" = 2L,
        "MISSING 99993" = 2L,
        "MISSING 99994" = 9L,
        "MISSING 99995" = 1L,
        N = 2641
      ),
      row.names = c(NA,-1L),
      class = c("ReportSummaryTable",  "data.frame")
    )
  )

  im2 <- com_item_missingness(resp_vars = "DBP_0", study_data, md0,
                              label_col = LABEL,
                              suppressWarnings = TRUE,
                              threshold_value = 100,
                              include_sysmiss = FALSE,
                              show_causes = TRUE)

  expect_equal(
    rbind.ReportSummaryTable(im2$ReportSummaryTable),
    structure(
      list(
        Variables = structure(1L, .Label = "DBP_0", class = "factor"),
        "MISSING 99980" = 3L,
        "MISSING 99981" = 3L,
        "MISSING 99982" = 4L,
        "MISSING 99983" = 6L,
        "MISSING 99984" = 7L,
        "MISSING 99985" = 12L,
        "MISSING 99986" = 5L,
        "MISSING 99987" = 3L,
        "MISSING 99988" = 4L,
        "MISSING 99989" = 5L,
        "MISSING 99990" = 85L,
        "MISSING 99991" = 10L,
        "MISSING 99992" = 5L,
        "MISSING 99993" = 4L,
        "MISSING 99994" = 3L,
        "MISSING 99995" = 4L,
        N = 2647
      ),
      row.names = c(NA,-1L),
      class = c("ReportSummaryTable",  "data.frame")
    )
  )

  im3 <- com_item_missingness(resp_vars = c("AGE_0", "AGE_1"), study_data, md0,
                              label_col = LABEL,
                              suppressWarnings = TRUE,
                              threshold_value = 100,
                              include_sysmiss = FALSE,
                              show_causes = TRUE)
  expect_equal(
    rbind.ReportSummaryTable(im3$ReportSummaryTable),
    structure(list(),
              .Names = character(0),
              class = c("ReportSummaryTable",  "data.frame"),
              row.names = integer(0))
  )

  expect_equal(rbind.ReportSummaryTable(im2$ReportSummaryTable,
                                        im3$ReportSummaryTable,
                                        im1$ReportSummaryTable),
               structure(
                 list(
                   Variables = structure(1:2, .Label = c("DBP_0",  "SBP_0"), class = "factor"),
                   "MISSING 99980" = c(3L, 1L),
                   "MISSING 99981" = c(3L,  5L),
                   "MISSING 99982" = 4:3,
                   "MISSING 99983" = c(6L, 4L),
                   "MISSING 99984" = c(7L,  4L),
                   "MISSING 99985" = c(12L, 3L),
                   "MISSING 99986" = 5:4,
                   "MISSING 99987" = c(3L,  6L),
                   "MISSING 99988" = c(4L, 4L),
                   "MISSING 99989" = 5:4,
                   "MISSING 99990" = c(85L,  82L),
                   "MISSING 99991" = c(10L, 6L),
                   "MISSING 99992" = c(5L, 2L),
                   "MISSING 99993" = c(4L, 2L),
                   "MISSING 99994" = c(3L, 9L),
                   "MISSING 99995" = c(4L, 1L),
                   N = c(2647, 2641)
                 ),
                 row.names = c(NA,-2L),
                 class = c("ReportSummaryTable", "data.frame")
               ))

})
