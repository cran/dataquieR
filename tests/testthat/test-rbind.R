test_that("rbind.ReportSummaryTable works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")

  md0 <- meta_data

  expect_true(all(dim(rbind.ReportSummaryTable()) == 0))

  empty <- data.frame()
  expect_error(rbind.ReportSummaryTable(empty),
               regexp = "Can only bind ReportSummaryTables")
  class(empty) <- union("ReportSummaryTable", class(empty))

  expect_true(all(dim(rbind.ReportSummaryTable(empty)) == 0))

  im1 <- suppressMessages(suppressWarnings(com_item_missingness(
    resp_vars = "SBP_0",
                                               study_data, md0,
                              label_col = LABEL,
                              suppressWarnings = TRUE,
                              threshold_value = 100,
                              include_sysmiss = FALSE,
                              show_causes = TRUE,
                              drop_levels = TRUE,
                              assume_consistent_codes = TRUE,
                              expand_codes = TRUE)))

  expect_equal(
    rbind.ReportSummaryTable(im1$ReportSummaryTable),
    structure(
      list(
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
        Variables = structure("SBP_0", label_col =
                                "LABEL"),
        N = 2940L
      ),
      row.names = c(NA,-1L),
      class = c("ReportSummaryTable", "data.frame"),
      VAR_NAMES = setNames("v00004", "SBP_0")
    )
  )

  im2 <- suppressWarnings(suppressMessages(com_item_missingness(resp_vars = "DBP_0", study_data, md0,
                              label_col = LABEL,
                              suppressWarnings = TRUE,
                              threshold_value = 100,
                              include_sysmiss = FALSE,
                              show_causes = TRUE,
                              drop_levels = TRUE,
                              assume_consistent_codes = TRUE,
                              expand_codes = TRUE)))

  expect_equal(
    rbind.ReportSummaryTable(im2$ReportSummaryTable),
    structure(
      list(
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
        Variables = structure("DBP_0", label_col =
                                "LABEL"),
        N = 2940L
      ),
      row.names = c(NA,-1L),
      class = c("ReportSummaryTable", "data.frame"),
      VAR_NAMES = setNames("v00005", "DBP_0")
    )
  )

  im3 <- suppressWarnings(suppressMessages(
    com_item_missingness(resp_vars = c("AGE_0", "AGE_1"), study_data, md0,
                              label_col = LABEL,
                              suppressWarnings = TRUE,
                              threshold_value = 100,
                              include_sysmiss = FALSE,
                              show_causes = TRUE,
                              drop_levels = TRUE,
                              assume_consistent_codes = TRUE,
                              expand_codes = TRUE)))
  expect_equal(
    rbind.ReportSummaryTable(im3$ReportSummaryTable),
    structure(
      list(
        Variables = structure(c("AGE_0", "AGE_1"), label_col =
                                "LABEL"),
        N = c(2940L,  2940L)
      ),
      row.names = c(NA,-2L),
      class = c("ReportSummaryTable",  "data.frame"),
      VAR_NAMES = setNames(c("v00003", "v01003"), c("AGE_0", "AGE_1"))
    )
  )

  expect_equal(rbind.ReportSummaryTable(im2$ReportSummaryTable,
                                        im3$ReportSummaryTable,
                                        im1$ReportSummaryTable),
               structure(
                 list(
                   "MISSING 99980" = c(3, 0, 0, 1),
                   "MISSING 99981" = c(3,  0, 0, 5),
                   "MISSING 99982" = c(4, 0, 0, 3),
                   "MISSING 99983" = c(6,  0, 0, 4),
                   "MISSING 99984" = c(7, 0, 0, 4),
                   "MISSING 99985" = c(12,  0, 0, 3),
                   "MISSING 99986" = c(5, 0, 0, 4),
                   "MISSING 99987" = c(3,  0, 0, 6),
                   "MISSING 99988" = c(4, 0, 0, 4),
                   "MISSING 99989" = c(5,  0, 0, 4),
                   "MISSING 99990" = c(85, 0, 0, 82),
                   "MISSING 99991" = c(10,  0, 0, 6),
                   "MISSING 99992" = c(5, 0, 0, 2),
                   "MISSING 99993" = c(4,  0, 0, 2),
                   "MISSING 99994" = c(3, 0, 0, 9),
                   "MISSING 99995" = c(4,  0, 0, 1),
                   Variables = structure(c("DBP_0", "AGE_0", "AGE_1", "SBP_0"),
                                         label_col = "LABEL"),
                   N = c(2940L, 2940L, 2940L, 2940L)
                 ),
                 row.names = c(NA,-4L),
                 class = c("ReportSummaryTable", "data.frame"),
                 VAR_NAMES = setNames(c("v00005", "v00003", "v01003", "v00004"),
                                      c("DBP_0", "AGE_0", "AGE_1", "SBP_0"))
               )
  )

})
