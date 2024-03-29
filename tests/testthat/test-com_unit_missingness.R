test_that("com_unit_missingness works", {
  skip_on_cran()
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  r <- com_unit_missingness(study_data, meta_data, label_col = LABEL,
                       id_vars = "PSEUDO_ID", strata_vars = "CENTER_0")
  expect_equal(
    length(intersect(
      names(r),
      c("FlaggedStudyData", "SummaryData")
    )), length(union(
      names(r),
      c("FlaggedStudyData", "SummaryData")
    ))
  )
  expect_equal(r$SummaryData,
               structure(
                 list(
                   CENTER_0 = c("Berlin",
                                "Hamburg",
                                "Leipzig",
                                "Cologne",
                                "Munich"),
                   N_OBS = c(617L, 581L, 593L, 564L, 585L),
                   N_UNIT_MISSINGS = c(15L,  11L, 9L, 13L, 12L),
                   "N_UNIT_MISSINGS_(%)" = c(2.43, 1.89, 1.52,  2.3, 2.05)
                 ),
                 row.names = c(NA,-5L),
                 class = "data.frame"
               ))
  expect_equal(sum(r$FlaggedStudyData$Unit_missing == 1), 60)
  expect_equal(unique(r$FlaggedStudyData$Unit_missing), 0:1)
})
