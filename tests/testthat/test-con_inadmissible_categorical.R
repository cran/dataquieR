test_that("con_inadmissible_categorical works", {
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())
  expect_warning(
    IAVCatAll <- con_inadmissible_categorical(study_data = study_data,
                                              meta_data  = meta_data,
                                              label_col  = "LABEL"),
    regexp = sprintf("(%s|%s)",
                     paste("All variables with VALUE_LABELS",
                           "in the metadata are used."),
                     paste("The following variable.s.: EDUCATION_1_IAV,",
                           "FAM_STAT_0_IAV, SMOKE_SHOP_0_IAV,",
                           "MEDICATION_0_IAV, USR_SOCDEM_0_IAV flag.s.",
                           "inadmissible values.")),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(sum(IAVCatAll$SummaryTable$GRADING), 5)
  expect_equal(sum(IAVCatAll$FlaggedStudyData$EDUCATION_1_IAV), 3)

})
