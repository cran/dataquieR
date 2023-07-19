test_that("con_inadmissible_categorical works", {
  skip_on_cran() # slow
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  expect_message(
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

  expect_silent(suppressWarnings({
    IAVCatAll <- con_inadmissible_categorical(study_data = study_data,
                                              meta_data  = meta_data,
                                              label_col  = "LABEL",
                                              resp_vars =
                                                c("MARRIED_0",
                                                  "SMOKING_0",
                                                  "PREGNANT_0"))

    IAVCatAll <- con_inadmissible_categorical(study_data = study_data,
                                              meta_data  = meta_data,
                                              label_col  = "LABEL",
                                              resp_vars = c("MARRIED_0"))
  }))

})
