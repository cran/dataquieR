test_that("con_inadmissible_categorical works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  {
    IAVCatAll <- con_inadmissible_categorical(study_data = study_data,
                                              meta_data  = meta_data,
                                              label_col  = "LABEL")
  } %>% expect_message(
    regexp = paste("All variables with VALUE_LABELS.+",
                           "in the metadata are used."),
    perl = TRUE
  ) %>% expect_message(
    regexp = paste("The following variable.s.: .+_IAV.+flag.s.",
                           "inadmissible values."),
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
