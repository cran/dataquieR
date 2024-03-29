test_that("prep_apply_coding works", {
  skip_on_cran()
  study_data <- iris
  x <- prep_study2meta(study_data = study_data,
                       convert_factors = TRUE)
  study_data$Species <- as.character(study_data$Species)
  expect_warning(
    expect_equal(
      prep_apply_coding(study_data, x$MetaData)$Species,
      x$ModifiedStudyData$Species
    ),
    regexp = "Metadata does not provide a filled column called .+JUMP_LIST"
  )
})
