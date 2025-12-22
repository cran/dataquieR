test_that("util_adjust_data_type2==util_adjust_data_type2", {
  skip_on_cran() # depends on locales

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  require_english_locale_and_berlin_tz()
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  ds1_old <- withr::with_options(
    list(dataquieR.old_type_adjust = "TRUE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )
  ds1_new <- withr::with_options(
    list(dataquieR.old_type_adjust = "FALSE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )

  expect_equal(ds1_new, ds1_old)

})

test_that("util_adjust_data_type2==util_adjust_data_type2_FCT", {
  skip_on_cran() # depends on locales

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  require_english_locale_and_berlin_tz()
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  study_data$v00014 <- as.factor(study_data$v00014)

  ds1_old <- withr::with_options(
    list(dataquieR.old_type_adjust = "TRUE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )
  ds1_new <- withr::with_options(
    list(dataquieR.old_type_adjust = "FALSE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )

  expect_equal(ds1_new, ds1_old)

})
