test_that("util_plot_categorical_vars works", {
  skip_on_cran() # slow
  skip_if_translated()
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }

  expect_message(
    t1 <- util_plot_categorical_vars(resp_vars = "v00103",
                                 group_vars = "v00012",
                                 time_vars = "v00013",
                                 study_data = study_data, meta_data = meta_data,
                                 n_cat_max = 10,
                                 n_group_max = 5,
                                 n_data_min = 50),
    "Missing some or all entries"
  )

  expect_message(
    t2 <- util_plot_categorical_vars(resp_vars = "v00103",
                               time_vars = "v00013",
                               study_data = study_data, meta_data = meta_data,
                               n_cat_max = 10,
                               n_data_min = 50),
    "Missing some or all entries"
  )

  expect_message(
    t3 <- util_plot_categorical_vars(resp_vars = "v00103",
                                 group_vars = "v00012",
                                 study_data = study_data, meta_data = meta_data,
                                 n_cat_max = 10,
                                 n_group_max = 5),
    "Missing some or all entries"
  )

  expect_message(
    t4 <- util_plot_categorical_vars(resp_vars = "v00103",
                                 study_data = study_data, meta_data = meta_data,
                                 n_cat_max = 10),
    "Missing some or all entries"
  )
})

test_that("util_plot_categorical_vars works for nominal and ordinal variables", {
  skip_on_cran() # slow
  skip_if_translated()
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }

  meta_data[[SCALE_LEVEL]][meta_data[["VAR_NAMES"]] == "v00008"] <-
    SCALE_LEVELS$NOMINAL
  expect_message(
    t5 <- util_plot_categorical_vars(resp_vars = "v00008",
                                     group_vars = "v00011",
                                     study_data = study_data, meta_data = meta_data,
                                     n_cat_max = 10,
                                     n_group_max = 5),
    "Missing some or all entries"
  )

  meta_data[[SCALE_LEVEL]][meta_data[["VAR_NAMES"]] == "v00008"] <-
    SCALE_LEVELS$ORDINAL
  expect_message(
    t6 <- util_plot_categorical_vars(resp_vars = "v00008",
                                     group_vars = "v00011",
                                     study_data = study_data, meta_data = meta_data,
                                     n_cat_max = 10,
                                     n_group_max = 5),
    "Missing some or all entries"
  )

})
