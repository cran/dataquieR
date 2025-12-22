test_that("util_check_group_levels works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  f <- function(study_data, meta_data = "item_level", label_col) {
    ds1 <- prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .internal = TRUE)

    dim(util_check_group_levels(ds1, "CENTER_0"))

    expect_equal(
      nrow(util_check_group_levels(ds1, "USR_BP_0", min_obs_in_subgroup = 400)),
      448)
  }
  environment(f) <- environment(acc_margins)
  expect_message2(f(
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
    label_col = LABEL
  ))
})
