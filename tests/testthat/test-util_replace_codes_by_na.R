test_that("util_replace_codes_by_na works", {
  skip_on_cran()
  local({
    meta_data <- prep_get_data_frame("meta_data")
    study_data <- prep_get_data_frame("study_data")
    sd0 <- study_data[1:30, 30:35, FALSE]
    md0 <- meta_data[meta_data$VAR_NAMES %in% colnames(sd0), , FALSE]
    expected <- structure(list(v00024 = c(
        0, 1, 0, NA, NA,
        1, 0, 0, 1, 0, NA, 1, 1, 0, 0, 1, NA, 0, 1, 0, NA, 0, 0, 0, 1,
        0, NA, 1, 1, 0
      ), v00025 = c(
        NA, 1, NA, NA, NA, NA, NA, NA, 4,
        NA, NA, 3, 0, NA, NA, 0, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA,
        NA, 3, 3, NA
      ), v00026 = c(
        3, 7, 4, 6, NA, 11, 2, 7, 3, 8, NA,
        6, 7, 7, 4, 2, NA, NA, 3, 5, NA, 3, NA, 7, 8, 11, NA, 4, 7, 8
      ), v00027 = c(
        NA, 4, 3, NA, NA, NA, 4, NA, 3, 6, NA, NA, 2, 4,
        4, 2, NA, NA, NA, NA, NA, 2, NA, 2, NA, NA, NA, 4, NA, 2
      ), v00028 = c(
        1,
        2, 0, 2, NA, 2, 1, 3, 1, 1, NA, 2, 0, 2, 0, NA, NA, 1, 4, 1,
        NA, 1, 3, 5, 3, 3, NA, NA, 3, NA
      ), v00029 = c(
        0, 0, NA, 0, NA,
        NA, NA, NA, 0, 0, NA, NA, 0, 0, 1, 0, NA, NA, NA, NA, NA, 0,
        NA, 0, 1, NA, NA, 0, NA, 0
      )), row.names = c(NA, 30L), class = "data.frame", Codes_to_NA = TRUE)
    got <- dataquieR:::util_replace_codes_by_NA(
      study_data = sd0, meta_data = md0)
    expect_equal(got, expected)
  })
})
