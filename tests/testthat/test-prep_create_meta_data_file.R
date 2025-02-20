test_that("multiplication works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  prep_create_meta_data_file("Test.xlsx",
                             cars,
                             open = FALSE,
                             overwrite = TRUE)
  expect_gt(file.size("Test.xlsx"), 1000)
  md0 <- prep_get_data_frame("Test.xlsx|item_level")
  expect_snapshot(md0)
})
