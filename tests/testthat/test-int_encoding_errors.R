test_that("int_encoding_errors works", {
  skip_on_cran() # depends on OS code page support
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_purge_data_frame_cache()

  sd <-
    prep_get_data_frame(
  "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"
  )

  sd$v00001[[1]] <- "ä"
  skip_if(gsub("[^a-z0-9]", "", tolower(Encoding(sd$v00001[[1]]))) != "utf8",
          "Platform seems not to use UTF-8")

  # Introduce an encoding error
  Encoding(sd$v00001[[1]]) <- "latin1"

  expect_warning(
    x <- int_encoding_errors(study_data = sd),
    regexp = "neither"

  )

  expect_equal(sum(x$SummaryTable$NUM_int_uenc), 1)
  expect_equal(which(x$SummaryTable$NUM_int_uenc > 0), 1)

  prep_purge_data_frame_cache()
  prep_load_workbook_like_file(
"https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  dl <- prep_get_data_frame("dataframe_level")

  dl[[ENCODING]] <- c("utf-8")
  dl[[DF_CODE]] <- "sd"
  prep_add_data_frames(dataframe_level = dl)

  il <- prep_get_data_frame("item_level")

  il[[DATAFRAMES]] <- "sd"
  il[[ENCODING]] <- "utf-8"
  prep_add_data_frames(item_level = il)

  x <- int_encoding_errors(study_data = sd)
  expect_equal(sum(x$SummaryTable$NUM_int_uenc), 1)
  expect_equal(which(x$SummaryTable$NUM_int_uenc > 0), 1)

  prep_purge_data_frame_cache()
  prep_load_workbook_like_file(
"https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  dl <- prep_get_data_frame("dataframe_level")

  dl[[ENCODING]] <- c("utf-8")
  dl[[DF_CODE]] <- "sd"
  prep_add_data_frames(dataframe_level = dl)

  il <- prep_get_data_frame("item_level")

  il[[DATAFRAMES]] <- "sd"
  prep_add_data_frames(item_level = il)

  x <- int_encoding_errors(study_data = sd)
  expect_equal(sum(x$SummaryTable$NUM_int_uenc), 1)
  expect_equal(which(x$SummaryTable$NUM_int_uenc > 0), 1)

  prep_purge_data_frame_cache()
  prep_load_workbook_like_file(
"https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  il <- prep_get_data_frame("item_level")

  il[[ENCODING]] <- "utf-8"
  prep_add_data_frames(item_level = il)

  x <- int_encoding_errors(study_data = sd)
  expect_equal(sum(x$SummaryTable$NUM_int_uenc), 1)
  expect_equal(which(x$SummaryTable$NUM_int_uenc > 0), 1)

})

test_that("reports with encoding errors work", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_on_cran() # depends on OS code page support
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_purge_data_frame_cache()

  sd <-
    prep_get_data_frame(
  "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"
    )

  sd <- head(sd, 10)

  sd$v00012[[1]] <- "ä"
  skip_if(gsub("[^a-z0-9]", "", tolower(Encoding(sd$v00012[[1]]))) != "utf8",
          "Platform seems not to use UTF-8")

  # Introduce an encoding error
  Encoding(sd$v00012[[1]]) <- "latin1"

  r <- dq_report2(sd,
                  resp_vars = c("SBP_0", "ITEM_1_0"),
                  dimensions = NULL,
                  cores = NULL,
                  meta_data_v2 =
"https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  td <- withr::local_tempdir("testdqareportby")
  print(r, view = FALSE, dir = td)

  sz <- sum(file.size(list.files(td,
                                 recursive = TRUE,
                                 full.names = TRUE,
                                 all.files = TRUE)))
  expect_gt(sz, 13000000)

})
