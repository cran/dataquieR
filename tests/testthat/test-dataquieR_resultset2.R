test_that("dataquieR_resultset2 class", {
  skip_on_cran() # slow, parallel, ...
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  # db_dir <- "/tmp/asdfasdf"
  # unlink(db_dir, recursive = T, force = T)
  db_dir <-  withr::local_tempdir()
  db_dir2 <-  withr::local_tempdir()

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"), 100)
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table")

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL
  md1 <- md0
  md1$LABEL <- c("CENTER_0",
                 "",
                 "CENTER_0 DUPLICATE", # will become a duplicated label
                 "CENTER_0", # direct duplication of the first label
                 "Have you been physically vigorously active in the past 12 hours ('physically vigorously active' means at least 30 minutes of jogging or fast cycling, digging up your garden, carrying heavy objects weighing more than 10 kg for a long time, or similar physical activities)?", # very long label
                 "Hybpvaitp1hpvamal3mojofcduygchowmt1kfaltospa") # matches the very long label after abbreviation
  md1$VAR_NAMES[2] <- "yOvCzPY60JRjmrYb16Tsd6qMymal4B5Skw9rZ5PHSCtaBqOVglAKcguPkQhakampFJcC8xqLbZJs7kZUdKH804pbOmM5ORPVabrkEkVkiWbakWiixZ99NRYF6BP8SRxzNYY2tED7DjmhMUwk0t674RjH828jq9zoTJgDxYP6nEdHBxhmXJh0ClCPjGsi1q" # very long variable name that should get caught and not be used as label as it is
  colnames(sd0)[2] <- md1$VAR_NAMES[2]

  sf <- prep_create_storr_factory(namespace = "Test",
                            db_dir = db_dir)

  suppressWarningsMatching(
    report <- dq_report2(sd0, md1,
                         storr_factory =
                           sf,
                         label_col = VAR_NAMES,
                         cores = NULL,
                         dimensions =
                           c("Integrity",
                             "Consistency")),
    "(Some variables have labels with more than 30 characters in.+|Lost 16.7% of the study data because of missing/not assignable metadata|Need.+VAR_NAMES.+discard.+|.+dummy names)"
  )
  report0 <- report

  testidx <- head(which(names(report) == "des_summary_categorical.v00000"), 1)
  expect_length(testidx, 1)

  x0 <- force(report0[[testidx]]$SummaryTable)
  expect_s3_class(report0[["con_limit_deviations.v00004"]], "master_result")
  expect_error(report0[["con_limit_deviations.v0000xx4"]],
               regexp = "element not found")


  suppressWarningsMatching({
    report[[1]] <- report[[1]]
    report[["con_limit_deviations.v00004"]] <-
      report[["con_limit_deviations.v00004"]]

    expect_error(
      report[["con_limit_deviations.v0000xx4"]] <- NULL,
      regexp = "element not found")
  }, regexps = ".*inefficient.*")

  x1 <- force(report[[testidx]]$SummaryTable)
  # save(x0, x1, report, file = "/tmp/xx.RData")

  expect_s3_class(report[["con_limit_deviations.v00004"]], "master_result")
  expect_error(report[["con_limit_deviations.v0000xx4"]],
               regexp = "element not found")

  report0 <- prep_set_backend(report, NULL)
  report2 <- prep_set_backend(report0,
                              prep_create_storr_factory(namespace = "Test",
                                                        db_dir = db_dir2))
  report3 <- prep_load_report_from_backend(
      namespace = "Test",
      db_dir = db_dir2
    )

  expect_equal(
    report0$`int_all_datastructure_dataframe.[ALL]`,
    report$`int_all_datastructure_dataframe.[ALL]`
  )

  expect_equal(
    report2$`int_all_datastructure_dataframe.[ALL]`,
    report$`int_all_datastructure_dataframe.[ALL]`
  )

  expect_equal(
    report3$`int_all_datastructure_dataframe.[ALL]`,
    report$`int_all_datastructure_dataframe.[ALL]`
  )

  # testthat::skip_if(identical(Sys.getenv("R_COVR"), "true"),
  #                   message = "Does not work, if instrumented")


  expect_s3_class(x0, "TableSlot")
  expect_s3_class(x1, "TableSlot")

})

test_that("storr helper functions", {
  sf1 <- prep_create_storr_factory()
  so1 <- sf1()
  sf2 <- prep_create_storr_factory(namespace = "Test")
  so2 <- sf2()
  expect_equal(util_get_storr_att_namespace(so1), "objects.attributes")
  expect_equal(util_get_storr_att_namespace(so2), "Test.attributes")
  expect_equal(util_get_storr_summ_namespace(so1), "objects.summary")
  expect_equal(util_get_storr_summ_namespace(so2), "Test.summary")
})

