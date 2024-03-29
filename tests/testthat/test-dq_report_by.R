test_that("dq_report_by works with content", {
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
    skip_on_cran() # slow test
    print("TODO: test with all sorts of _by calls and on windows")
    target <- withr::local_tempdir("testdqareportby")


    expect_message(dq_report_by("study_data",
                 dimensions = "int",
                 cores = 1,
                 meta_data_split = STUDY_SEGMENT,
                 study_data_split = "CENTER_0",
                 meta_data_v2 = "meta_data_v2",
                 output_dir = !!file.path(target, "sm"),
                 also_print = TRUE))

    expect_message(dq_report_by("study_data",
                 dimensions = "int",
                 cores = 1,
                 meta_data_split = NULL,
                 study_data_split = "CENTER_0",
                 meta_data_v2 = "meta_data_v2",
                 output_dir = !!file.path(target, "s"),
                 also_print = TRUE))

    expect_message(dq_report_by("study_data",
                 dimensions = "int",
                 cores = 1,
                 meta_data_split = STUDY_SEGMENT,
                 meta_data_v2 = "meta_data_v2",
                 output_dir = !!file.path(target, "m"),
                 also_print = TRUE))


    expect_message(dq_report_by("study_data",
                 dimensions = "int",
                 cores = 1,
                 meta_data_split = NULL,
                 meta_data_v2 = "meta_data_v2",
                 output_dir = !!file.path(target, "na"),
                 also_print = TRUE))

    # TODO: check target path for all expected artifacts, namely the dq2 files, the .output_dirs containing the HTML reports and the summary RDS files

})
