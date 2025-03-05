test_that("dq_report_by works with content m", {
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
    skip_on_cran() # slow test
    # TODO: test with all sorts of _by calls and on windows
    target <- withr::local_tempdir("testdqareportby")

    study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"), 100)


    #Added a new grading ruleset
    default_grading_ruleset <-
      prep_get_data_frame(system.file("grading_rulesets.xlsx",
                                      package = "dataquieR"))

    default_grading_ruleset[default_grading_ruleset$indicator_metric ==
                              "PCT_con_rvv_icat",] <- c("0",
                                                        "PCT_con_rvv_icat",
                                                        NA,
                                                        NA,
                                                        NA,
                                                        "[0; 0]",
                                                        "(0; 100]")

    prep_add_data_frames(grading_rulesets = default_grading_ruleset)

    expect_message(dq_report_by(study_data = study_data,
                 dimensions = "int",
                 cores = NULL,
                 segment_column = STUDY_SEGMENT,
                 segment_select = c("STUDY", "LAB"),
                 meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                 output_dir = file.path(target, "m"),
                 also_print = TRUE))

    # TODO: check target path for all expected artifacts, namely the dq2 files, the .output_dirs containing the HTML reports and the summary RDS files

})
