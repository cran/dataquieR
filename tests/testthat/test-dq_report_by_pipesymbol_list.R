test_that("dq_report_by works with pipe list in argum", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  target <- withr::local_tempdir("testdqareportby_pipe")

  study_data <- head(
    prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE),
    10)

  study_data <- study_data[setdiff(names(study_data), c("v00035", "v00036",
                                                        "v00037","v00038",
                                                        "v00039","v00040",
                                                        "v00041"))]

  prep_load_workbook_like_file( "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  item_level <- prep_get_data_frame("item_level")
  item_level <- item_level[setdiff(names(item_level), c("HARD_LIMITS",
                                                        "DETECTION_LIMITS",
                                                        "SOFT_LIMITS",
                                                        "DISTRIBUTION",
                                                        "DECIMALS",
                                                        "DATA_ENTRY_TYPE" , "GROUP_VAR_DEVICE" ,
                                                        "GROUP_VAR_OBSERVER" ,
                                                        "ELEMENT_HOMOGENITY_CHECKTYPE",
                                                        "UNIVARIATE_OUTLIER_CHECKTYPE",
                                                        "N_RULES",
                                                        "LOCATION_METRIC",
                                                        "LOCATION_RANGE",
                                                        "PROPORTION_RANGE",
                                                        "REPEATED_MEASURES_VARS" ,
                                                        "REPEATED_MEASURES_GOLDSTANDARD",
                                                        "CO_VARS" ))]

  item_level<- item_level[!item_level$VAR_NAMES %in% c("v00035", "v00036",
                                                       "v00037","v00038",
                                                       "v00039","v00040",
                                                       "v00041"), ]


  expect_message2(dq_report_by(study_data = study_data,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              segment_select ="STUDY | LAB",
                              item_level = item_level,
                              subgroup = "[v00003]<48",
                              strata_column = "SEX_0",
                              strata_select ="0|1",
                              #meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m1"),
                              also_print = TRUE))

  prep_load_workbook_like_file( "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  study_data[is.na(study_data[, "v00008"]), "v00008"] <- "B"
  expect_warning(dq_report_by(study_data = study_data,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              segment_exclude ="LAB | INTERVIEW | QUESTIONNAIRE | PHYS_EXAM",
                              item_level = item_level,
                              strata_column = "VO2_CAPCAT_0",
                              strata_exclude ="B |C| D ",
                              selection_type = "value",
                              #meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m2"),
                              also_print = TRUE),
                 regexp = "The stratum.*strata.*D.*not present in the study data")

  prep_load_workbook_like_file( "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  expect_message2(dq_report_by(study_data = c("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS",
                                             "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
                              meta_data_v2 ="https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m5"),
                              cores = NULL,
                              also_print=TRUE,
                              segment_column = STUDY_SEGMENT,
                              segment_exclude = c("PHYS_EXAM", "LAB",
                                                  "INTERVIEW", "QUESTIONNAIRE"),
                              strata_column = "v00002",
                              strata_select = "females",
                              dimensions = "Integrity",
                              selection_type = "v_label",
                              subgroup = "[v00003]<40",
                              id_vars = "v00001|v00000"))

})
