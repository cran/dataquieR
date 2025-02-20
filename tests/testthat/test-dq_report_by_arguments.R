test_that("dq_report_by error_arguments_test", {
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  target <- withr::local_tempdir("testdqareportby")

  sd0 <- head(prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    20)
  sd0 <- sd0[, head(colnames(sd0), 10)]

  md0 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

  md0 <- md0[md0$VAR_NAMES %in% colnames(sd0), ]
  # fewer segments to speed up things.
  # can be solved in a more elegant way after
  # https://gitlab.com/libreumg/dataquier/-/issues/481

  md0$STUDY_SEGMENT <- "STUDY"


  expect_warning(dq_report_by(study_data = sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            subgroup = "[v00003] == 106",
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm"),
                            also_print = TRUE),
                 regexp = "No data available to create the report.+")

  expect_warning(dq_report_by(study_data = sd0,
                              meta_data = md0,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              subgroup = "[v00003] > 1",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "sm1"),
                              also_print = TRUE),
               regexp = "The number of cases did not change after applying the subgroup.+")

  expect_error(dq_report_by(sd0,
                              meta_data = md0,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              strata_select = "2",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "sm2"),
                              also_print = TRUE),
               "strata_column is needed for selecting the strata", fixed = TRUE)


  expect_message(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            strata_column = "v00002",
                            strata_select = "^ma",
                            selection_type = "regex",
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm3"),
                            also_print = TRUE))

  #Label_col and strata not matching
  expect_error(dq_report_by(sd0,
                              meta_data = md0,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              label_col = VAR_NAMES,
                              strata_column = "SEX_0",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "sm4"),
                              also_print = TRUE),
               regexp = "The strata_column provided does not.+item_level_metadata")

  #no existing strata matches the regex
  expect_error(dq_report_by(sd0,
                              meta_data = md0,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              strata_column = "v00002",
                              strata_select = "^hello",
                              selection_type = "regex",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "sm5"),
                              also_print = TRUE),
               regexp = "No value or value label.+strata.+select")

  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            strata_exclude = "1",
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm6"),
                            also_print = TRUE),
               "strata_column is needed for excluding the strata", fixed = TRUE)

  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            selection_type = "value",
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm7"),
                            also_print = TRUE),
               paste0("selection_type can only be specified if strata_select",
                      " or strata_exclude are present"), fixed = TRUE)

  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            strata_column = "SEX_0",
                            strata_exclude = "1",
                            selection_type = "values",
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm8"),
                            also_print = TRUE),
               paste0('Internal error: The selection_type can only',
                      ' be "value", "v_label", or "regex"'), fixed = TRUE)

  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            subgroup = c("[AGE_0]>30", "[SBP_0]>100"),
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm9"),
                            also_print = TRUE),
               paste0("Need exactly one element in argument subgroup, ",
                      "got 2: [[AGE_0]>30, [SBP_0]>100]"),
               fixed = TRUE)

  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            input_dir = "test/error",
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm10"),
                            also_print = TRUE),
               regexp = ".+test/error.+does not exist. Provide an.+input_dir.+containing the study data")

  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            id_vars = "non-existing",
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm11"),
                            also_print = TRUE),
               regexp = "The id_vars.+non-existing.+is not present in the item_level metadata")


  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm12"),
                            strata_column = "SEX_0",
                            strata_exclude = "female", selection_type = "v_label",
                            also_print = TRUE),
               regexp = "Internal error: No value label.+strata_exclude")

  expect_message(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm13"),
                            strata_column = "SEX_0",
                            strata_exclude = "females", selection_type = "v_label",
                            also_print = TRUE))


  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm14"),
                            strata_column = "SEX_0",
                            strata_exclude = "^femm", selection_type = "regex",
                            also_print = TRUE),
               regexp = "Internal error: No value or value label.+strata_exclude")


  expect_warning(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm15"),
                            strata_column = "SEX_0",
                            strata_exclude = "^fem",
                            also_print = TRUE),
                 regexp = "The argument selection_type not provided.+expressions")


  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm16"),
                            strata_column = "SEX_0", selection_type = "value",
                            strata_exclude = c("0", "1"),
                            also_print = TRUE),
               regexp = "Internal error: No strata remain.+ones")


  expect_error(dq_report_by(sd0,
                            meta_data = md0,
                            dimensions = "int",
                            label_col = "VAR_NAMES",
                            cores = NULL,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm17"),
                            strata_column = "SEX_0",
                            also_print = TRUE),
               regexp = "The strata_column provided does not correpond.+item_level_metadata")

  #remove columns VAR_NAMES from metadata
  md1 <-  md0[ , !(names(md0) == VAR_NAMES)]
  expect_error(dq_report_by(sd0,
                            meta_data = md1,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm18"),
                            also_print = TRUE),
               regexp = "Did not find the mandatory column.+VAR_NAMES.+in the.+meta_data.+")

  #remove columns STUDY_SEGMENT from metadata
  md2 <-  md0[ , !(names(md0) == STUDY_SEGMENT)]
  expect_error(dq_report_by(sd0,
                            meta_data = md2,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = STUDY_SEGMENT,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm19"),
                            also_print = TRUE),
               regexp = "No metadata attribute.+STUDY_SEGMENT.+found for segmenting DQ report.")

  expect_error(dq_report_by(sd0,
                            meta_data = md2,
                            dimensions = "int",
                            cores = NULL,
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm20"),
                            also_print = TRUE),
               regexp = "No information for split provided.+segment_column = NULL.+")


  expect_error(dq_report_by(sd0,
                            meta_data = md2,
                            dimensions = "int",
                            cores = NULL,
                            strata_column = "sex_two",
                            meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            output_dir = !!file.path(target, "sm21"),
                            also_print = TRUE),
               regexp = "The strata_column provided does not correpond.+item_level_metadata")

  expect_message(dq_report_by(study_data = c("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS",
                                             "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
                              meta_data_v2 ="https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "comb1"),
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
                              id_vars = "v00001"))


  sd1 <- head(prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    20)

  expect_message(dq_report_by(resp_vars = c("v00000", "v00001", "v00002",
                                            "v00003", "v00004", "v00005",
                                            "v00006", "v00018"),
                              study_data = sd1,
                              cores = NULL,
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "comb2"),
                              also_print=TRUE,
                              segment_column = STUDY_SEGMENT,
                              segment_select = "^STU",
                              dimensions = "Integrity"))


  sd3 <- head(prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    20)
  sd3 <- sd3[, head(colnames(sd3), 10)]

  md3 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

  md3 <- md3[md3$VAR_NAMES %in% colnames(sd3), ]

  md3$STUDY_SEGMENT <- "STUDY"
  md3$DATAFRAMES <- "my_sd"

  df3 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | dataframe_level")

  df3$DF_ID_REF_TABLE <- NA
  df3$DF_RECORD_CHECK <- NA
  df3$DF_UNIQUE_ID <- NA
  df3$DF_CODE <- "my_sd"

  segl3 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | segment_level")

  segl3$SEGMENT_ID_TABLE <- NA
  segl3$SEGMENT_ID_VARS <- NA
  segl3$SEGMENT_RECORD_CHECK<- NA
  segl3$SEGMENT_UNIQUE_ROWS <- NA
  md3$MISSING_LIST_TABLE <- NA

  expect_message(dq_report_by(sd3,
                              meta_data = md3,
                              meta_data_dataframe = df3,
                              meta_data_segment = segl3,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              segment_select = "^ST",
                              output_dir = !!file.path(target, "comb3"),
                              also_print = TRUE))



df3$DF_NAME <- "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"
expect_message(dq_report_by(meta_data = md3,
                            meta_data_dataframe = df3,
                            cores = NULL,
                            segment_select = "STUDY",
                            output_dir = !!file.path(target, "comb4"),
                            also_print = TRUE ))
df3 <- rbind(df3,
             data.frame(DF_ELEMENT_COUNT = NA,
                        DF_RECORD_COUNT = NA,
                        DF_ID_REF_TABLE = NA,
                        DF_RECORD_CHECK = NA,
                        DF_UNIQUE_ID= NA,
                        DF_ID_VARS = "id",
                        DF_UNIQUE_ROWS = NA,
                        DF_CODE = "ship",
                        DF_NAME = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS"))


expect_message(dq_report_by(meta_data = md3,
                            meta_data_dataframe = df3,
                            cores = NULL,
                            segment_select = "STUDY",
                            output_dir = !!file.path(target, "comb5"),
                            also_print = TRUE ))

md3$REPORT_NAME <- c("A", "B")
expect_message(dq_report_by(sd3,
                            meta_data = md3,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = "REPORT_NAME",
                            segment_select = "A",
                            output_dir = !!file.path(target, "comb6"),
                            also_print = TRUE))

expect_error(dq_report_by(sd3,
                            meta_data = md3,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = "REPORTT_NAME",
                            output_dir = !!file.path(target, "comb7"),
                            also_print = TRUE),
             regexp = "No metadata.+for segmenting DQ report.")

expect_error(dq_report_by(sd3,
                          meta_data = md3,
                          dimensions = "int",
                          cores = NULL,
                          strata_column = "non-existent_strata",
                          output_dir = !!file.path(target, "comb8"),
                          also_print = TRUE),
             regexp = "The strata_column provided.+in the item_level_metadata")


md3 <- md3[, !colnames(md3) %in% "DATAFRAMES"]
df3 <- df3[, !colnames(df3) %in% "DF_CODE"]
expect_warning(dq_report_by(meta_data = md3,
                            meta_data_dataframe = df3,
                            cores = NULL,
                            segment_select = "STUDY",
                            output_dir = !!file.path(target, "comb9"),
                            also_print = TRUE ),
               regexp = "Because no id variable is available.+duplicated rows.")

#Without LABEL and VARIABLE_ROLE in metadata
sd1 <- head(prep_get_data_frame(
  "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
  20)
sd1 <- sd1[, head(colnames(sd1), 10)]

md1 <- prep_get_data_frame(
  "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

md1 <- md1[md1$VAR_NAMES %in% colnames(sd1), ]
md1 <- md1[, !colnames(md1) %in% c("LABEL", "VARIABLE_ROLE")]
md1$MISSING_LIST_TABLE <- NA

expect_message(dq_report_by(study_data = sd1,
                            meta_data = md1,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = NULL,
                            output_dir = !!file.path(target, "test1"),
                            also_print = TRUE))

#only segment_select, but from STUDY_SEGMENT
expect_message(dq_report_by(study_data = sd1,
                            meta_data = md1,
                            dimensions = "int",
                            cores = NULL,
                            segment_select = "PHYS_EXAM",
                            output_dir = !!file.path(target, "test2"),
                            also_print = TRUE))

#only segment_select but not present in STUDY_SEGMENT
expect_error(dq_report_by(study_data = sd1,
                          meta_data = md1,
                          dimensions = "int",
                          cores = NULL,
                          segment_select = "THIS_EXAM",
                          output_dir = !!file.path(target, "test3"),
                          also_print = TRUE),
             regexp = "segment_select values are.+not assigned.")


#only segment_exclude, but from STUDY_SEGMENT
expect_message(dq_report_by(study_data = sd1,
                            meta_data = md1,
                            dimensions = "int",
                            cores = NULL,
                            segment_exclude = "PHYS_EXAM",
                            output_dir = !!file.path(target, "test4"),
                            also_print = TRUE))

#remove all segments with segment_exclude
expect_error(dq_report_by(study_data = sd1,
                            meta_data = md1,
                            dimensions = "int",
                            cores = NULL,
                            segment_column = "STUDY_SEGMENT",
                            segment_exclude = c("PHYS_EXAM", "STUDY"),
                            output_dir = !!file.path(target, "test5"),
                            also_print = TRUE),
             regexp = "No segment.+column level left after removing unwanted segments.+")

#only segment_exclude but not present in STUDY_SEGMENT
expect_error(dq_report_by(study_data = sd1,
                          meta_data = md1,
                          dimensions = "int",
                          cores = NULL,
                          segment_exclude = "THIS_EXAM",
                          output_dir = !!file.path(target, "test6"),
                          also_print = TRUE),
             regexp = "segment_exclude values.+not assigned.")


#no STUDY_SEGMENT and no segment_select provided
#Without LABEL and VARIABLE_ROLE in metadata
md2 <- prep_get_data_frame(
  "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

md2 <- md2[md2$VAR_NAMES %in% colnames(sd1), ]
md2 <- md2[, !colnames(md2) %in% "STUDY_SEGMENT"]
md2$MISSING_LIST_TABLE <- NA

expect_error(dq_report_by(study_data = sd1,
                          meta_data = md2,
                          dimensions = "int",
                          cores = NULL,
                          output_dir = !!file.path(target, "test7"),
                          also_print = TRUE),
             regexp = "No information for split provided..+segment_column = NULL.+")

})
