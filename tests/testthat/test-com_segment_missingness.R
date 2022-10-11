test_that("com_segment_missingness works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                               threshold_value = NA, direction = "high",
                               exclude_roles = VARIABLE_ROLES$PROCESS),
    regexp = sprintf("%s|%s",
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%.")),
    all = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = NA, direction = "high"),
    regexp = sprintf("%s|%s|%s",
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."),
                     paste("Formal exclude_roles is used with default:",
                           "all process variables are not included here.")),
    all = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data,
                                 threshold_value = NA, direction = "high"),
    regexp = sprintf("%s|%s|%s",
                     paste("Study variables: .+v00010.+,",
                           ".+v00011.+, .+v00012.+,",
                           ".+v00013.+, .+v00016.+,",
                           ".+v00017.+, .+v00032.+,",
                           ".+v00033.+, .+v00042.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."),
                     paste("Formal exclude_roles is used with default:",
                           "all process variables are not included here.")),
    all = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = NA, direction = "high",
                                 strata_vars = "CENTER_0"),
    regexp = sprintf("%s|%s|%s",
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."),
                     paste("Formal exclude_roles is used with default:",
                           "all process variables are not included here.")),
    all = TRUE
  )

  meta_data2 <- meta_data
  meta_data2$KEY_STUDY_SEGMENT <- NULL
  expect_error(suppressWarnings(
    r <- com_segment_missingness(study_data, meta_data2,
                                 threshold_value = 10, direction = "high",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid"))
    ),
    regexp = paste("In suppressWarnings: Metadata do not contain",
                   "the column KEY_STUDY_SEGMENT"),
    perl = TRUE
  )

  meta_data2 <- meta_data
  meta_data2$LONG_LABEL <- NA
  expect_warning(
    r <- com_segment_missingness(study_data, meta_data2,
                                 threshold_value = 10, direction = "high",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid")),
    regexp = sprintf("%s|%s",
                     paste("Specified VARIABLE_ROLE.s.:",
                           ".+invalid.+ was not found in metadata, only:",
                           ".+process.+ is used."),
                     paste("Study variables: .+v00010.+, .+v00011.+,",
                           ".+v00012.+, .+v00013.+, .+v00016.+, .+v00017.+,",
                           ".+v00032.+, .+v00033.+, .+v00042.+ are not",
                           "considered due to their VARIABLE_ROLE.")),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data,
                                 threshold_value = 10, direction = "high",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid")),
    regexp = sprintf("%s|%s",
                     paste("Specified VARIABLE_ROLE.s.:",
                           ".+invalid.+ was not found in metadata, only:",
                           ".+process.+ is used."),
                     paste("Study variables: .+v00010.+, .+v00011.+,",
                           ".+v00012.+, .+v00013.+, .+v00016.+, .+v00017.+,",
                           ".+v00032.+, .+v00033.+, .+v00042.+ are not",
                           "considered due to their VARIABLE_ROLE.")),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, direction = "high",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid")),
    regexp = sprintf("%s|%s",
                     paste("Specified VARIABLE_ROLE.s.:",
                           ".+invalid.+ was not found in metadata, only:",
                           ".+process.+ is used."),
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE.")),
    perl = TRUE,
    all = TRUE
  )

  expect_error(suppressWarnings(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, direction = "invalid",
                                 exclude_roles = VARIABLE_ROLES$PROCESS)
    ),
    regexp = paste("Parameter .+direction.+ should be either .+low.+ or",
                   ".+high.+, but not .+invalid.+."),
    perl = TRUE
  )

  expect_error(suppressWarnings(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, direction = 1:2,
                                 exclude_roles = VARIABLE_ROLES$PROCESS)
    ),
    regexp = paste("Parameter .+direction.+ should be of length",
                   "1, but not 2."),
    perl = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, direction = "high",
                                 exclude_roles = VARIABLE_ROLES$PROCESS),
    regexp = paste("Study variables: .+ARM_CUFF_0.+,",
                   ".+USR_VO2_0.+, .+USR_BP_0.+,",
                   ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                   ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                   ".+INT_DT_0.+, .+QUEST_DT_0.+",
                   "are not considered due to their",
                   "VARIABLE_ROLE.")
  )
  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, direction = "low",
                                 exclude_roles = VARIABLE_ROLES$PROCESS),
    regexp = paste("Study variables: .+ARM_CUFF_0.+,",
                   ".+USR_VO2_0.+, .+USR_BP_0.+,",
                   ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                   ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                   ".+INT_DT_0.+, .+QUEST_DT_0.+",
                   "are not considered due to their",
                   "VARIABLE_ROLE.")
  )
  expect_equal(
    length(intersect(
      names(r),
      c("SummaryData", "SummaryPlot")
    )), length(union(
      names(r),
      c("SummaryData", "SummaryPlot")
    ))
  )
  expect_true(abs(suppressWarnings(sum(as.numeric(as.matrix(
    r$SummaryData)), na.rm = TRUE)) - 16027.23) < 2)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("segment missingness plot ok",
                              r$SummaryPlot)
})
