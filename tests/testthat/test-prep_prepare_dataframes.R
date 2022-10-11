test_that("prep_prepare_dataframes works", {
  acc_test1 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars, study_data, meta_data) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  acc_test2 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars, study_data, meta_data, label_col) {
    ds1 <- prep_prepare_dataframes(study_data, meta_data)
    invisible(ds1)
  }
  environment(acc_test1) <- asNamespace("dataquieR")
  environment(acc_test2) <- asNamespace("dataquieR")
  acc_test3 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data,
                        label_col) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  acc_test4 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data,
                        label_col) {
    ds1 <- prep_prepare_dataframes(study_data, meta_data)
    invisible(ds1)
  }
  environment(acc_test3) <- asNamespace("dataquieR")
  environment(acc_test4) <- asNamespace("dataquieR")
  load(system.file("extdata/meta_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir = environment())

  expect_error(
    acc_test1(),
    regexp = "Need study data as a data frame"
  )
  expect_error(
    acc_test2(),
    regexp = "Need study data as a data frame"
  )
  expect_warning(
    acc_test1(study_data = study_data),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste("Missing .+meta_data.+, try to guess a preliminary one from the",
            "data using .+prep_prepare_dataframes.+. Please consider amending",
            "this minimum guess manually."),
      paste("Meta data does not provide a filled column called",
            ".+MISSING_LIST.+ for replacing codes with NAs."),
      paste("Meta data does not provide a filled column called .+JUMP_LIST.+",
            "for replacing codes with NAs.")
    ),
    perl = TRUE,
    all = TRUE
  )
  expect_error(
    acc_test1(meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )
  expect_error(
    acc_test2(study_data = 12, meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )
  expect_equal(colnames(acc_test1(study_data = study_data,
                                  meta_data = meta_data)),
               c(
                 "v00000",
                 "v00001",
                 "v00002",
                 "v00003",
                 "v00103",
                 "v01003",
                 "v01002",
                 "v10000",
                 "v00004",
                 "v00005",
                 "v00006",
                 "v00007",
                 "v00008",
                 "v00009",
                 "v00109",
                 "v00010",
                 "v00011",
                 "v00012",
                 "v00013",
                 "v20000",
                 "v00014",
                 "v00015",
                 "v00016",
                 "v00017",
                 "v30000",
                 "v00018",
                 "v01018",
                 "v00019",
                 "v00020",
                 "v00021",
                 "v00022",
                 "v00023",
                 "v00024",
                 "v00025",
                 "v00026",
                 "v00027",
                 "v00028",
                 "v00029",
                 "v00030",
                 "v00031",
                 "v00032",
                 "v00033",
                 "v40000",
                 "v00034",
                 "v00035",
                 "v00036",
                 "v00037",
                 "v00038",
                 "v00039",
                 "v00040",
                 "v00041",
                 "v00042",
                 "v50000"
               )
  )
  expect_equal(colnames(acc_test2(study_data = study_data,
                                  meta_data = meta_data)),
               c(
                 "v00000",
                 "v00001",
                 "v00002",
                 "v00003",
                 "v00103",
                 "v01003",
                 "v01002",
                 "v10000",
                 "v00004",
                 "v00005",
                 "v00006",
                 "v00007",
                 "v00008",
                 "v00009",
                 "v00109",
                 "v00010",
                 "v00011",
                 "v00012",
                 "v00013",
                 "v20000",
                 "v00014",
                 "v00015",
                 "v00016",
                 "v00017",
                 "v30000",
                 "v00018",
                 "v01018",
                 "v00019",
                 "v00020",
                 "v00021",
                 "v00022",
                 "v00023",
                 "v00024",
                 "v00025",
                 "v00026",
                 "v00027",
                 "v00028",
                 "v00029",
                 "v00030",
                 "v00031",
                 "v00032",
                 "v00033",
                 "v40000",
                 "v00034",
                 "v00035",
                 "v00036",
                 "v00037",
                 "v00038",
                 "v00039",
                 "v00040",
                 "v00041",
                 "v00042",
                 "v50000"
               )
  )
  expect_equal(colnames(acc_test3(study_data = study_data,
                                  meta_data = meta_data)),
               c(
                 "v00000",
                 "v00001",
                 "v00002",
                 "v00003",
                 "v00103",
                 "v01003",
                 "v01002",
                 "v10000",
                 "v00004",
                 "v00005",
                 "v00006",
                 "v00007",
                 "v00008",
                 "v00009",
                 "v00109",
                 "v00010",
                 "v00011",
                 "v00012",
                 "v00013",
                 "v20000",
                 "v00014",
                 "v00015",
                 "v00016",
                 "v00017",
                 "v30000",
                 "v00018",
                 "v01018",
                 "v00019",
                 "v00020",
                 "v00021",
                 "v00022",
                 "v00023",
                 "v00024",
                 "v00025",
                 "v00026",
                 "v00027",
                 "v00028",
                 "v00029",
                 "v00030",
                 "v00031",
                 "v00032",
                 "v00033",
                 "v40000",
                 "v00034",
                 "v00035",
                 "v00036",
                 "v00037",
                 "v00038",
                 "v00039",
                 "v00040",
                 "v00041",
                 "v00042",
                 "v50000"
               )
  )
  expect_equal(colnames(acc_test4(study_data = study_data,
                                  meta_data = meta_data)),
               c(
                 "v00000",
                 "v00001",
                 "v00002",
                 "v00003",
                 "v00103",
                 "v01003",
                 "v01002",
                 "v10000",
                 "v00004",
                 "v00005",
                 "v00006",
                 "v00007",
                 "v00008",
                 "v00009",
                 "v00109",
                 "v00010",
                 "v00011",
                 "v00012",
                 "v00013",
                 "v20000",
                 "v00014",
                 "v00015",
                 "v00016",
                 "v00017",
                 "v30000",
                 "v00018",
                 "v01018",
                 "v00019",
                 "v00020",
                 "v00021",
                 "v00022",
                 "v00023",
                 "v00024",
                 "v00025",
                 "v00026",
                 "v00027",
                 "v00028",
                 "v00029",
                 "v00030",
                 "v00031",
                 "v00032",
                 "v00033",
                 "v40000",
                 "v00034",
                 "v00035",
                 "v00036",
                 "v00037",
                 "v00038",
                 "v00039",
                 "v00040",
                 "v00041",
                 "v00042",
                 "v50000"
               )
  )

  expect_equal(
    colnames(
      acc_test3(
        study_data = study_data,
        meta_data = meta_data,
        label_col = "LABEL"
      )
    ),
    c(
      "CENTER_0",
      "PSEUDO_ID",
      "SEX_0",
      "AGE_0",
      "AGE_GROUP_0",
      "AGE_1",
      "SEX_1",
      "PART_STUDY",
      "SBP_0",
      "DBP_0",
      "GLOBAL_HEALTH_VAS_0",
      "ASTHMA_0",
      "VO2_CAPCAT_0",
      "ARM_CIRC_0",
      "ARM_CIRC_DISC_0",
      "ARM_CUFF_0",
      "USR_VO2_0",
      "USR_BP_0",
      "EXAM_DT_0",
      "PART_PHYS_EXAM",
      "CRP_0",
      "BSG_0",
      "DEV_NO_0",
      "LAB_DT_0",
      "PART_LAB",
      "EDUCATION_0",
      "EDUCATION_1",
      "FAM_STAT_0",
      "MARRIED_0",
      "N_CHILD_0",
      "EATING_PREFS_0",
      "MEAT_CONS_0",
      "SMOKING_0",
      "SMOKE_SHOP_0",
      "N_INJURIES_0",
      "N_BIRTH_0",
      "INCOME_GROUP_0",
      "PREGNANT_0",
      "MEDICATION_0",
      "N_ATC_CODES_0",
      "USR_SOCDEM_0",
      "INT_DT_0",
      "PART_INTERVIEW",
      "ITEM_1_0",
      "ITEM_2_0",
      "ITEM_3_0",
      "ITEM_4_0",
      "ITEM_5_0",
      "ITEM_6_0",
      "ITEM_7_0",
      "ITEM_8_0",
      "QUEST_DT_0",
      "PART_QUESTIONNAIRE"
    )
  )

  expect_equal(
    colnames(
      acc_test3(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL
      )
    ),
    c(
      "CENTER_0",
      "PSEUDO_ID",
      "SEX_0",
      "AGE_0",
      "AGE_GROUP_0",
      "AGE_1",
      "SEX_1",
      "PART_STUDY",
      "SBP_0",
      "DBP_0",
      "GLOBAL_HEALTH_VAS_0",
      "ASTHMA_0",
      "VO2_CAPCAT_0",
      "ARM_CIRC_0",
      "ARM_CIRC_DISC_0",
      "ARM_CUFF_0",
      "USR_VO2_0",
      "USR_BP_0",
      "EXAM_DT_0",
      "PART_PHYS_EXAM",
      "CRP_0",
      "BSG_0",
      "DEV_NO_0",
      "LAB_DT_0",
      "PART_LAB",
      "EDUCATION_0",
      "EDUCATION_1",
      "FAM_STAT_0",
      "MARRIED_0",
      "N_CHILD_0",
      "EATING_PREFS_0",
      "MEAT_CONS_0",
      "SMOKING_0",
      "SMOKE_SHOP_0",
      "N_INJURIES_0",
      "N_BIRTH_0",
      "INCOME_GROUP_0",
      "PREGNANT_0",
      "MEDICATION_0",
      "N_ATC_CODES_0",
      "USR_SOCDEM_0",
      "INT_DT_0",
      "PART_INTERVIEW",
      "ITEM_1_0",
      "ITEM_2_0",
      "ITEM_3_0",
      "ITEM_4_0",
      "ITEM_5_0",
      "ITEM_6_0",
      "ITEM_7_0",
      "ITEM_8_0",
      "QUEST_DT_0",
      "PART_QUESTIONNAIRE"
    )
  )

  expect_equal(
    colnames(
      acc_test4(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL
      )
    ),
    c(
      "CENTER_0",
      "PSEUDO_ID",
      "SEX_0",
      "AGE_0",
      "AGE_GROUP_0",
      "AGE_1",
      "SEX_1",
      "PART_STUDY",
      "SBP_0",
      "DBP_0",
      "GLOBAL_HEALTH_VAS_0",
      "ASTHMA_0",
      "VO2_CAPCAT_0",
      "ARM_CIRC_0",
      "ARM_CIRC_DISC_0",
      "ARM_CUFF_0",
      "USR_VO2_0",
      "USR_BP_0",
      "EXAM_DT_0",
      "PART_PHYS_EXAM",
      "CRP_0",
      "BSG_0",
      "DEV_NO_0",
      "LAB_DT_0",
      "PART_LAB",
      "EDUCATION_0",
      "EDUCATION_1",
      "FAM_STAT_0",
      "MARRIED_0",
      "N_CHILD_0",
      "EATING_PREFS_0",
      "MEAT_CONS_0",
      "SMOKING_0",
      "SMOKE_SHOP_0",
      "N_INJURIES_0",
      "N_BIRTH_0",
      "INCOME_GROUP_0",
      "PREGNANT_0",
      "MEDICATION_0",
      "N_ATC_CODES_0",
      "USR_SOCDEM_0",
      "INT_DT_0",
      "PART_INTERVIEW",
      "ITEM_1_0",
      "ITEM_2_0",
      "ITEM_3_0",
      "ITEM_4_0",
      "ITEM_5_0",
      "ITEM_6_0",
      "ITEM_7_0",
      "ITEM_8_0",
      "QUEST_DT_0",
      "PART_QUESTIONNAIRE"
    )
  )
  expect_error(
    acc_test2(study_data = NULL, meta_data = meta_data),
    regexp = " Need study data as a data frame"
  )
  expect_error(
    prep_prepare_dataframes(.replace_missings = cars),
    regexp = paste("Called prepare_dataframes with .replace_missings",
                   "not being logical(1)"),
    fixed = TRUE
  )

  load(system.file("extdata/meta_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir = environment())
  err_test1 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars, study_data, meta_data, label_col) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test1) <- asNamespace("dataquieR")

  expect_error(err_test1(label_col = testxx),
               rregexp = "Cannot resolve .+label_col = stop.+",
               perl = TRUE)
  if (exists("testxx")) rm(testxx)
  expect_error(err_test1(label_col = stop()),
               rregexp = "Cannot resolve .+label_col = testxx.+",
               perl = TRUE)


  sd99 <- study_data
  err_test2 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars, study_data = sd99,
                        meta_data, label_col) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test2) <- asNamespace("dataquieR")

  sd99 <- 42

  expect_error(err_test2(meta_data = meta_data),
               regexp = "Need study data as a data frame")

  err_test3 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars,
                        meta_data, label_col) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test3) <- asNamespace("dataquieR")
  if (exists("study_data")) rm(study_data)
  expect_error(err_test3(meta_data = meta_data),
               regexp = "Need study data as a data frame")
  load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir = environment())
  md99 <- meta_data
  err_test4 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars, study_data,
                        meta_data = md99, label_col) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test4) <- asNamespace("dataquieR")
  expect_silent(err_test4(study_data = study_data))
  md99 <- 42
  expect_error(err_test4(study_data = study_data),
               regexp = "Need meta data as a data frame")
  err_test5 <- function(resp_variable, aux_variable,
                        time_variable, co_variables,
                        group_vars, study_data,
                        meta_data, label_col) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test5) <- asNamespace("dataquieR")
  expect_warning(err_test5(study_data = study_data),
                 regexp = sprintf("(%s|%s)",
                                  paste("Missing .+meta_data.+, try to guess a",
                                        "preliminary one from the data using",
                                        ".+prep_prepare_dataframes.+. Please",
                                        "consider amending this minimum guess",
                                        "manually."),
                                  paste("Meta data does not provide a filled",
                                        "column called .+JUMP_LIST.+ for",
                                        "replacing codes with NAs.")
                                  ),
                 perl = TRUE,
                 all = TRUE)


  study_data <- tibble::as_tibble(study_data)
  meta_data <- tibble::as_tibble(meta_data)
  acc_test5 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data,
                        label_col) {
    expect_true(tibble::is_tibble(study_data))
    expect_true(tibble::is_tibble(meta_data))
    prep_prepare_dataframes()
    expect_false(tibble::is_tibble(study_data))
    expect_false(tibble::is_tibble(meta_data))
    expect_false(tibble::is_tibble(ds1))
    invisible(ds1)
  }
  environment(acc_test5) <- asNamespace("dataquieR")

  x <- acc_test5(study_data = study_data,
                 meta_data = meta_data, label_col = LABEL)
  md99 <- meta_data
  md99$LABEL <- meta_data$VAR_NAMES
  y <- acc_test5(study_data = tibble::as_tibble(x),
                 meta_data = md99, label_col = LABEL)
  expect_identical(x, y) # because this has already been mapped.
  md99$VAR_NAMES <- NULL
  expect_error(
    y <- acc_test5(study_data = study_data,
                   meta_data = md99, label_col = LABEL),
    regexp = paste(
      ".+VAR_NAMES.+ not found in meta data .LABEL, DATA_TYPE,",
      "VALUE_LABELS, MISSING_LIST, JUMP_LIST, HARD_LIMITS, DETECTION_LIMITS,",
      "CONTRADICTIONS, SOFT_LIMITS, DISTRIBUTION, DECIMALS, DATA_ENTRY_TYPE,",
      "KEY_OBSERVER, KEY_DEVICE, KEY_DATETIME, KEY_STUDY_SEGMENT,",
      "VARIABLE_ROLE, VARIABLE_ORDER, LONG_LABEL."),
    perl = TRUE
  )
  sd99 <- study_data
  md99 <- meta_data
  colnames(sd99) <- letters[1:ncol(sd99)]
  expect_warning(
    expect_error(
      acc_test3(study_data = sd99, meta_data = md99),
      regexp = "No data left. Aborting."
    ),
    regexp = sprintf("(%s|%s|%s|%s)",
                     paste("Lost 100. of the study data because of",
                           "missing.not assignable meta.data"),
                     paste("Lost 100. of the meta data because of",
                           "missing.not assignable study.data"),
                     paste("Did not find any meta data for the following",
                           "variables from the study data. .+a.+, .+b.+"),
                     paste("Found meta data for the following variables not",
                           "found in the study data: .+v00000.+, .+v00001.+,",
                           ".+v00002.+, .+v00003.+, .+v00004.+")),
    perl = TRUE,
    all = TRUE
  )
})
