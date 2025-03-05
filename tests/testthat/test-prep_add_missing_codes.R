test_that("prep_add_missing_codes works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[SEX_0]=1',
   )
   r <- prep_add_missing_codes(NA, study_data = study_data, meta_data =
   meta_data,
     label_col = "LABEL", rules = rules, use_value_labels = FALSE)
   subset(r$ModifiedMetaData, LABEL == "PREGNANT_0", JUMP_LIST)
   subset(meta_data, LABEL == "PREGNANT_0", JUMP_LIST)
   vn <- subset(r$ModifiedMetaData, LABEL == "PREGNANT_0", VAR_NAMES)[[1]]
   expect_snapshot(table(study_data[[vn]], useNA = "always"))
   expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))
   r <- prep_add_missing_codes(NA, study_data = study_data, meta_data =
     meta_data, label_col = "LABEL", rules = rules, use_value_labels = FALSE,
     overwrite = TRUE)
   expect_snapshot(table(study_data[[vn]], useNA = "always"))
   expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))

  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[SEX_0]="males"',
   )
   r <- prep_add_missing_codes(NA, study_data = study_data,
     meta_data = meta_data,
     label_col = "LABEL", rules = rules, use_value_labels = TRUE,
     overwrite = FALSE)
   expect_snapshot(table(study_data[[vn]], useNA = "always"))
   expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))

  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregs in males", "9999", '[v00002]="males"',
   )
   r <- prep_add_missing_codes(NA, study_data = study_data,
     meta_data = meta_data,
     label_col = "LABEL", rules = rules, use_value_labels = TRUE,
     overwrite = FALSE)
   expect_snapshot(table(study_data[[vn]], useNA = "always"))
   expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))
   # devtools::load_all(".")

  study_data$v00002 <- ifelse(study_data$v00002 == "0", "females", "males")
  meta_data[meta_data$LABEL == "SEX_0", "VALUE_LABELS"] <- "females|males"
  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[v00002]="males"',
  )
  r <- prep_add_missing_codes(NA, study_data = study_data, meta_data =
                              meta_data,
                              label_col = "LABEL", rules = rules,
                              use_value_labels = TRUE, overwrite = FALSE)
  expect_snapshot(table(study_data[[vn]], useNA = "always"))
  expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))
})
