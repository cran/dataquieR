test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  # TODO: Write
  # load(system.file("extdata/study_data.RData", package = "dataquieR"))
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  #
  # code_labels <- prep_get_data_frame(
  #   "meta_data_v2|missing_table")
  #
  # meta_data$MISSING_LIST[[44]] <- "x| 9"
  # util_validate_known_meta(meta_data = meta_data)
  #
  # study_data$v00003
  # study_data$v00003[1:400] <- 99980
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  #
  # prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = LABEL, .replace_missings = TRUE)
  #
  # head(ds1$AGE_0)
  #
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE)
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE, expand_codes = TRUE)
  #
  #
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$MISSING_LIST[[44]] <- "x| 9"
  # meta_data$MISSING_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE)
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE, expand_codes = !TRUE)
  #
  # # expect warning
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # meta_data$MISSING_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE)
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE, expand_codes = !TRUE)
  #
  # # expect warning
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP | 99980 = XXX"
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE)
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE, expand_codes = !TRUE)
  #
  # # expect warning, if assume_consistent_codes
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # meta_data$MISSING_LIST[meta_data$VAR_NAMES == "v00002"] <- "99980 = NOOP"
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE)
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE, expand_codes = !TRUE)
  #
  # # expect warning, if assume_consistent_codes
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00002"] <- "99980 = XXX"
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE)
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE, expand_codes = !TRUE)
  #
  # com_item_missingness(resp_vars = c("AGE_0", "ITEM_3_0"), study_data, meta_data, label_col = LABEL, show_causes = TRUE, assume_consistent_codes = !TRUE,
  #                      expand_codes = !TRUE, cause_label_df = code_labels)
  #
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00002"] <- "99980 = XXX"
  # util_validate_missing_lists(meta_data = meta_data, cause_label_df = code_labels)
  #
  # meta_data$JUMP_LIST[1] <- "2021-02-28 = missing | - = missing | 9999 = missing"
  # code_labels <- rbind.data.frame(code_labels, c(NA, NA))
  # util_validate_missing_lists(meta_data = meta_data, cause_label_df = code_labels)
  #
  # load(system.file("extdata/meta_data.RData", package = "dataquieR"))
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
  # meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00002"] <- "99980 = XXX"
  # util_validate_missing_lists(meta_data = meta_data, cause_label_df = code_labels, label_col = LABEL)
  #
})
