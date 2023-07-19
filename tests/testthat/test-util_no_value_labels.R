test_that("util_no_value_labels works", {
  local({
    meta_data <- prep_get_data_frame("meta_data")
    expect_identical(util_no_value_labels("CENTER_0", meta_data = meta_data,
                                          label_col = LABEL, stop = FALSE,
                                          warn = FALSE), character(0))
    expect_message(util_no_value_labels("CENTER_0", meta_data = meta_data,
                                        label_col = LABEL, stop = FALSE,
                                        warn = TRUE), character(0),
      regexp = NULL
    )
    expect_error(util_no_value_labels("CENTER_0", meta_data = meta_data,
                                      label_col = LABEL, stop = TRUE,
                                      warn = FALSE), character(0),
      regexp = NULL,
      class = "error"
    )
    expect_identical(
      sort(util_no_value_labels(resp_vars = meta_data[[LABEL]],
                                meta_data = meta_data, label_col = LABEL,
                                stop = FALSE, warn = FALSE)),
      c(
        "AGE_0", "AGE_1", "ARM_CIRC_0", "BSG_0", "CRP_0", "DBP_0", "DEV_NO_0",
        "GLOBAL_HEALTH_VAS_0", "ITEM_1_0", "ITEM_2_0", "ITEM_3_0",
        "ITEM_4_0", "ITEM_5_0", "ITEM_6_0", "ITEM_7_0", "ITEM_8_0",
        "N_ATC_CODES_0", "N_BIRTH_0", "N_CHILD_0", "N_INJURIES_0", "SBP_0"
      )
    )
  })
})
