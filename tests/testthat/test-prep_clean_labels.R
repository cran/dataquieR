test_that("prep_clean_labels works", {
  meta_data1 <- data.frame(
    LABEL =
      c(
        "syst. Blood pressure (mmHg) 1",
        "1st heart frequency in MHz",
        "body surface (\\u33A1)"
      )
  )
  expect_warning(
    expect_equal(prep_clean_labels(meta_data1$LABEL),
                 c("syst_Blood_pressure_mmHg_1", "st_heart_frequency_in_MHz",
                   "body_surface_u33A1_")),
    regexp = "Adjusted labels to be valid variable names."
  )
  expect_warning(
    expect_equal(prep_clean_labels("LABEL", meta_data1),
               structure(list(LABEL = c("syst_Blood_pressure_mmHg_1",
                                        "st_heart_frequency_in_MHz",
                                        "body_surface_u33A1_")),
                         row.names = c(NA, -3L), class = "data.frame")),
    regexp = "Adjusted labels in .{1,4}LABEL.{1,4} to be valid variable names."
  )
  meta_data2 <- data.frame(
    LABEL =
      c(
        "syst. Blood pressure (mmHg) 1",
        "syst. Blood pressure  mmHg! 1",
        "body surface (\\u33A1)"
      )
  )
  expect_error(print(prep_clean_labels(meta_data2$LABEL, no_dups = TRUE)),
                   regexp = "Have duplicates in desired variable labels"
  )
  expect_warning(
    expect_equal(prep_clean_labels(meta_data2$LABEL, no_dups = FALSE),
               c("syst_Blood_pressure_mmHg_1", "syst_Blood_pressure_mmHg_1",
                 "body_surface_u33A1_")),
    regexp = "Adjusted labels to be valid variable names."
  )
})
