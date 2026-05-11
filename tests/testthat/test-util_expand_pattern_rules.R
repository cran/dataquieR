test_that("literal bracketed variable names are kept", {
  valid_names <- c("AGE_0", "SEX_0")

  expect_equal(
    util_expand_pattern_rules("[AGE_0] > 55", valid_names),
    util_attach_attr("[AGE_0] > 55", mismatches = list())
  )
})

test_that("wildcards expand matching variable names", {
  valid_names <- c("SEX_0", "PREGNANT_0", "PREGNANT_1", "AGE_0")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_*]", valid_names),
    util_attach_attr(c("[PREGNANT_0]", "[PREGNANT_1]"), mismatches = list())
  )
})

test_that("digit wildcard expands one digit only", {
  valid_names <- c("PREGNANT_0", "PREGNANT_09", "PREGNANT_x")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_#]", valid_names),
    util_attach_attr("[PREGNANT_0]", mismatches = list())
  )
})

test_that("two digit wildcard expands exactly two digits", {
  valid_names <- c("PREGNANT_0", "PREGNANT_09", "PREGNANT_99")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_##]", valid_names),
    util_attach_attr(c("[PREGNANT_09]", "[PREGNANT_99]"), mismatches = list())
  )
})

test_that("one or two digit wildcard expands one or two digits", {
  valid_names <- c("PREGNANT_0", "PREGNANT_09", "PREGNANT_123")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_#?]", valid_names),
    util_attach_attr(c("[PREGNANT_0]", "[PREGNANT_09]"), mismatches = list())
  )
})

test_that("captures are reused across variables", {
  valid_names <- c(
    "PREGNANT_0", "PREGNANT_1",
    "AGE_0", "AGE_1", "AGE_2"
  )

  expect_equal(
    util_expand_pattern_rules(
      "[PREGNANT_{W:#}] = \"yes\" and [AGE_{W}] > 55",
      valid_names
    ),
    util_attach_attr(c(
      "[PREGNANT_0] = \"yes\" and [AGE_0] > 55",
      "[PREGNANT_1] = \"yes\" and [AGE_1] > 55"
    ), mismatches = list())
  )
})

test_that("captures do not create mixed combinations", {
  valid_names <- c("PREGNANT_0", "PREGNANT_1", "AGE_0")

  expect_equal(
    util_expand_pattern_rules(
      "[PREGNANT_{W:#}] = \"yes\" and [AGE_{W}] > 55",
      valid_names
    ),
    util_attach_attr("[PREGNANT_0] = \"yes\" and [AGE_0] > 55", mismatches = list())
  )
})

test_that("capture arithmetic with minus offset works", {
  valid_names <- c("AGE_0", "AGE_1", "AGE_2")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:#}] < [AGE_{CE-1}]", valid_names),
    util_attach_attr(c("[AGE_1] < [AGE_0]", "[AGE_2] < [AGE_1]"), mismatches = list())
  )
})

test_that("capture arithmetic with plus offset works", {
  valid_names <- c("AGE_0", "AGE_1", "AGE_2")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:#}] < [AGE_{CE+1}]", valid_names),
    util_attach_attr(c("[AGE_0] < [AGE_1]", "[AGE_1] < [AGE_2]"), mismatches = list())
  )
})

test_that("invalid arithmetic results are dropped", {
  valid_names <- c("AGE_0", "AGE_1")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:#}] < [AGE_{CE-1}]", valid_names),
    util_attach_attr("[AGE_1] < [AGE_0]", mismatches = list())
  )
})

test_that("arithmetic on non-integer captures drops generated rule", {
  valid_names <- c("AGE_A", "AGE_B")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:?}] < [AGE_{CE-1}]", valid_names),
    util_attach_attr(character(0), mismatches = list())
  )
})

test_that("escaped mini-language characters are treated literally", {
  valid_names <- c("Pregnant? 0", "Pregnant* 0", "AGE_0")

  expect_equal(
    util_expand_pattern_rules(
      "[Pregnant\\? {W:#}] and [AGE_{W}]",
      valid_names
    ),
    util_attach_attr("[Pregnant? 0] and [AGE_0]", mismatches = list())
  )
})

test_that("variable labels with spaces and punctuation work", {
  valid_names <- c("Age at exam 0", "Pregnant? 0")

  expect_equal(
    util_expand_pattern_rules(
      "[Pregnant\\? {W:#}] and [Age at exam {W}]",
      valid_names
    ),
    util_attach_attr("[Pregnant? 0] and [Age at exam 0]", mismatches = list())
  )
})

test_that("valid names must not contain square brackets", {
  expect_warning(
    util_expand_pattern_rules("[AGE_0]", c("AGE_0", "AGE[1]")),
    "valid names must not contain"
  )
})

test_that("undefined capture references fail clearly", {
  expect_error(
    util_expand_pattern_rules("[AGE_{W}]", "AGE_0"),
    "used before being defined"
  )
})

test_that("unmatched rules return character zero", {
  expect_equal(
    util_expand_pattern_rules("[DOES_NOT_EXIST_*]", "AGE_0"),
    util_attach_attr(character(0), mismatches = list("DOES_NOT_EXIST_*"))
  )
})

test_that("expanded cross-item pattern rules get unique IDs and labels", {
  meta_data <- data.frame(
    VAR_NAMES = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    LABEL = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    LONG_LABEL = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    ORIGINAL_VAR_NAMES = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    ORIGINAL_LABEL = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("bloodpressure", "age"),
    CHECK_LABEL = c("bloodpressure", "age"),
    CONTRADICTION_TERM = c(
      "[sbp_0] > [dbp_0]",
      "[age_{W:#}] < [age_{W-1}]"
    ),
    stringsAsFactors = FALSE
  )

  out <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )

  expect_equal(out[[CHECK_ID]], c("bloodpressure", "age_1", "age_2"))
  expect_equal(out[[CHECK_LABEL]], c("bloodpressure", "age #1", "age #2"))
  expect_equal(
    out[[CONTRADICTION_TERM]],
    c("[sbp_0] > [dbp_0]", "[age_1] < [age_0]", "[age_2] < [age_1]")
  )
})
