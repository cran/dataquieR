test_that("util_filter_names_regexps works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  # selecting functions I want to keep with regexp----
  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
    c("^acc_", "^con_")),
    setNames(nm = c("acc_margins", "con_limit_dev" )))

  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
    c("^acc_")),
    setNames(nm = c("acc_margins")))

  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
    c()),
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")))

  expect_error(util_filter_names_by_regexps(
    setNames(nm = c()), c("^acc_", "^con_")),
    regexp = paste(".*Need names.*"),
    perl = TRUE)

  expect_identical(util_filter_names_by_regexps(
    setNames(nm = character(0)),
    c("^acc_", "^con_")),
    setNames(character(0), character(0)))

  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins")),
    c("^acc_", "^con_")),
    setNames(nm = c("acc_margins")))

  expect_identical(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins")),
    c("^xacc_", "^con_")),
    setNames(character(0), character(0)))

  # excluding functions I do not want to keep with regexp----
  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
    c("^acc_", "^con_"),
    negate = TRUE),
    setNames(nm = c("com_item_miss")))

  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
    c("^acc_"),
    negate = T),
    setNames(nm = c("com_item_miss", "con_limit_dev")))

  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
    c(),
    negate = T),
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")))

  expect_error(util_filter_names_by_regexps(
    setNames(nm = c()),
    c("^acc_", "^con_"),
    negate = TRUE),
    regexp = paste(".*Need names.*"),
    perl = TRUE)

  expect_identical(util_filter_names_by_regexps(
    setNames(nm = character(0)),
    c("^acc_", "^con_"),
    negate = TRUE),
    setNames(character(0), character(0)))

  expect_equal(util_filter_names_by_regexps(
    setNames(nm = c("acc_margins")),
    c("^acc_", "^con_"),
    negate = T),
    setNames(character(0), character(0)))


})
