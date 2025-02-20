test_that("util_split_val_tab works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_purge_data_frame_cache()

  test <- tibble::tribble(
    ~ CODE_VALUE, ~ CODE_LABEL, ~ CODE_CLASS, ~ CODE_INTERPRET, ~ CODE_ORDER,
    ~ VALUE_LABEL_TABLE, ~ MISSING_LIST_TABLE,
    1234, "Test1", "MISSING", "P", 1, "vtab1", "mtab1",
    2234, "Test2", "JUMP", "P", 1, "vtab1", "mtab1",
    3234, "Test3", "VALUE", NA, 1, "vtab1", "vtab1",
    4234, "Test4", "MISSING", "P", 1, "vtab1", "mtab2",
    5234, "Test5", "MISSING", "P", 1, "vtab1", "mtab2",
    3234, "Test999", "VALUE", NA, 1, "vtab2", NA
  )
  expect_warning(util_split_val_tab(test))
  expect_equal(sort(prep_list_dataframes()),
               c("mtab1", "mtab2", "vtab1", "vtab2"))
  prep_purge_data_frame_cache()

  test <- tibble::tribble(
    ~ CODE_VALUE, ~ CODE_LABEL, ~ CODE_CLASS, ~ CODE_INTERPRET, ~ CODE_ORDER,
    ~ VALUE_LABEL_TABLE, ~ MISSING_LIST_TABLE,
    1234, "Test1", "MISSING", "P", 1, "vtab1", "mtab1",
    2234, "Test2", "JUMP", "P", 1, "vtab1", "mtab1",
    3234, "Test3", "VALUE", NA, 1, "vtab1", "",
    4234, "Test4", "MISSING", "P", 1, "vtab1", "mtab2",
    5234, "Test5", "MISSING", "P", 1, "vtab1", "mtab2",
    3234, "Test999", "VALUE", NA, 1, "vtab2", NA
  )
  util_split_val_tab(test)
  expect_equal(sort(prep_list_dataframes()),
               c("mtab1", "mtab2", "vtab1", "vtab2"))
  prep_purge_data_frame_cache()

  test <- tibble::tribble(
    ~ CODE_VALUE, ~ CODE_LABEL, ~ CODE_CLASS, ~ CODE_INTERPRET, ~ CODE_ORDER,
    ~ CODE_LIST_TABLE,
    1234, "Test1", "MISSING", "P", 1, "tab1",
    2234, "Test2", "JUMP", "P", 1, "tab1",
    3234, "Test3", "VALUE", NA, 1, "tab1",
    4234, "Test4", "MISSING", "P", 1, "tab1",
    5234, "Test5", "MISSING", "P", 1, "tab1",
    3234, "Test999", "VALUE", NA, 1, "tab2",
    3235, "Test99x9", "VALUE", NA, 2, "tab2",
    3236, "Test999a", "MISSING", NA, 1, "tab4",
    3237, "Test99x9b", "JUMP", NA, 2, "tab4",
    0, "no", "VALUE", NA, 1, "tab9",
    1, "yes", "VALUE", NA, 2, "tab9",
    3234, "MISSING", "MISSING", "P", 1, "tab9"
  )
  expect_warning(util_split_val_tab(test))
  expect_equal(prep_list_dataframes(), c("tab1", "tab2", "tab4", "tab9"))

  # prep_purge_data_frame_cache()
  # prep_load_workbook_like_file("meta_data_v2")
  # ilmd <- prep_get_data_frame("item_level")
  # ilmd <- util_normalize_value_labels(ilmd)
  # ilmd$VALUE_LABEL_TABLE[8] <- "tab9"
  # ilmd$MISSING_LIST_TABLE[8] <- "tab9"
  # ilmd$VARIABLE_ROLE[8] <- VARIABLE_ROLES$PRIMARY
  # prep_add_data_frames(item_level = ilmd)
  # prep_get_labels(ilmd$VAR_NAMES[[8]])
  # # r <- dq_report2("study_data", dimensions = NULL)
})

test_that("prep_unsplit_val_tabs works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_purge_data_frame_cache()
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  prep_unsplit_val_tabs(val_tab = CODE_LIST_TABLE)
  # prep_open_in_excel(CODE_LIST_TABLE)
  clt <- prep_get_data_frame(CODE_LIST_TABLE)
  # View(clt)
  expect_snapshot(clt)

  prep_purge_data_frame_cache()
  util_split_val_tab(clt)
  expect_snapshot(prep_list_dataframes())
})
