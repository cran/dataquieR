test_that("pipeline_vectorized works", {
  skip_on_cran() # deprecated
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  expect_warning({
    a <- pipeline_vectorized(
      fct = acc_margins, study_data = study_data,
      meta_data = meta_data, label_col = LABEL,
      key_var_names = c(group_vars = GROUP_VAR_OBSERVER),
      resp_vars = "SBP_0",
      cores = 1
    )
  }, regexp =
    "For co_vars and id_vars, auto-fill has not yet been implemented.")
})
