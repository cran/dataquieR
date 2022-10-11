test_that("pipeline_vectorized works", {

  load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata/meta_data.RData", package = "dataquieR"),
       envir = environment())

  expect_warning({
    a <- pipeline_vectorized(
      fct = acc_margins, study_data = study_data,
      meta_data = meta_data, label_col = LABEL,
      key_var_names = c(group_vars = KEY_OBSERVER),
      resp_vars = "SBP_0",
      cores = 1
    )
  }, regexp =
    "For co_vars and id_vars, auto-fill has not yet been implemented.")
})
