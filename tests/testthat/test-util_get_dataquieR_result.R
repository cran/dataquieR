test_that("get dataquieR_result works", {
  skip_on_cran() # deprecated
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)

  skip_on_cran() # slow and anyway tested implicitly by other tests

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  expect_message({
      a <- pipeline_vectorized(
       fct = acc_margins, study_data = study_data,
       meta_data = meta_data, label_col = LABEL,
       key_var_names = c(group_vars = GROUP_VAR_OBSERVER),
       resp_vars = "SBP_0",
       cores = 1
      )
    }, regexp =
      "For co_vars and id_vars, auto-fill has not yet been implemented.")

  x <- names(attributes(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`$SummaryData)
  )
  y <- c("names", "row.names", "class", "error", "message", "warning" )
  expect_equal(
    length(intersect(x, y)),
    length(union(x, y))
  )
  expect_s3_class(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`,
    "dataquieR_result"
  )
  expect_s3_class(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`$SummaryPlot,
    "dataquieR_result"
  )
  expect_s3_class(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`$SummaryTable,
    "dataquieR_result"
  )
  expect_s3_class(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`$SummaryData,
    "dataquieR_result"
  )
  expect_s3_class(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`["SummaryPlot"],
    "dataquieR_result"
  )
  expect_s3_class(
    a$`group_vars = USR_BP_0`$`resp_vars = SBP_0`[["SummaryPlot"]],
    "dataquieR_result"
  )
})
