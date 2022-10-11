test_that("get dataquieR_result works", {

  skip_on_cran() # slow and anyway tested implicitly by other tests

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
