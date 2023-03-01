test_that("pipeline_recursive_result works", {
  skip_on_cran() # deprecated
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  expect_error(
    pipeline_recursive_result(42),
    regexp =
      paste("call_plan_with_results must be a data frame as returned",
            "by pipeline_vectorize(..., result_groups = NULL)"),
    fixed = TRUE
  )
  expect_error(
    pipeline_recursive_result(data.frame()),
    regexp =
      paste("call_plan_with_results must be a data frame as returned",
            "by pipeline_vectorize(..., result_groups = NULL)"),
    fixed = TRUE
  )
  call_plan_with_results <- structure(list(
    resp_vars =
      c(
        "SBP_0", "DBP_0", "VO2_CAPCAT_0",
        "BSG_0"
      ), group_vars = c(
      "USR_BP_0", "USR_BP_0", "USR_VO2_0",
      "USR_BP_0"
    ), co_vars = list("SEX_0", "SEX_0", "SEX_0", "SEX_0")
  ),
  class = "data.frame", row.names = c(
    NA,
    -4L
  )
  )
  call_plan_with_results[["results"]] <-
    list(NA, 2, "Hello", list())
  result_groups <-
    colnames(call_plan_with_results)[2:(ncol(call_plan_with_results) - 1)]
  expect_equal(
    pipeline_recursive_result(call_plan_with_results, result_groups),
    list(
      "group_vars = USR_BP_0" = list(
        "co_vars = SEX_0" = list(
          "resp_vars = SBP_0" = NA,
          "resp_vars = DBP_0" = 2,
          "resp_vars = BSG_0" = list()
        )
      ),
      "group_vars = USR_VO2_0" = list("co_vars = SEX_0" =
                                        list("resp_vars = VO2_CAPCAT_0" =
                                               "Hello"))
    )
  )
  expect_equal(
    pipeline_recursive_result(call_plan_with_results, rev(result_groups)),
    list("co_vars = SEX_0" = list(
      "group_vars = USR_BP_0" = list(
        "resp_vars = SBP_0" = NA,
        "resp_vars = DBP_0" = 2,
        "resp_vars = BSG_0" = list()
      ),
      "group_vars = USR_VO2_0" = list("resp_vars = VO2_CAPCAT_0" = "Hello")
    ))
  )

  call_plan_with_results <- structure(list(
    resp_vars =
      c(
        "SBP_0", "SBP_0", "SBP_0",
        "BSG_0"
      ), group_vars = c(
        "USR_BP_0", "USR_BP_0", "USR_VO2_0",
        "USR_BP_0"
      ), co_vars = list("SEX_0", "SEX_0", "SEX_0", "SEX_0"),
      x_vars = 1:4
  ),
  class = "data.frame", row.names = c(
    NA,
    -4L
  )
  )
  call_plan_with_results[["results"]] <-
    list(NA, 2, "Hello", list())
  result_groups <-
    colnames(call_plan_with_results)[2:(ncol(call_plan_with_results) - 1)]
  expect_warning(
    pipeline_recursive_result(call_plan_with_results, head(result_groups, -2)),
    regexp =
      paste("Not each parameter has been selected to create a recursion level.",
            "You may miss some results with identical names. Please check your",
            ".+result_groups.+-argument."),
    perl = TRUE
  )
  expect_error(
    pipeline_recursive_result(call_plan_with_results, head(result_groups, -3)),
    regexp =
      paste("argument",
            "result_groups must be character and length > 0"),
    fixed = TRUE
  )
  expect_warning(
    pipeline_recursive_result(call_plan_with_results,
                              c(result_groups, "xyz")),
    regexp = paste("Not all desired result groups correspond to",
                   "columns in the call_plan. Remove the unknowns."),
    fixed = TRUE
  )
  expect_warning(
    expect_error(pipeline_recursive_result(call_plan_with_results,
                              c("xyz")),
                 regexp = paste("argument result_groups must be character",
                                "and length > 0"),
                 fixed = TRUE
                 ),
    regexp = paste("Not all desired result groups correspond to",
                   "columns in the call_plan. Remove the unknowns."),
    fixed = TRUE
  )
  result_groups <- colnames(call_plan_with_results)
  expect_warning(pipeline_recursive_result(call_plan_with_results,
                            result_groups = result_groups),
                 regexp =
                   "resp_vars and results cannot be used as result_groups.",
                 fixed = TRUE)
})
