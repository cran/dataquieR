test_that("util_correct_variable_use works", {
  skip_on_cran() # slow and implicitly tested by other tests
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  acc_test <- function(resp_variable, aux_variable, time_variable,
                       co_variables, group_vars, study_data, meta_data,
                       label_col) {
    prep_prepare_dataframes()
    util_correct_variable_use(resp_variable)
    util_correct_variable_use("resp_variable")
    util_correct_variable_use("aux_variable")
    util_correct_variable_use("time_variable")
    if (missing(co_variables)) {
      co_variables <- NA
      warning("No co_variables were defined")
    }
    util_correct_variable_use("co_variables", allow_na = TRUE,
                              allow_more_than_one = TRUE)
    util_correct_variable_use2(group_vars)
    co_variables <- na.omit(co_variables)
  }
  environment(acc_test) <- asNamespace("dataquieR")
  acc_test2 <- function(resp_variable, aux_variable, time_variable,
                       co_variables, group_vars, study_data, meta_data,
                       label_col) {
    prep_prepare_dataframes()
    expect_error(util_correct_variable_use(c(a, b)), regexp =
                   paste("argument arg_name must be of length 1, wrong use",
                         "of util_correct_variable_use."))
    expect_error(util_correct_variable_use(character(0)), regexp =
                   paste("argument arg_name must be of length 1, wrong use",
                         "of util_correct_variable_use."))

    expect_error(util_correct_variable_use(resp_variable, role = "invalid"),
                 regexp =
                   paste("Unknown variable-argument role: invalid for",
                         "argument resp_variable, wrong use",
                         "of util_correct_variable_use."))

    expect_error(util_correct_variable_use(invalid),
                 regexp =
                   paste("Unknown function argument invalid checked,",
                         "wrong use",
                         "of util_correct_variable_use."))
    resp_variable <- "v00000"
    label_col <- VAR_NAMES
    expect_silent(util_correct_variable_use(resp_variable))
    label_col <- "XXX"
    expect_silent(util_correct_variable_use(resp_variable))
    rm(label_col)
    expect_silent(util_correct_variable_use(resp_variable))
    delayedAssign("label_col", stop())
    expect_silent(util_correct_variable_use(resp_variable))
    label_col <- VAR_NAMES
  }
  environment(acc_test2) <- asNamespace("dataquieR")
  acc_test3 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data) {
    prep_prepare_dataframes()
    expect_error(util_correct_variable_use(c(a, b)), regexp =
                   paste("argument arg_name must be of length 1, wrong use",
                         "of util_correct_variable_use."))
    expect_error(util_correct_variable_use(character(0)), regexp =
                   paste("argument arg_name must be of length 1, wrong use",
                         "of util_correct_variable_use."))

    expect_error(util_correct_variable_use(resp_variable, role = "invalid"),
                 regexp =
                   paste("Unknown variable-argument role: invalid for",
                         "argument resp_variable, wrong use",
                         "of util_correct_variable_use."))

    expect_error(util_correct_variable_use(invalid),
                 regexp =
                   paste("Unknown function argument invalid checked,",
                         "wrong use",
                         "of util_correct_variable_use."))
    resp_variable <- "v00000"
    expect_silent(util_correct_variable_use(resp_variable))
  }
  environment(acc_test3) <- asNamespace("dataquieR")
  acc_test_type <-
    function(resp_variable, aux_variable, time_variable, co_variables,
                            group_vars, study_data, meta_data, need_type,
             label_col) {
    prep_prepare_dataframes()
    util_correct_variable_use(resp_variable, need_type = need_type,
      allow_more_than_one = TRUE)
  }
  environment(acc_test_type) <- asNamespace("dataquieR")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  expect_warning(
    expect_error(
      acc_test(study_data = study_data, meta_data = meta_data),
      regexp = "Argument resp_variable is NULL"
    ),
    regexp = paste(
      "Missing argument .+resp_variable.+ without default value.",
      "Setting to NULL. As a dataquieR developer"
    )
  )
  expect_error(
    acc_test("v00001"),
    regexp = "Need study data as a data frame"
  )
  expect_warning(
    expect_error(
      acc_test("v00001", study_data = study_data, meta_data = meta_data),
      regexp = paste(
        "Argument aux_variable is NULL"
      ),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+aux_variable.+ without default value. Setting",
      "to NULL. As a dataquieR developer,"
    ),
    perl = TRUE
  )
  expect_warning(
    expect_error(
      acc_test("v00001", "v00001", study_data = study_data,
               meta_data = meta_data),
      regexp = paste(
        "Argument time_variable is NULL"
      ),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+time_variable.+ without default value.",
      "Setting to NULL. As a dataquieR developer,"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test("v00001", "v00001", "v00001", study_data = study_data,
     meta_data = meta_data, co_variables = 1:10),
    regexp = paste(
      "Need character variable names in argument co_variables"
    ),
    perl = TRUE
  )

  expect_warning(
    expect_error(
      acc_test("v00001", "v00001", "v00001", "v00001", study_data = study_data,
        meta_data = meta_data),
      regexp = paste(
        "Argument group_vars is NULL"
      ),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+group_vars.+ without default value.",
      "Setting to NULL. As a dataquieR developer,"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c()
    ),
  regexp = paste(
    "Argument group_vars is NULL"
  ),
  perl = TRUE
  )

  expect_silent(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c("v00001")
    )
  )

  expect_error(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c("v00001", "v00002")
    ),
    regexp = paste(
      "Variable .+v00002.+ \\(group_vars\\) has NA observations,",
      "which is not allowed"
    ),
    perl = TRUE
  )

  expect_silent(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c("v00001", "v00000")
    )
  )

  expect_silent(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "integer|string"
    )
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "integer|float"
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00001.+ \\(string\\) does",
      "not have an allowed type \\(integer\\|float\\)"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "integer|xxx"
    ),
    regexp = paste(
      "Internal error: .+resp_variable.+s .+need_type.+ contains invalid type",
      "names .+xxx.+allowed are .+integer.+,",
      ".+string.+, .+float.+, .+datetime.+.",
      "As a dataquieR developer, you should fix your call of",
      ".+acc_test_type+."
    ),
    perl = TRUE
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "string"
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00000.+ \\(integer\\) does",
      "not have an allowed type \\(string\\)"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "!integer"
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00000.+ \\(integer\\)",
      "has a disallowed type \\(.+integer.+\\)"
    ),
    perl = TRUE
  )

  expect_silent(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "!datetime"
    )
  )

  acc_test2("v00001", "v00001", "v00001", "v00001",
            study_data = study_data, meta_data = meta_data,
            group_vars = c("v00001", "v00000")
  )
  acc_test3("v00001", "v00001", "v00001", "v00001",
            study_data = study_data, meta_data = meta_data,
            group_vars = c("v00001", "v00000")
  )

  acc_test4 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data,
                        label_col) {
    prep_prepare_dataframes()
    util_correct_variable_use(resp_variable)
  }
  environment(acc_test4) <- asNamespace("dataquieR")
  delayedAssign("resp_variable", stop("Error"))

  suppressWarnings(expect_warning(expect_warning(
    expect_error(
      acc_test4(resp_variable = resp_variable,
              "v00001", "v00001", "v00001",
              study_data = study_data, meta_data = meta_data,
              group_vars = c("v00001", "v00000")),
      regexp = "Argument resp_variable is NULL"
    ),
    regexp = paste("Could not get value of argument",
                   "resp_variable for unexpected reasons.",
                   "Setting to NULL."
    ),
    perl = TRUE
  ), regexp = paste("Error")))

  resp_variable <- "v00001"

  acc_test5 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data,
                        label_col) {
    util_correct_variable_use(resp_variable)
  }
  environment(acc_test5) <- asNamespace("dataquieR")
  expect_error(
    acc_test5(resp_variable = "v00001", meta_data = meta_data),
    regexp = paste("Did not find merged study data and metadata ds1.",
                   "Wrong use of util_correct_variable_use?")
  )

  acc_test6 <- function(resp_variable, aux_variable, time_variable,
                        co_variables, group_vars, study_data, meta_data,
                        label_col, cmd, ...) {
    prep_prepare_dataframes()
    cmd()
    util_correct_variable_use(resp_variable, ...)
  }
  environment(acc_test6) <- asNamespace("dataquieR")
  expect_error(
    acc_test6(resp_variable = "v00001", study_data = study_data,
              meta_data = meta_data,
              cmd = function() { rm(meta_data, envir = parent.frame())}),
    regexp = paste("Did not find metadata.",
                   "Wrong use of util_correct_variable_use?")
  )
  expect_error(
    acc_test6(resp_variable = "v00001", study_data = study_data,
              meta_data = meta_data,
              cmd = function() { assign("meta_data", 42, parent.frame()) }),
    regexp = paste("meta_data does not provide a metadata data frame.",
                   "Wrong use of util_correct_variable_use?")
  )

  expect_error(
    acc_test6(resp_variable = "v00001", study_data = study_data,
              meta_data = meta_data,
              cmd = function() { assign("ds1", 42, parent.frame()) }),
    regexp = paste("ds1 does not provide merged study data and metadata.",
                   "Wrong use of util_correct_variable_use?")
  )

  expect_error(
    acc_test6(resp_variable = character(0), study_data = study_data,
              meta_data = meta_data,
              cmd = function() { }, allow_more_than_one = TRUE),
    regexp = paste("Need at least one element in argument resp_variable, got 0")
  )

  expect_error(
    acc_test6(resp_variable = letters, study_data = study_data,
              meta_data = meta_data,
              cmd = function() { }),
    regexp = paste("Need exactly one element in argument resp_variable, got",
                   "26: .a, b, c, d,.+x, y, z."),
    perl = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE <- NA
  expect_warning(
    acc_test6(resp_variable = "v00001", study_data = study_data,
              meta_data = md0,
              cmd = function() { }, need_type = "string"),
    regexp = paste("predicted the.+DATA_TYPE"),
    perl = TRUE
  )

  expect_error(
    acc_test6(resp_variable = "v00000", study_data = study_data,
              meta_data = meta_data,
              cmd = function() { }, need_type = "string",
              allow_more_than_one = TRUE),
    regexp = paste("Argument .+resp_variable.+: Variable .+v00000.+ .integer.",
                   "does not have an allowed type .string."),
    perl = TRUE
  )

})
