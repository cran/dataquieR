test_that("acc_shape_or_scale works with 3 args", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())


  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         guess = c(TRUE, FALSE, TRUE)),
    regexp = sprintf(
      "(%s|%s)",
      paste("guess should be a scalar logical value.",
            "Have more than one value, use the first one only"),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         end_digits = c(TRUE, FALSE, TRUE)),
    regexp = sprintf(
      "(%s|%s)",
      paste("end_digits should be a scalar logical value.",
            "Have more than one value, use the first one only"),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         par1 = "xxx"),
    regexp = "par1 should be a numeric value"
  )

  expect_error(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         par1 = 0,
                         par2 = "xxx",
                         guess = FALSE),
    regexp = "par2 should be a numeric value"
  )

  md1 <- meta_data
  md1[md1$VAR_NAMES == "v00014", DISTRIBUTION] <- NA
  suppressWarnings(expect_error(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = md1,
                         dist_col = DISTRIBUTION,
                         par1 = 0,
                         par2 = 1,
                         guess = FALSE),
    regexp = "No distribution specified for v00014 in DISTRIBUTION"
  ))

  md1 <- meta_data
  md1[md1$VAR_NAMES == "v00014", DISTRIBUTION] <- "dirichlet"
  suppressWarnings(expect_error(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = md1,
                         dist_col = DISTRIBUTION,
                         par1 = 0,
                         par2 = 1,
                         guess = FALSE),
    regexp = "This distribution .+dirichlet.+ is not supported yet..."
  ))

  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00016", # uniform and integer
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         par1 = -10,
                         par2 = 50,
                         guess = FALSE),
    regexp = "Due to missing values in v00016 308 observations were deleted."
  )
  expect_equal(sum(1 == res1$SummaryData$GRADING), 4)
  expect_error(
    suppressWarnings(
      res1 <-
        acc_shape_or_scale(resp_vars = "v00014",
                           study_data = study_data,
                           meta_data = meta_data,
                           dist_col = DISTRIBUTION,
                           par1 = Inf,
                           par2 = 1,
                           guess = FALSE)
    ),
    regexp = paste(
      "Since .+guess.+ is not true finite numerical",
      "parameters must be prespecified"),
    perl = TRUE
  )

expect_error(
  res1 <-
    acc_shape_or_scale(resp_vars = "v00001",
                       study_data = study_data,
                       meta_data = meta_data,
                       dist_col = DISTRIBUTION,
                       par1 = 0,
                       par2 = 1,
                       guess = FALSE),
  regexp = "resp_vars == .+v00001.+ must be a non-empty numeric variable",
  perl = TRUE
)
  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         par1 = 0:10,
                         par2 = 1:11),
    regexp = sprintf(
      "(%s|%s|%s|%s)",
      paste("Since parameters were specified: .+guess.+ is set to false"),
      paste("par1 should be a scalar numeric value. Have more than one value,",
            "use the first one only"),
      paste("par2 should be a scalar numeric value. Have more than one value,",
            "use the first one only"),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    all = TRUE,
    perl = TRUE
  )
  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         par1 = 0,
                         par2 = 1),
    regexp = sprintf(
      "(%s|%s)",
      paste("Since parameters were specified: .+guess.+ is set to false"),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    all = TRUE,
    perl = TRUE
  )
  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = meta_data,
                         dist_col = DISTRIBUTION,
                         guess = c(NA, TRUE)),
    regexp = sprintf("(%s|%s)",
                     paste("Have more than one value for guess,",
                           "use the first one only"),
                     paste("Due to missing values in v00014 301",
                           "observations were deleted.")),
    perl = TRUE,
    all = TRUE
  )
  md1 <- meta_data[, setdiff(colnames(meta_data), DISTRIBUTION)]
  expect_error(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data,
                         meta_data = md1,
                         dist_col = DISTRIBUTION),
    regexp = "Did not find variable attribute DISTRIBUTION in the meta_data"
  )
  expect_warning(
    expect_error(
      res1 <-
        acc_shape_or_scale(study_data = study_data, meta_data = meta_data),
      regexp =
        "Argument resp_vars is NULL",
      perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste("Missing argument .+resp_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer,")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "v00014",
                         study_data = study_data, meta_data = meta_data),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("A column of the metaddata specifying the distributions has",
              "not been specified. Trying the default .+DISTRIBUTION.+."),
        paste("Due to missing values in v00014 301 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true(all(c("SummaryData",
                  "SummaryPlot",
                  "SummaryTable") %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryData)),
      na.rm = TRUE) - 5484.87)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 1
  )
})

test_that("acc_shape_or_scale works with label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    expect_error(
      res1 <-
        acc_shape_or_scale(study_data = study_data, meta_data = meta_data,
                           label_col = LABEL),
      regexp =
        "Argument resp_vars is NULL",
      perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste("Missing argument .+resp_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer,")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
      acc_shape_or_scale(resp_vars = "CRP_0",
                         study_data = study_data, meta_data = meta_data,
                         label_col = LABEL),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("A column of the metaddata specifying the distributions has",
              "not been specified. Trying the default .+DISTRIBUTION.+."),
        paste("Due to missing values in CRP_0 301 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true(all(c("SummaryData",
                    "SummaryPlot",
                    "SummaryTable") %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryData)),
      na.rm = TRUE) - 5484.87)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 1
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("shape_or_scale plot for CRP_0 ok",
                              res1$SummaryPlot)
})
