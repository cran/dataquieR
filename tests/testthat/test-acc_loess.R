test_that("acc_loess works without label_col", {
  skip_on_cran() # slow test
  skip_if_translated()
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data)
      },
      regexp = paste(".*Argument group_vars is NULL"),
      perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("Missing argument .+group_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data,
                  min_obs_in_subgroup = 29)
    },
    regexp = paste(".*Argument group_vars is NULL"),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste("Missing argument .+group_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016")
    },
    regexp = paste(".*Argument time_vars is NULL"),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("Missing argument .+time_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer")
      ),
    perl = TRUE,
    all = TRUE
  )

  sd1 <- study_data
  sd1[["v00017"]][1:1000] <- NA
  expect_warning(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = sd1,
                meta_data = meta_data, group_vars = "v00016",
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      sprintf(
        "(%s|%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+v00014.+"),
        paste("Due to missing values in v00016",
              "93 observations were deleted."),
        paste("Due to missing values in v00017",
              "931 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  sd1 <- study_data
  sd1[["v00017"]] <- as.character(sd1[["v00017"]])
  sd1[["v00017"]][1:1000] <- "2001-02-29"
  expect_warning(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = sd1,
                meta_data = meta_data, group_vars = "v00016",
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      sprintf(
        "(%s|%s|%s|%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+v00014.+"),
        paste("Due to missing values in v00016",
              "138 observations were deleted."),
        paste("Converting .+v00017.+ to DATETIME,",
              "886 values could not be converted"),
        paste("Due to missing values in v00017",
              "886 observations were deleted."),
        paste("Due to invalid time formats in v00017",
              "886 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_error(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                min_obs_in_subgroup = 1:2,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      paste("In acc_loess: .+min_obs_in_subgroup.+ should",
            "be a scalar integer value, not 2 values."),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                resolution = Inf,
                min_obs_in_subgroup = 30,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      paste("In acc_loess: .+resolution.+ needs to be a",
            "single finite numeric value."),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                resolution = "12",
                min_obs_in_subgroup = 30,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      paste("In acc_loess: .+resolution.+ needs to be a",
            "single finite numeric value."),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                resolution = complex(imaginary = 12),
                min_obs_in_subgroup = 30,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      paste("In acc_loess: .+resolution.+ needs to be a",
            "single finite numeric value."),
    perl = TRUE
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  plot_data_time = "x",
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp =
        paste("Argument .+plot_data_time.+ must be",
              "a sclar logical value."),
      perl = TRUE
    )
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  plot_data_time = TRUE,
                  se_line = 42,
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp =
        paste(".+se_line.+ needs to be a list of arguments",
              "for ggplot2..geom_line for the standard error lines."),
      perl = TRUE
    )
  )

  sd0 <- study_data
  sd0$v00017 <- "XXXX"
  expect_warning(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = sd0,
                  meta_data = meta_data, group_vars = "v00016",
                  plot_data_time = TRUE,
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp =
        paste("No not-missing value in DATETIME variable .+v00017.+"),
      perl = TRUE
    ),
    perl = TRUE,
    all = TRUE,
    regexp = sprintf("(%s|%s|%s)",
                     paste("Converting .+v00017.+ to DATETIME,",
                           "2561 values could not be converted"),
                     paste("301 observations were omitted due to",
                           "missing values in 'v00014'"),
                     paste("Due to missing values in v00016 138",
                           "observations were deleted."))
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  plot_data_time = "TRUE",
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp =
        paste("Argument .+plot_data_time.+ must be",
              "a sclar logical value."),
      perl = TRUE
    )
  )

  expect_warning(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                min_obs_in_subgroup = NA,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("No min_obs_in_subgroup. Default n=30 per level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+v00014.+"),
        paste("Due to missing values in v00016",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
         acc_loess(resp_vars = "v00014", study_data = study_data,
                   meta_data = meta_data, group_vars = "v00016",
                   min_obs_in_subgroup = "x",
                   time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("Coulud not convert min_obs_in_subgroup .+x.+ to a number.",
              "Set to standard value n=30."),
        paste("301 observations were omitted due to missing values",
              "in .+v00014.+"),
        paste("Due to missing values in v00016",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+v00014.+"),
        paste("Due to missing values in v00016",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  expect_lt(
    suppressWarnings(abs(mean(as.numeric(
      as.matrix(res1$SummaryPlotList$Loess_fits_combined$data)),
      na.rm = TRUE) - 21.82126)), 50
  )
})

test_that("acc_loess works with label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data,
                  label_col = LABEL)
    },
    regexp = paste(".*Argument group_vars is NULL"),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("Missing argument .+group_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  label_col = LABEL)
    },
    regexp = paste(".*Argument time_vars is NULL"),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("Missing argument .+time_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL)
    ,
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+CRP_0.+"),
        paste("Due to missing values in DEV_NO_0",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  expect_lt(
    suppressWarnings(abs(mean(as.numeric(
      as.matrix(res1$SummaryPlotList$Loess_fits_combined$data)),
      na.rm = TRUE) - 21.82126)), 50
  )
})

test_that("acc_loess output matches", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL)
    ,
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+CRP_0.+"),
        paste("Due to missing values in DEV_NO_0",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("loess facets plot for CRP_0 ok",
                              res1$SummaryPlotList$Loess_fits_facets)
  vdiffr::expect_doppelganger("loess combined plot for CRP_0 ok",
                              res1$SummaryPlotList$Loess_fits_combined)
})

test_that("acc_loess min_obs_in_subgroups with label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  time_vars = "LAB_DT_0",
                  label_col = LABEL, min_obs_in_subgroup = 999)
    },
    regexp = paste("No data left, cannot produce a plot, sorry."),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s|s)",
        paste("301 observations were omitted due to missing values",
              "in .+CRP_0.+"),
        paste("Due to missing values in DEV_NO_0",
              "138 observations were deleted."),
        paste("The following levels: 1 2 3 4 5 have < 30",
              "observations and were discarded.")
      ),
    perl = TRUE,
    all = TRUE
  )
})

test_that("acc_loess with co-vars output matches", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  sd0 <- study_data
  sd0$v00003[1:10] <- NA
  sd0$v00002[11:20] <- NA
  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
                label_col = LABEL)
    ,
    regexp =
      sprintf(
        "(%s|%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+CRP_0.+"),
        paste("Due to missing values in DEV_NO_0",
              "138 observations were deleted."),
        paste("Due to missing values in any of AGE_0, SEX_0",
              "18 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  sd0 <- study_data
  sd0$v00014 <- as.factor(sd0$v00014)
  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
                label_col = LABEL)
    ,
    regexp =
      sprintf(
        "(%s|%s|%s|%s|%s)",
        paste(".+CRP_0.+ is a categorial but not an ordinal variable.",
              "I.ll use the levels as ordinals, but this may lead to",
              "wrong conclusions."),
        paste(".+RP_0.+ is not a metric variable. Ordinal variables may in",
              "some cases still be interpretable with the LOESS plots, but",
              "be aware that distances are meaningless."),
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+CRP_0.+"),
        paste("Due to missing values in DEV_NO_0",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
                label_col = LABEL)
    ,
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("No min_obs_in_subgroup was set. Default n=30 per",
              "level is used."),
        paste("301 observations were omitted due to missing values",
              "in .+CRP_0.+"),
        paste("Due to missing values in DEV_NO_0",
              "138 observations were deleted.")
      ),
    perl = TRUE,
    all = TRUE
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("loess facets plot for CRP_0 with Covars ok",
                              res1$SummaryPlotList$Loess_fits_facets)
  vdiffr::expect_doppelganger("loess combined plot for CRP_0 with Covars ok",
                              res1$SummaryPlotList$Loess_fits_combined)
})

test_that("acc_loess works for all time span ranges", {
  skip_on_cran() # slow test
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  sd0 <- study_data
  v <- subset(meta_data, LABEL == "LAB_DT_0", VAR_NAMES, TRUE)
  sd0[[v]] <- min(sd0[[v]], na.rm = TRUE)
  expect_error(
    suppressWarnings(res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL)),
    regexp = "span is too small"
  )
  sd0 <- study_data
  expect_error(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 0.1,
                                 label_col = LABEL)),
    regexp = "span is too small"
  )
  expect_error(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 1,
                                 label_col = LABEL)),
    regexp = "span is too small"
  )
  expect_silent(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 10,
                                 label_col = LABEL))
  )
  expect_silent(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 100,
                                 label_col = LABEL))
  )
  expect_silent(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 1000,
                                 label_col = LABEL))
  )
  expect_silent(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 10000,
                                 label_col = LABEL))
  )
  expect_silent(
    suppressWarnings(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 1000000,
                                 label_col = LABEL))
  )
  sd0 <- rbind(study_data,
               study_data,
               study_data,
               study_data,
               study_data,
               study_data,
               study_data,
               study_data
  )
  sd0[[v]] <-
    as.POSIXct(rnorm(nrow(sd0), sd = as.numeric(as.POSIXct("1972-01-01")),
                     mean = mean(sd0[[v]], na.rm = TRUE)),
               origin = as.POSIXct(as.POSIXct("1970-01-01")))

  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                resolution = 10000,
                label_col = LABEL),
    regexp = paste(
      "Argument .+plot_data_time.+ was not set.",
      "Based on the maximum of observations of .... for group .+2.+ > 4000,",
      "marks for timepoints featuring data will be turned off."),
    perl = TRUE
  )

  sd0 <- study_data
  g <- subset(meta_data, LABEL == "DEV_NO_0", VAR_NAMES, TRUE)
  sd0[[g]][!is.na(sd0[[g]])] <-
    sample(x = 11, size = sum(!is.na(sd0[[g]])),
           replace = TRUE) # for >= 11 groups,
                           # R standard colors are used.
  expect_warning(
    res0 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL)
  )

  sd0[[g]][!is.na(sd0[[g]])] <-
    sample(x = 10, size = sum(!is.na(sd0[[g]])),
           replace = TRUE) # for <= 10 groups,
                           # dataquieR standard colors are used.
  expect_warning(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL)
  )

  g <- ggplot2::ggplot_build(res1$SummaryPlotList$Loess_fits_combined)
  got1 <- sort(unique(g$data[[1]][["colour"]]))
  hex_code <- sort(c( # the dataquieR colors
    "#000000", "#B0B0B0", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
  ))
  expect_equal(got1, hex_code)

  g <- ggplot2::ggplot_build(res0$SummaryPlotList$Loess_fits_combined)
  got0 <- sort(unique(g$data[[1]][["colour"]]))
  ggs_default <- sort(ggplot2::scale_color_discrete()$palette(11))

  expect_equal(got0, ggs_default)
})
