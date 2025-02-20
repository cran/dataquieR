test_that("util_dichotomize works", {
  skip_on_cran() # TODO: activate?
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data[meta_data[[LABEL]] == "EATING_PREFS_0", RECODE_CONTROL] <-
    "0" # 0 = "eat meat" as reference
  meta_data[meta_data[[LABEL]] == "MEAT_CONS_0", RECODE_CASES] <-
    "1| 2|3 | 4 " # eats meat more frequently than 0="never"
  meta_data[meta_data[[LABEL]] == "SBP_0", RECODE_CASES] <-
    "(-Inf;100] |[140;Inf)"
  meta_data[meta_data[[LABEL]] == "SBP_0", RECODE_CONTROL] <-
    "[120;130)"
  m_study_data <- util_replace_codes_by_NA(study_data, meta_data)
  d_study_data <- util_dichotomize(m_study_data, meta_data)
  x <- d_study_data[,
                    prep_map_labels(c("EATING_PREFS_0", "MEAT_CONS_0"),
                                    from = LABEL, to = VAR_NAMES, meta_data = meta_data)]
  expect_equal(
    d_study_data[["v00022"]],
    ifelse(is.na(m_study_data[["v00022"]]),
           NA,
           ifelse(m_study_data[["v00022"]] %in% 0,
                  0, # eat meat
                  1  # veg*
           )
    )
  )
  expect_equal(
       d_study_data[["v00023"]],
       ifelse(is.na(m_study_data[["v00023"]]),
               NA,
         ifelse(m_study_data[["v00023"]] %in% 1:4,
                 1, # eats meat on some days
                 0  # never eats meat
               )
             )
     )
  col_int <- NA
  col_int[which(m_study_data[["v00004"]] <= 100)] <- 1
  col_int[which(m_study_data[["v00004"]] >= 140)] <- 1
  col_int[which(m_study_data[["v00004"]] >= 120 &
                  m_study_data[["v00004"]] < 130)] <- 0
  expect_equal(d_study_data[["v00004"]],
               col_int)
})

test_that("util_dichotomize is robust", {
  skip_on_cran() # TODO: activate?
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")

  # with wrong input for the recoding, the function returns the original data
  meta_data[meta_data[[LABEL]] == "EATING_PREFS_0", RECODE_CONTROL] <-
    "| |"
  meta_data[meta_data[[LABEL]] == "MEAT_CONS_0", RECODE_CASES] <-
    "1| 2|3 | 4 " # eats meat more than 0="never"
  meta_data[meta_data[[LABEL]] == "MEAT_CONS_0", RECODE_CONTROL] <-
    "1 "
  meta_data[meta_data[[LABEL]] == "SBP_0", RECODE_CASES] <-
    "(-Inf;112] |[125;Inf)"
  meta_data[meta_data[[LABEL]] == "SBP_0", RECODE_CONTROL] <-
    "[110;130)"
  m_study_data <- util_replace_codes_by_NA(study_data, meta_data)
  d_study_data <- suppressWarnings(util_dichotomize(m_study_data, meta_data))
  x <- d_study_data[,
                    prep_map_labels(c("EATING_PREFS_0", "MEAT_CONS_0"),
                                    from = LABEL, to = VAR_NAMES, meta_data = meta_data)]
  expect_equal(d_study_data[["v00022"]],
               m_study_data[["v00022"]])
  expect_equal(d_study_data[["v00023"]],
               m_study_data[["v00023"]])
  expect_equal(d_study_data[["v00004"]],
               m_study_data[["v00004"]])

  # ensure that the function can also be applied to ds1
  ds1 <- prep_prepare_dataframes(.study_data = study_data,
                                 .meta_data = meta_data, .label_col = LABEL,
                                 .apply_factor_metadata = TRUE)
  meta_data[meta_data[[LABEL]] == "SBP_0", RECODE_CONTROL] <- "(112;125)"
  meta_data[meta_data[[LABEL]] == "SBP_0", RECODE_CASES] <- ""
  meta_data[meta_data[[LABEL]] == "MEAT_CONS_0", RECODE_CASES] <- "1| 2|3 | 4" # factor from integer variable with integer codes
  meta_data[meta_data[[LABEL]] == "MEAT_CONS_0", RECODE_CONTROL] <- ""
  meta_data[meta_data[[LABEL]] == "EATING_PREFS_0", RECODE_CASES] <- "vegetarian | vegan" # factor from integer variable with recoding specified using value labels
  meta_data[meta_data[[LABEL]] == "EATING_PREFS_0", RECODE_CONTROL] <- ""
  meta_data[meta_data[[LABEL]] == "USR_SOCDEM_0", RECODE_CONTROL] <- "USR_321|USR_247|USR_520" # factor from string variable with recoding specified using value labels
  meta_data[meta_data[[LABEL]] == "USR_SOCDEM_0", RECODE_CASES] <- ""
  d_ds1 <- suppressWarnings(util_dichotomize(ds1, meta_data, label_col = LABEL))
  expect_equal(d_ds1[["MEAT_CONS_0"]],
               ifelse(is.na(m_study_data[["v00023"]]),
                      NA,
                      ifelse(m_study_data[["v00023"]] %in% 1:4,
                             1, # eats meat on some days
                             0  # never eats meat
                      )
               ))
  expect_equal(d_ds1[["EATING_PREFS_0"]],
               ifelse(is.na(m_study_data[["v00022"]]),
                      NA,
                      ifelse(m_study_data[["v00022"]] %in% 0,
                             0, # eat meat
                             1  # veg*
                      )
               ))
  expect_equal(d_ds1[["USR_SOCDEM_0"]],
               ifelse(is.na(ds1[["USR_SOCDEM_0"]]),
                      NA,
                      ifelse(ds1[["USR_SOCDEM_0"]] %in% c("USR_321", "USR_247", "USR_520"),
                             0,
                             1
                      )
               ))
  col_int <- NA
  col_int[which(m_study_data[["v00004"]] <= 112)] <- 1
  col_int[which(m_study_data[["v00004"]] >= 125)] <- 1
  col_int[which(m_study_data[["v00004"]] > 112 &
                  m_study_data[["v00004"]] < 125)] <- 0
  expect_equal(d_ds1[["SBP_0"]],
               col_int)
})
