test_that("test_create_descriptive_summ", {
  
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  target <- withr::local_tempdir("testdessummary")

  sd1 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE),
              20)

  expect_message2(
    des_summary(study_data = sd1,
                meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx"))

  sd1 <- sd1[, 4:7]
  desc1 <- des_summary(study_data = sd1)
  expect_equal(sum(as.numeric(desc1$SummaryData$Mean)),
                298.2)

  desc2 <- des_summary(resp_vars = c("v00003", "v00004"),
                   study_data = sd1)
  expect_equal(sum(as.numeric(desc2$SummaryData$Mean)),
                171.75)
  expect_equal(sum(as.numeric(desc2$SummaryData$SD)),
               10.932)
  expect_equal(sum(as.numeric(desc2$SummaryData$CV)),
               13.27)
  expect_equal(sum(as.numeric(desc2$SummaryData$Kurtosis)),
               -2.5740)
  expect_equal(sum(as.numeric(desc2$SummaryData$Median)),
               170.5)

})

test_that("test_create_descriptive_summ_to", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                    keep_types = TRUE)

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  meta_data <- prep_get_data_frame("item_level")

  set.seed(12345)
  day_course <- unlist(lapply(
    0:23,
    function(h) {
      list(
        hms::hms(hours = h,
                 minutes = 3),
        hms::hms(hours = h,
                 minutes = 13),
        hms::hms(hours = h,
                 minutes = 24),
        hms::hms(hours = h,
                 minutes = 30),
        hms::hms(hours = h,
                 minutes = 50)
      )
    }
  ), recursive = FALSE)
  probs <-
    rep(c(.7, .2, .05, .04, 0.01), 24)
  times <- sample(x = day_course,
                  prob = probs,
                  size = nrow(study_data),
                  replace = TRUE)
  study_data$v02000 <- times
  meta_data <- util_rbind(
    meta_data,
    data.frame(
      stringsAsFactors = FALSE,
      VAR_NAMES = "v02000",
      LABEL = "ADMIS_TM_0",
      DATA_TYPE = DATA_TYPES$TIME,
      SCALE_LEVEL = SCALE_LEVELS$INTERVAL,
      VALUE_LABELS = NA_character_,
      STANDARDIZED_VOCABULARY_TABLE = NA_character_,
      MISSING_LIST_TABLE = NA_character_,
      HARD_LIMITS = "[09:00:00;18:00:00]",
      DETECTION_LIMITS = NA_character_,
      SOFT_LIMITS = NA_character_,
      DISTRIBUTION = NA_character_,
      DECIMALS = NA_character_,
      DATA_ENTRY_TYPE = NA_character_,
      GROUP_VAR_OBSERVER = NA_character_,
      GROUP_VAR_DEVICE = NA_character_,
      TIME_VAR = NA_character_,
      STUDY_SEGMENT = "STUDY",
      PART_VAR = "PART_STUDY",
      VARIABLE_ROLE = "intro",
      VARIABLE_ORDER = "54",
      LONG_LABEL = "Admission time",
      ELEMENT_HOMOGENITY_CHECKTYPE = NA_character_,
      UNIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
      N_RULES = "4",
      LOCATION_METRIC = NA_character_,
      LOCATION_RANGE = NA_character_,
      PROPORTION_RANGE = NA_character_,
      REPEATED_MEASURES_VARS = NA_character_,
      REPEATED_MEASURES_GOLDSTANDARD = NA_character_,
      CO_VARS = NA_character_
    )
  )

  r <-
    des_summary(
      resp_vars = "ADMIS_TM_0",
      study_data = study_data,
      label_col = LABEL,
      meta_data = meta_data)

})
