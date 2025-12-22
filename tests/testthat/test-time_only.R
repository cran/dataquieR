test_that("Time-only variables general", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("stringdist")

  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                    keep_types = TRUE)

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  meta_data <- prep_get_data_frame("item_level")

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
      VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
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
      CO_VARS = NA_character_,
      MISSING_LIST = "00:00:00 = not available"
    )
  )

  compute_me <- function() {
    set.seed(12345)
    r <- dq_report2(
      resp_vars = "ADMIS_TM_0",
      study_data = study_data, label_col = LABEL,meta_data = meta_data,
      filter_indicator_functions =
        c("^com_item_missingness$",
          "^con_limit_deviations$"),
      filter_result_slots =
        c("^SummaryTable$"),
      cores = NULL,
      meta_data_v2 =
        "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
    r
  }

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
  times[sample(seq_along(times),
               size = length(times) %/% 100,
               replace = TRUE)] <-
    list(hms::hms(seconds = 0, minutes = 0, hours = 0))

  study_data$v02000 <- times

  r <- compute_me()

  expect_true(is.list(attr(r, "integrity_issues_before_pipeline")) &&
                length(attr(r, "integrity_issues_before_pipeline")) == 0)

  expect_length(r, 2)

  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Missing codes N`,
               30)
  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Sysmiss N`, 0)
  expect_equal(r$con_limit_deviations.ADMIS_TM_0$SummaryTable$NUM_con_rvv_inum,
               1865)

  times <- sample(x = day_course,
                  prob = probs,
                  size = nrow(study_data),
                  replace = TRUE)
  # introduce some missing
  times[sample(seq_along(times),
               size = length(times) %/% 100,
               replace = TRUE)] <-
    hms::hms(seconds = 0, minutes = 0, hours = 0)

  study_data$v02000 <- times

  r <- compute_me()

  expect_true(is.list(attr(r, "integrity_issues_before_pipeline")) &&
                length(attr(r, "integrity_issues_before_pipeline")) == 1)

  expect_length(r, 2)

  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Missing codes N`,
               30)
  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Sysmiss N`, 0)
  expect_equal(r$con_limit_deviations.ADMIS_TM_0$SummaryTable$NUM_con_rvv_inum,
               1879)


  times[c(4, 6)] <-
    cars
  times[[17]] <- iris
  study_data$v02000 <- times

  r <- compute_me()

  expect_true(is.list(attr(r, "integrity_issues_before_pipeline")) &&
                length(attr(r, "integrity_issues_before_pipeline")) == 2)

  expect_length(r, 2)

  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Missing codes N`,
               30)
  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Sysmiss N`, 3)
  expect_equal(r$con_limit_deviations.ADMIS_TM_0$SummaryTable$NUM_con_rvv_inum,
               1876)

  skip_if_not_installed("chron")

  probs <-
    rep(c(.7, .2, .05, .04, 0.01), 24)
  times <- sample(x = day_course,
                  prob = probs,
                  size = nrow(study_data),
                  replace = TRUE)
  times[sample(seq_along(times),
               size = length(times) %/% 100,
               replace = TRUE)] <-
    list(hms::hms(seconds = 0, minutes = 0, hours = 0))

  times <-
    get("as.times", envir = asNamespace("chron"))(util_as_character(times))

  study_data$v02000 <- times

  r <- compute_me()

  expect_true(is.list(attr(r, "integrity_issues_before_pipeline")) &&
                length(attr(r, "integrity_issues_before_pipeline")) == 0)

  expect_length(r, 2)

  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Missing codes N`,
               30)
  expect_equal(r$com_item_missingness.ADMIS_TM_0$SummaryTable$`Sysmiss N`, 0)
  expect_equal(r$con_limit_deviations.ADMIS_TM_0$SummaryTable$NUM_con_rvv_inum,
               1879)


})
