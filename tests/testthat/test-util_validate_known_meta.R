test_that("util_validate_known_meta works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # TODO: Also test retunred value
  local({
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
    md1 <- meta_data
    expect_silent(util_validate_known_meta(meta_data = md1))
    md1$VAR_NAMES <- NULL
    expect_silent(util_validate_known_meta(meta_data = md1))
    md1 <- meta_data
    md1$HARD_LIMITS[[10]] <- "[9; 1]"
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s|%s)",
                     paste("Invalid limits detected: Found HARD with",
                           "lower limit > upper limit: \\[9; 1\\]"),
          paste("Some code labels or -values are missing from .+meta_data.+")),
                   perl = TRUE)
    md1 <- meta_data
    md1$VALUE_LABELS[[53]] <- "0 = no"
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s|%s)",
                                    paste("Suspicious value labels",
                                          ".only 1 level. detected:",
                                          ".+0 = no.+"),
                                    paste("Some code labels or -values are",
                                          "missing from .+meta_data.+")
                                    ),
                   perl = TRUE)
    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- "x| 9"

    expect_warning(expect_warning(
        util_validate_known_meta(meta_data = md1),
       regexp = paste("Some missing codes are not numeric"),
       perl = TRUE),
     regexp = paste("Suspicious .+MISSING_LIST.+:",
                    "not numeric/date/assignment"),
     perl = TRUE)

    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- list(1:10)
    expect_warning(expect_warning(
      util_validate_known_meta(meta_data = md1),
      regexp = paste("Some missing codes are not numeric"),
      perl = TRUE),
      regexp = paste("Suspicious .+MISSING_LIST.+:",
                     "not numeric/date/assignment"),
      perl = TRUE)

    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- paste(md1$MISSING_LIST[[44]],
                                    md1$MISSING_LIST[[44]],
                                    collapse = SPLIT_CHAR)
    expect_warning(expect_warning(
      util_validate_known_meta(meta_data = md1),
      regexp = paste("Some missing codes are not numeric"),
      perl = TRUE),
      regexp = paste("Duplicates in"),
      perl = TRUE)
  })
})
