test_that("util_validate_known_meta works", {
  local({
    load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
         envir = environment())
    md1 <- meta_data
    expect_silent(util_validate_known_meta(meta_data = md1))
    md1$VAR_NAMES <- NULL
    expect_silent(util_validate_known_meta(meta_data = md1))
    md1 <- meta_data
    md1$HARD_LIMITS[[10]] <- "[9; 1]"
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s)",
                     paste("Invalid limits detected: Found HARD with",
                           "lower limit > upper limit: \\[9; 1\\]")),
                   all = TRUE,
                   perl = TRUE)
    md1 <- meta_data
    md1$VALUE_LABELS[[53]] <- "0 = no"
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s)",
                                    paste("Suspicious value labels",
                                          ".only 1 level. detected:",
                                          ".+0 = no.+")),
                   all = TRUE,
                   perl = TRUE)
    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- "x| 9"
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s)",
                                    paste("Suspicious .+MISSING_LIST.+:",
                                          "not numeric: .+x\\| 9.+")),
                   all = TRUE,
                   perl = TRUE)
    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- list(1:10)
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s)",
                                    paste("Suspicious .+MISSING_LIST.+:",
                                          "not numeric: .+list\\(1:10\\).+")),
                   all = TRUE,
                   perl = TRUE)
    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- paste(md1$MISSING_LIST[[44]],
                                    md1$MISSING_LIST[[44]],
                                    collapse = SPLIT_CHAR)
    expect_warning(util_validate_known_meta(meta_data = md1),
                   regexp = sprintf("(%s)",
                                    paste("Duplicates in .+MISSING_LIST.+:",
                                          ".+99980 \\| 99983 \\| 99988 \\|",
                                          "99989 \\| 99991 \\| 99993 \\|",
                                          "99994 \\| 99995 99980 \\| 99983",
                                          "\\| 99988 \\| 99989 \\| 99991",
                                          "\\| 99993 \\| 99994 \\| 99995.+.",
                                          "Maybe another missing code is not",
                                          "listed?")),
                   all = TRUE,
                   perl = TRUE)
  })
})
