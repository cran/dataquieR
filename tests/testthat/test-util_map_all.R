test_that("util_map_all works", {
  skip_on_cran()
  
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = LETTERS,
    MISSING_LIST = "",
    nums = seq_along(letters)
  )
  sd <- as.data.frame(lapply(setNames(nm = letters), paste, 1:10),
                      stringsAsFactors = FALSE)
  mapped <- util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = md
  )

  expect_equal(colnames(mapped$df), LETTERS)

  mapped <- util_map_all(
    label_col = "nums",
    study_data = sd,
    meta_data = md
  )

  expect_error(util_map_all(
      label_col = c("nums", "kkk"),
      study_data = sd,
      meta_data = md
    ),
    regexp =
      paste("label_col must be exactly 1 metadata attribute,",
            "neither a vector nor NULL.")
  )

  expect_error(util_map_all(
    label_col = c(),
    study_data = sd,
    meta_data = md
  ),
  regexp =
    paste("label_col must be exactly 1 metadata attribute,",
          "neither a vector nor NULL.")
  )

  expect_error(util_map_all(
    label_col = "NO NO NO NO NO",
    study_data = sd,
    meta_data = md
  ), regexp = paste("label_col .+NO NO NO NO NO.+ not found in metadata.",
                    "Did you mean .+VAR_NAMES.+"))

  expect_error(util_map_all(
    label_col = "speed",
    study_data = sd,
    meta_data = cars
  ), regexp = paste(".*VAR_NAMES not found in metadata."))

  expect_error(util_map_all(
    label_col = "DATA_TYPE",
    study_data = sd,
    meta_data = md
  ), regexp =
    paste("The following .+DATA_TYPE.+ are duplicated in the",
          "metadata and cannot be used as label therefore: .+float.+"))

  mdx <- md
  mdx$VAR_NAMES[[5]] <- mdx$VAR_NAMES[[1]]
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp =
    paste("The following variable names are duplicated in the",
          "metadata and cannot be used as label therefore: .+a.+"))

  mdx <- md
  mdx$VAR_NAMES[[5]] <- NA
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp = paste("For the following variables, some variable names are",
                    "missing in the metadata: Variable No. #5"))

  mdx <- md
  mdx$LABEL[[5]] <- NA
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp = paste("For the following variables, some .+LABEL.+ are missing",
                    "in the metadata and cannot be used as label therefore:",
                    "Variable No. #5"))

  mdx <- md
  mdx$VAR_NAMES[[1]] <- "xxx"
  mdx$VAR_NAMES[[4]] <- "yyy"

  withr::with_options(list(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact"),
                      expect_warning(
    expect_warning(
      expect_message2(
        expect_message2(
          invisible(capture.output(util_map_all(
            label_col = LABEL,
            study_data = sd,
            meta_data = mdx
          ))),
          regexp = paste("Did not find any metadata for the following",
                          "variables from the study data: .+a.+, .+d.+"),
        perl = TRUE),
      regexp = paste("Found metadata for the following variables not",
                     "found in the study data: .+xxx.+, .+yyy.+"),
      perl = TRUE),
      regexp = paste("Lost 7.7% of the study data because",
                     "of missing/not assignable metadata"),
      perl = TRUE
    ),
    regexp = paste("Lost 7.7% of the metadata because of",
                   "missing/not assignable study data"),
    perl = TRUE
  ))

  mdx <- md
  mdx$LABEL[[5]] <- "    "
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp = paste("Mapping of metadata on study data yielded invalid",
                    "variable labels:"))

  expect_equal(colnames(mapped$df), as.character(seq_along(letters)))

})
