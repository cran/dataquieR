test_that("util_parse_assignments works", {
  skip_on_cran()
  expected <- list("1" = "married", "2" = "single", "3" = "divorced",
                   "4" = "widowed")

  util_parse_assignments(
    "1 = married| \n 2 \t = \r  single|   3 =divorced|4=widowed")

  expect_equal(
    util_parse_assignments(
      "1 = married| \n 2 \t = \r  single|   3 =divorced|4=widowed"),
    expected
  )
})

test_that(paste("util_parse_assignments equivalent with an old independent",
                "development from con inadmissible categories"), {
  skip_on_cran()

  meta_data <- prep_get_data_frame("meta_data")

  got <- lapply(lapply(setNames(meta_data[[VALUE_LABELS]], nm =
                                  meta_data$VAR_NAMES),
                util_parse_assignments), unlist) # get for all item

  got <- mapply(lapply(got, names), got, FUN = setNames) # swap names and values

  label_col <- LABEL
  rvs <- meta_data[[LABEL]]

  expected <-
    lapply(
      lapply(
        setNames(meta_data[[VALUE_LABELS]][meta_data[[label_col]] %in% rvs],
                 nm = meta_data[[VAR_NAMES]][meta_data[[label_col]] %in% rvs]),
        function(x) {
          lapply(
            trimws(unlist(strsplit(x, SPLIT_CHAR, fixed = TRUE))),
            function(x) {
              setNames(lapply(
                x,
                function(x) trimws(unlist(strsplit(x, "=", fixed = TRUE))[1])
              ), nm = lapply(
                x,
                function(x) {
                  y <- unlist(strsplit(x, "=", fixed = TRUE))
                  if (length(y) > 1)
                    trimws(paste(y[-1], collapse = "="))
                  else
                    trimws(paste(y, collapse = "=")) # in case of no name, use
                                                     # the value as the name
                }
              ))
            }
          )
        }
      ),
      unlist
    )

  expected[vapply(expected, identical, c("NA" = NA_character_),
                  FUN.VALUE = logical(1))] <- list(NULL)

  expect_equal(got, expected)

})
