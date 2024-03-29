test_that("prep_add_to_meta works", {
  skip_on_cran()
  meta_data <- prep_get_data_frame("meta_data")
  md <- prep_add_to_meta(VAR_NAMES = c("X", "Y"),
                         LABEL = c("x", "y"),
                         DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
                         VALUE_LABELS = c("1 = female | 2 = male", NA),
                         LONG_LABEL = c("Ix", "Ypsilon"),
                         test = 3:4,
                         meta_data = meta_data
                        )
  expect_false("test" %in% colnames(md))
  expect_equal(nrow(md), 55)
  new <- md[md$VAR_NAMES %in% c("X", "Y"),
            c("VAR_NAMES", "LABEL", "DATA_TYPE", "VALUE_LABELS", "LONG_LABEL")]
  expect_identical(new,
                   structure(list(VAR_NAMES = c("X", "Y"), LABEL = c("x", "y"),
                                  DATA_TYPE = c("integer", "float"),
                                  VALUE_LABELS = c("1 = female | 2 = male", NA),
                                  LONG_LABEL = c("Ix", "Ypsilon")),
                             row.names = 54:55, class = "data.frame"))
})
