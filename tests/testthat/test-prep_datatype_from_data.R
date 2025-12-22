test_that("prep_datatype_from_data works", {
  skip_on_cran()
  expect_equal(prep_datatype_from_data(cars),
               c(speed = "integer", dist = "integer"))
  expect_equal(prep_datatype_from_data("speed", cars),
               c(speed = "integer"))
  expect_message2(
    expect_equal(prep_datatype_from_data("speedx", cars),
               c(speedx = NA_character_)),
    regexp = paste("The following .+resp_vars.+ are missing from",
                   "the .+study_data.+.",
                   "Won't return a type for them: .+speedx.+"),
    perl = TRUE
  )
  expect_error(prep_datatype_from_data("xx"),
               regexp = paste("Need study data as a data",
                              "frame in the argument .+study_data.+"),
               perl = TRUE)
  expect_error(prep_datatype_from_data(),
               regexp = paste("Need study data as a data",
                              "frame in the argument .+study_data.+"),
               perl = TRUE)
  expect_error(
    prep_datatype_from_data(42, cars),
    regexp =
      paste(".+resp_vars.+ should be missing",
        "or give variable names referring the study_data."
      ),
    perl = TRUE)

  xx <- cars
  class(xx$speed) <- "unknown_class"
  expect_equal(prep_datatype_from_data(xx),
               c(speed = "string", dist = "integer"))

})

test_that("prep_datatype_from_data works with tibble", {
  skip_on_cran()
  skip_if_not_installed("tibble")

  expect_equal(prep_datatype_from_data(tibble::as_tibble(cars)),
               c(speed = "integer", dist = "integer"))
})
