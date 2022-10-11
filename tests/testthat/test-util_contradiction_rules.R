study_data <- data.frame(
  age_at_baseline = c(20, 23, 34, 36, 34, 20, 19, 18),
  age_at_followup = 2 + c(20, 23, 34, 36, 34, 20, 19, 18),
  sex_at_baseline = c("m", "f", "m", "m", "m", "f", "m", "m"),
  sex_at_followup = c("m", "f", "m", "m", "m", "m", "m", "f"),
  income_class_bl = ordered(
    c(10, 10, 10, 20, 20, 30, 30, 30),
    levels = c(10, 20, 30),
    labels = c("low", "medium", "high")
  ),
  income_class_fu = ordered(
    c(10, 10, 10, 20, 10, 20, 30, 30),
    levels = c(30, 20, 10),
    labels = c("low", "medium", "high")
  ),
  worst_grade = c("1", "2", "3", "2", "2", "2", "3", "5"),
  best_grade = c("1", "1", "1", "2", "1", "1", "2", "4"),
  piercing_date = c(NA, as.POSIXct("2020-01-01"), NA, NA, NA, NA, NA, NA),
  piercing_type = c(NA, "12", NA, NA, NA, NA, NA, NA),
  mrt_date = c(as.POSIXct("2020-01-01"), NA,
               as.POSIXct("2020-01-01"),
               as.POSIXct("2020-01-01"),
               as.POSIXct("2020-01-01"),
               as.POSIXct("2020-01-01"),
               as.POSIXct("2020-01-01"),
               as.POSIXct("2020-01-01")),
  body_size = c(180, 150, 179, 178, 150, 150, 190, 200),
  stringsAsFactors = TRUE
)

test_that("A_not_equal_B_vv works", {
  expect_equal(
    A_not_equal_B_vv(study_data = study_data,
                     A = "sex_at_baseline",
                     B = "sex_at_followup"
                    ),
    c(0, 0, 0, 0, 0, 1, 0, 1))
})
test_that("A_less_than_B_vv works", {
  expect_equal(
    A_less_than_B_vv(study_data = study_data,
                     A = "age_at_followup",
                     B = "age_at_baseline")
    ,  c(0, 0, 0, 0, 0, 0, 0, 0))
  expect_warning(A_less_than_B_vv(study_data = study_data,
                                  A = "sex_at_baseline",
                                  B = "sex_at_followup"),
                 regexp = "Could not convert .* to numeric values")
  expect_warning(A_less_than_B_vv(study_data = study_data,
                   A = "sex_at_baseline",
                   B = "sex_at_followup"),
               regexp =
                      "Don't know, how to compare values of .+? (.+factor.+)")
  expect_equal(A_less_than_B_vv(study_data = study_data,
                   A = "income_class_fu",
                   B = "income_class_bl"),
               c(0, 0, 0, 0, 0, 1, 1, 1))
  expect_warning(
    expect_equal(A_less_than_B_vv(study_data = study_data,
                   A = "worst_grade",
                   B = "best_grade"),
               c(0, 0, 0, 0, 0, 0, 0, 0)),
    regexp =
      paste0("Don't know, how to compare values of .+worst_grade.+",
             "(.+factor.+, .+integer.+)", collapse = " ")
  )
})

test_that("A_less_equal_B_vv works", {
  expect_equal(
    A_less_equal_B_vv(study_data = study_data,
                     A = "age_at_followup",
                     B = "age_at_baseline")
    ,  c(0, 0, 0, 0, 0, 0, 0, 0))
  expect_warning(
    A_less_equal_B_vv(study_data = study_data,
                                   A = "sex_at_baseline",
                                   B = "sex_at_followup"),
    regexp = "Could not convert .* to numeric values")

  expect_warning(
    A_less_equal_B_vv(study_data = study_data,
                                  A = "sex_at_baseline",
                                  B = "sex_at_followup"),
    regexp = "Don't know, how to compare values of .+? (.+factor.+)"
  )
  expect_equal(A_less_equal_B_vv(study_data = study_data,
                                A = "income_class_fu",
                                B = "income_class_bl"),
               c(0, 0, 0, 1, 0, 1, 1, 1))
  expect_warning(
    expect_equal(A_less_equal_B_vv(study_data = study_data,
                                  A = "worst_grade",
                                  B = "best_grade"),
                 c(1, 0, 0, 1, 0, 0, 0, 0)),
    regexp =
      paste0("Don't know, how to compare values of .+worst_grade.+",
             "(.+factor.+, .+integer.+)", collapse = " ")
  )
})


test_that("A_greater_than_B_vv works", {
  expect_equal(
    A_greater_than_B_vv(study_data = study_data,
                      A = "age_at_followup",
                      B = "age_at_baseline")
    ,  c(1, 1, 1, 1, 1, 1, 1, 1))
  expect_warning(
    A_greater_than_B_vv(study_data = study_data,
                                     A = "sex_at_baseline",
                                     B = "sex_at_followup"),
    regexp = "Could not convert .* to numeric values")

  expect_warning(
    A_greater_than_B_vv(study_data = study_data,
                                   A = "sex_at_baseline",
                                   B = "sex_at_followup"),
    regexp = "Don't know, how to compare values of .+? (.+factor.+)"
  )
  expect_equal(A_greater_than_B_vv(study_data = study_data,
                                 A = "income_class_fu",
                                 B = "income_class_bl"),
               c(1, 1, 1, 0, 1, 0, 0, 0))
  expect_warning(
    expect_equal(A_greater_than_B_vv(study_data = study_data,
                                   A = "worst_grade",
                                   B = "best_grade"),
                 c(0, 1, 1, 0, 1, 1, 1, 1)),
    regexp =
      paste("Don't know, how to compare values of .+worst_grade.+",
             "(.+factor.+, .+integer.+)", collapse = " ")
  )
})


test_that("A_greater_equal_B_vv works", {
  expect_equal(
    A_greater_equal_B_vv(study_data = study_data,
                     A = "age_at_followup",
                     B = "age_at_baseline")
    ,  c(1, 1, 1, 1, 1, 1, 1, 1))
  expect_warning(
    A_greater_equal_B_vv(study_data = study_data,
                                      A = "sex_at_baseline",
                                      B = "sex_at_followup"),
    regexp = "Could not convert .* to numeric values")

  expect_warning(
    A_greater_equal_B_vv(study_data = study_data,
                                  A = "sex_at_baseline",
                                  B = "sex_at_followup"),
    regexp = "Don't know, how to compare values of .+? (.+factor.+)"
  )
  expect_equal(A_greater_equal_B_vv(study_data = study_data,
                                A = "income_class_fu",
                                B = "income_class_bl"),
               c(1, 1, 1, 1, 1, 0, 0, 0))
  expect_warning(
    expect_equal(A_greater_equal_B_vv(study_data = study_data,
                                  A = "worst_grade",
                                  B = "best_grade"),
                 c(1, 1, 1, 1, 1, 1, 1, 1)),
    regexp =
      paste0("Don't know, how to compare values of .+worst_grade.+",
             "(.+factor.+, .+integer.+)", collapse = " ")
  )
})

test_that("A_present_not_B_vv works", {
  expect_equal(
    A_present_not_B_vv(study_data = study_data,
                         A = "piercing_date",
                         B = "piercing_type")
    ,  c(0, 0, 0, 0, 0, 0, 0, 0))
  expect_equal(
    A_present_not_B_vv(study_data = study_data,
                       A = "piercing_date",
                       B = "mrt_date")
    ,  c(0, 1, 0, 0, 0, 0, 0, 0))
})

test_that("A_present_and_B_vv works", {
  expect_equal(
    A_present_and_B_vv(study_data = study_data,
                       A = "piercing_date",
                       B = "mrt_date")
    ,  c(0, 0, 0, 0, 0, 0, 0, 0))
  expect_equal(
    A_present_and_B_vv(study_data = study_data,
                       A = "piercing_date",
                       B = "piercing_type")
    ,  c(0, 1, 0, 0, 0, 0, 0, 0))
})


test_that("A_present_and_B_levels_vl works", {
  expect_equal(
    A_present_and_B_levels_vl(study_data = study_data,
                     A = "piercing_date",
                     B = "piercing_type",
                     B_levels = "none")
    , c(0, 0, 0, 0, 0, 0, 0, 0))

  expect_equal(
    A_present_and_B_levels_vl(study_data = study_data,
                              A = "piercing_date",
                              B = "piercing_type",
                              B_levels = "12")
    , c(0, 1, 0, 0, 0, 0, 0, 0))


})

test_that("A_levels_and_B_levels_ll works", {
  expect_equal(
    A_levels_and_B_levels_ll(study_data = study_data,
                              A = "sex_at_baseline",
                              A_levels = "f",
                              B = "income_class_bl",
                              B_levels = "high")
    , c(0, 0, 0, 0, 0, 1, 0, 0))

})

test_that("A_levels_and_B_gt_value_lc works", {
  expect_equal(
    A_levels_and_B_gt_value_lc(study_data = study_data,
                             A = "sex_at_baseline",
                             A_levels = "f",
                             B = "body_size",
                             B_value = 185)
    , c(0, 0, 0, 0, 0, 0, 0, 0))

})

test_that("A_levels_and_B_lt_value_lc works", {
  expect_equal(
    A_levels_and_B_lt_value_lc(study_data = study_data,
                               A = "sex_at_baseline",
                               A_levels = "m",
                               B = "body_size",
                               B_value = 180)
    , c(0, 0, 1, 1, 1, 0, 0, 0))

})
