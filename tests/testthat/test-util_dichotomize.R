test_that("multiplication works", {
     load(system.file("extdata/meta_data.RData", package = "dataquieR"),
       envir = environment())
     load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir = environment())
     meta_data[meta_data[[LABEL]] == "EATING_PREFS_0", RECODE] <-
       "0" # 0 = "eat meat" as reference
     meta_data[meta_data[[LABEL]] == "MEAT_CONS_0", RECODE] <-
       "1| 2|3 | 4 " # eats meat more than 0="never"
     m_study_data <- util_replace_codes_by_NA(study_data, meta_data)$study_data
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
                 0, # eat meat on some days
                 1  # nevereats meat
               )
             )
     )
})
