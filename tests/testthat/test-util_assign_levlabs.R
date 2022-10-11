test_that("util_assign_levlabs works", {
     load(system.file("extdata/meta_data.RData", package = "dataquieR"),
       envir = environment())
     load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir = environment())
     vname <- "v50000"
     labelled_var <-
       dataquieR:::util_assign_levlabs(
         variable = study_data[[vname]],
         string_of_levlabs = subset(meta_data,
                                    VAR_NAMES == vname,
                                    VALUE_LABELS,
                                    TRUE),
         splitchar = SPLIT_CHAR,
         assignchar = " = "
       )

     expect_identical(
       table(labelled_var, useNA = "always"),
       structure(c(76L, 2864L, 60L), .Dim = 3L,
         .Dimnames = list(labelled_var = c("no",  "yes", NA)),
         class = "table")
     )
     expect_true(labelled_var[2021] < labelled_var[1])
     expect_true(is.ordered(labelled_var))

     labelled_var1 <-
       dataquieR:::util_assign_levlabs(
         ordered = FALSE,
         variable = study_data[[vname]],
         string_of_levlabs = subset(meta_data,
                                    VAR_NAMES == vname,
                                    VALUE_LABELS,
                                    TRUE),
         splitchar = SPLIT_CHAR,
         assignchar = " = "
       )

     expect_identical(
       table(labelled_var1, useNA = "always"),
       structure(c(76L, 2864L, 60L), .Dim = 3L,
                 .Dimnames = list(labelled_var1 = c("no",  "yes", NA)),
                 class = "table")
     )
     expect_false(is.ordered(labelled_var1))

     variable <- study_data[[vname]]
     string_of_levlabs <- subset(meta_data,
            VAR_NAMES == vname,
            VALUE_LABELS,
            TRUE)
     labelled_var <-
             dataquieR:::util_assign_levlabs(
                     variable = variable,
                     string_of_levlabs = string_of_levlabs,
                     splitchar = SPLIT_CHAR,
                     assignchar = " = "
             )
     non_num_labs <- as.character(labelled_var)
     string_of_levlabs_now <- "no = no | yes = yes"
     labelled_var2 <- util_assign_levlabs(
             variable = non_num_labs,
             string_of_levlabs = string_of_levlabs_now,
             splitchar = SPLIT_CHAR,
             assignchar = " = "
     )
     expect_equal(labelled_var2, labelled_var)
     string_of_levlabs_now <- "no = no | yes"
     expect_warning(invisible(
             util_assign_levlabs(
                     variable = non_num_labs,
                     string_of_levlabs = string_of_levlabs_now,
                     splitchar = SPLIT_CHAR,
                     assignchar = " = "
             )),
             regexp =
                     "No labels assigned for some levels, use levels as labels",
             all = TRUE,
             fixed = TRUE
     )
     string_of_levlabs_now <- "no = no = no"
     expect_warning(invisible(
             util_assign_levlabs(
                     variable = non_num_labs,
                     string_of_levlabs = string_of_levlabs_now,
                     splitchar = SPLIT_CHAR,
                     assignchar = " = "
             )),
             regexp =
          "Number of levels in variable greater than in character string.",
             all = TRUE,
             fixed = TRUE
     )
})
