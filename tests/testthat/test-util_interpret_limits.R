test_that("posix offset is 1970-01-01 01:00:00 CET)", {
  Sys.setenv(TZ = 'CET')
  expect_equal(as.POSIXct(as.numeric(as.POSIXct("1975-12-16 03:46:00 CET")),
                          origin = min(as.POSIXct(Sys.Date()), 0)),
               as.POSIXct("1975-12-16 03:46:00 CET"))
})
test_that("util_interpret_limits works", {
  Sys.setenv(TZ = 'CET')
  meta <- prep_create_meta(
    VAR_NAMES = 1:26,
    DATA_TYPE = c(rep(DATA_TYPES$INTEGER, 13), rep(DATA_TYPES$FLOAT, 9),
                  rep(DATA_TYPES$DATETIME, 4)),
    LABEL = LETTERS,
    MISSING_LIST = ""
  )
  expect_error(util_interpret_limits(meta), regexp =
                 "No column containing the term LIMIT")
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]] <- NA
  expect_warning(util_interpret_limits(meta), regexp =
                   "HARD_LIMITS has no defined intervals and is omitted.")
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]] <- "xx"
  expect_warning(util_interpret_limits(meta), regexp =
                   "Found invalid limits for .HARD_LIMITS.: .* will ignore these",
                 perl = TRUE)
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]] <- NA
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][1] <- "[0; 10)"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][2] <- "[0;Inf)"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][3] <- "(0; 10)"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][4] <- "[0; 10]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][5] <- "[-Inf; 0]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][6] <- "(-Inf; Inf]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][7] <- "(0.1; 13.324]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][8] <- "(0.1; 13324.0]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][23] <- "(2020-01-01 00:00:00 CET; Inf]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][24] <- "(+Inf; 2019-09-09 00:00:00 CET]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][25] <- "(2020-01-01 00:00:00 CET; ]"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][26] <- "(+Inf; ]"
  m2 <- util_interpret_limits(meta)
  a <- m2[, c("HARD_LIMIT_LOW", "HARD_LIMIT_UP", "INCL_HARD_LIMIT_LOW",
              "INCL_HARD_LIMIT_UP")]
  b <- dplyr::tribble(
    ~HARD_LIMIT_LOW, ~HARD_LIMIT_UP, ~INCL_HARD_LIMIT_LOW, ~INCL_HARD_LIMIT_UP,
    0, 10, TRUE, FALSE,
    0, Inf, TRUE, FALSE,
    0, 10, FALSE, FALSE,
    0, 10, TRUE, TRUE,
    -Inf, 0, TRUE, TRUE,
    -Inf, Inf, FALSE, TRUE,
    0.1, 13.324, FALSE, TRUE,
    0.1, 13324, FALSE, TRUE,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    1577833200, Inf, FALSE, TRUE,
    Inf, 1567980000, FALSE, TRUE,
    1577833200, NA, FALSE, TRUE,
    Inf, NA, FALSE, TRUE,
  )
  expect_equivalent(a, b, tolerance = 1e-3)

  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]] <- NA
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][1] <- "[0; 0-0)"
  meta[[WELL_KNOWN_META_VARIABLE_NAMES$HARD_LIMITS]][2] <- "[3-3;Inf)"
  expect_warning(m3 <- util_interpret_limits(meta),
                 regexp =
                   paste0("In util_interpret_limits: Damaged (lower|upper)",
                          ".+HARD_LIMITS.+: .+(3-3|0-0).+ in .+[12].+"),
                 all = TRUE,
                 perl = TRUE
  )
})
