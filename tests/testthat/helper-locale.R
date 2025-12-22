locale_available <- function(locale, category = "LC_TIME") {
  old <- Sys.getlocale(category)
  on.exit(suppressWarnings(Sys.setlocale(category, old)), add = TRUE)

  res <- suppressWarnings(
    tryCatch(
      Sys.setlocale(category, locale),
      warning = function(e) "",
      error   = function(e) ""
    )
  )

  !identical(res, "") && !is.na(res)
}

tz_available <- function(tz) {
  tz %in% OlsonNames()
}

has_valid_system_tz <- function() {
  tz <- attr(Sys.time(), "tzone")
  !is.null(tz) && nzchar(tz) && all(tz %in% OlsonNames())
}

require_english_locale_and_berlin_tz <- function(
    locale_candidates = c("en_US.UTF-8", "English.UTF-8"),
    tz = "Europe/Berlin",
    skip_on_cran = TRUE
) {
  if (isTRUE(skip_on_cran)) {
    testthat::skip_on_cran()
  }

  english_locale <- NULL
  for (loc in locale_candidates) {
    if (locale_available(loc, "LC_TIME")) {
      english_locale <- loc
      break
    }
  }

  if (is.null(english_locale)) {
    testthat::skip(
      sprintf(
        "No English UTF-8 LC_TIME locale available (tried: %s)",
        paste(locale_candidates, collapse = ", ")
      )
    )
  }

  if (!tz_available(tz)) {
    testthat::skip(
      sprintf("Timezone '%s' not available on this platform", tz)
    )
  }

  caller <- parent.frame()
  withr::local_locale(c(LC_TIME = english_locale), .local_envir = caller)
  withr::local_timezone(tz, .local_envir = caller)

  invisible(list(locale = english_locale, tz = tz))
}
