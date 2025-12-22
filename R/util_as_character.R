#' like as.character, but S3-aware for time classes (`hms`, `difftime`, `chron::times`)
#'
#' @param x  object to convert
#' @param digits.secs integer, passed via options(digits.secs) to control fractional seconds
#' @param tz optional `tz` for `POSIXt` formatting (if `NULL`, use default printing)
#' @param ... ignored
#' @return character vector (length(x)), never recurses
#' @noRd
util_as_character <- function(x, digits.secs = getOption("digits.secs", 3L), tz = NULL, ...) {
  old_dsec <- getOption("digits.secs")
  on.exit(options(digits.secs = old_dsec), add = TRUE)
  options(digits.secs = digits.secs)
  fmt_hms_vec <- function(v) {
    # v may be hms or numeric seconds
    if (inherits(v, "hms")) {
      return(.util_format_hms(v))
    }
    if (inherits(v, "difftime")) {
      return(.util_format_hms(hms::as_hms(v)))
    }
    if (inherits(v, "times")) { # chron::times -> fraction of day
      return(.util_format_hms(hms::as_hms(as.numeric(v) * 86400)))
    }
    if (is.numeric(v)) {
      return(.util_format_hms(hms::as_hms(v)))
    }
    # fallback
    return(as.character(v))
  }

  # Fast paths (vectorised) ----------------------------------------------------
  if (inherits(x, "hms")) {
    r <- .util_format_hms(x)
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (inherits(x, "difftime")) {
    r <- .util_format_hms(hms::as_hms(x))
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (inherits(x, "times")) {
    r <- .util_format_hms(hms::as_hms(as.numeric(x) * 86400))
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (inherits(x, "POSIXt")) {
    if (is.null(tz)) {
      r <- format(x)
    } else {
      r <- format(x, tz = tz, usetz = TRUE)
    }
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (is.factor(x))   return(as.character(x))
  if (is.character(x)) return(x)
  if (is.numeric(x))  return(as.character(x))
  if (is.logical(x))  return(as.character(x))
  if (length(x) == 0L) return(character(0))

  # List-handling (ohne Rekursion) --------------------------------------------
  if (is.list(x)) {
    # Check for inhomogeneous value formats (hms "vectors")
    ..is_hms <- vapply(x, inherits, "hms", FUN.VALUE = logical(1))
    ..is_na1 <- lapply(x, function(x) {
      r <- FALSE
      try(r <- length(x) == 1 && is.na(x), silent = TRUE)
      r
    })
    if (any(..is_hms) && !all(..is_hms)) {
      varname <- attr(x, "..cn")
      vn <- dQuote(varname)
      if (length(vn) == 0)
        vn <- "data"
      x[!..is_hms] <-
        lapply(x[!..is_hms], function(x) {
          if (length(x) != 1) {
            r <- hms::as_hms(NA_character_)
          } else {
            r <- try(hms::as_hms(x), silent = TRUE)
            if (util_is_try_error(r)) {
              r <- hms::as_hms(NA_character_)
            }
          }
          r
        })
      ..is_na2 <- lapply(x, function(x) {
        r <- FALSE
        try(r <- length(x) == 1 && is.na(x), silent = TRUE)
        r
      })
      lost <- sum(!vapply(mapply(..is_na1, ..is_na2, FUN = identical,
                                 SIMPLIFY = FALSE), FUN = identity,
                          FUN.VALUE = logical(1)))
      if (!.called_in_pipeline2()) {
        if (lost > 0) {
          .cnd <- util_warning
        } else {
          .cnd <- util_message
        }
        .cnd(
          # TODO: This warning is thrown several times, sometimes (if study data cache is active, it is missed)
          c("Found %d non-hms %s values in %s -- %d of which",
            "could not be casted to hms"),
          sum(!..is_hms),
          sQuote(DATA_TYPES$TIME),
          vn,
          lost,
          integrity_indicator = "int_vfe_inhom",
          varname = varname)
      } else {
        .cnd <- util_message
        .cnd(
          c("Found %d non-hms %s values in %s"),
          sum(!..is_hms),
          sQuote(DATA_TYPES$TIME),
          vn,
          integrity_indicator = "int_vfe_inhom",
          varname = varname)
      }
    }

    # Viele data.frame-Spalten mit hms sind tatsächlich Listen von Skalar-hms.
    # Wir formatieren elementweise, aber rufen NICHT wieder util_as_character() auf.
    # out <- lapply(
    #   x,
    #   function(e) {
    #     if (is.null(e) || length(e) == 0L) return(NA_character_)
    #     if (inherits(e, "hms"))       return(.util_format_hms(e))
    #     if (inherits(e, "difftime"))  return(.util_format_hms(hms::as_hms(e)))
    #     if (inherits(e, "times"))     return(.util_format_hms(hms::as_hms(as.numeric(e) * 86400)))
    #     if (inherits(e, "POSIXt")) {
    #       if (is.null(tz)) return(format(e))
    #       return(format(e, tz = tz, usetz = TRUE))
    #     }
    #     if (is.factor(e))    return(as.character(e))
    #     if (is.character(e)) return(e)
    #     if (is.numeric(e))   return(as.character(e))
    #     if (is.list(e))      return("<list>")  # Schutz vor verschachtelten Listen → keine Rekursion!
    #     return(as.character(e))
    #   }
    # )
    # if (!all(vapply(out, is.character, FUN.VALUE = logical(1)))) {
    #   save(out, file = "/tmp/xx")
    #   stop()
    # }
    out <- lapply(
      x,
      function(e) {
        if (is.null(e) || length(e) == 0L) return(NA_character_)
        if (inherits(e, "hms"))       {
          r <- .util_format_hms(e)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (inherits(e, "difftime"))  {
          r <- .util_format_hms(hms::as_hms(e))
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (inherits(e, "times"))     {
          r <- .util_format_hms(hms::as_hms(as.numeric(e) * 86400))
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (inherits(e, "POSIXt")) {
          if (is.null(tz)) r <- format(e)
          r <- format(e, tz = tz, usetz = TRUE)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (is.factor(e))    {
          r <- as.character(e)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (is.character(e)) {
          return(e)
        }
        if (is.numeric(e)) {
          r <- as.character(e)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (is.list(e)) {
          return(NA_character_)  # Schutz vor verschachtelten Listen → keine Rekursion!
        }
        r <- as.character(e)
        r[trimws(r) == "NA"] <- NA_character_
        return(r)
      })
    out[vapply(out, length, FUN.VALUE = integer(1)) != 1] <-
      NA_character_
    out <- vapply(
      out,
      FUN = identity,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
    return(out)
  }

  # Fallback -------------------------------------------------------------------
  as.character(x)
}

.util_format_hms <- function(...) {
  r <- ..util_format_hms(...)
  if (identical(r, "hms()")) {
    r <- character(0)
  }
  r
}

..util_format_hms <- utils::getS3method("format", "hms",
                                        envir = asNamespace("hms"))
