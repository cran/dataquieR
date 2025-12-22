util_is_time_only <- function(x, na_ok = TRUE) { # TODO: See also in redcap-rules-env
  # Already "pure time" classes
  if (inherits(x, "hms")) return(TRUE)
  if (inherits(x, "times")) return(TRUE)                  # chron::times

  # Character strings that parse cleanly as time-of-day
  if (is.character(x)) {
    ok <- !is.na(suppressWarnings(readr::parse_time(x)))
    return(if (na_ok) all(ok | is.na(x)) else all(ok))
  }

  # Numeric that clearly represents time-of-day (seconds or Excel fractions)
  if (is.numeric(x)) {
    # Treat values in [0, 1) as Excel day fractions (time of day),
    # otherwise treat as seconds modulo 24h
    v <- x[!is.na(x)]
    looks_like_fraction <- length(v) && all(v >= 0 & v < 1)
    if (looks_like_fraction) return(TRUE)
    # Seconds in a reasonable range? accept as time of day too.
    if (length(v) && all(v >= 0 & v < 86400*2)) return(TRUE)  # allow 0..<2 days
  }

  if (is.list(x)) {
    return(all(vapply(x, util_is_time_only, na_ok = na_ok,
                      FUN.VALUE = logical(1))))
  }

  # POSIXt: has a date, but can be reduced to time-of-day
  inherits(x, "POSIXt") && !is.null(x)
}

# ------------- Coercion to hms ----------------------------------------------

util_as_time_only <- function(x, tz = "UTC") { # TODO: See util_parse_time
  # Normalize any “time-ish” input to hms::hms (seconds since midnight).
  sec_per_day <- 86400

  if (inherits(x, "hms")) return(x)

  if (inherits(x, "times")) {              # chron::times -> hms
    return(hms::as_hms(as.numeric(x) * sec_per_day))
  }

  if (inherits(x, "POSIXt")) {
    # Extract time-of-day in given tz
    tt <- as.POSIXlt(x, tz = tz)
    secs <- tt$hour * 3600 + tt$min * 60 + floor(tt$sec)
    return(hms::as_hms(secs %% sec_per_day))
  }

  if (inherits(x, "difftime")) {
    return(hms::as_hms(as.numeric(x, units = "secs") %% sec_per_day))
  }

  if (is.character(x)) {
    # Robust parser from readr -> hms
    return(suppressWarnings(readr::parse_time(x)))
  }

  if (is.factor(x)) {
    return(hms::as_hms(as.character(x)))
  }

  if (is.numeric(x)) {
    # Two conventions:
    # - [0,1) -> Excel fraction of a day
    # - else  -> seconds since midnight (or arbitrary seconds -> modulo 24h)
    v <- x
    is_frac <- !is.na(v) & v >= 0 & v < 1
    secs <- ifelse(is_frac, v * sec_per_day, v)
    return(hms::as_hms(secs %% sec_per_day))
  }

  if (is.list(x)) {
    return(lapply(x, function(x) {
      r <- NA
      try(
        r <- util_as_time_only(x, tz = tz),
        silent = TRUE
      )
      return(r)
    }))
  }

  # Logical, lists, etc. -> try character fallback
  hms::as_hms(as.character(x))
}
