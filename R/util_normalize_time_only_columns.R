#' Normalize time-only columns to `hms`
#'
#' @description
#' Convert columns that contain *time-only* class values to [`hms::hms()`].
#' Supported time-only classes are:
#' - `chron::times` (fraction of day),
#' - `data.table::ITime` (integer seconds since midnight), and
#' - `calcal::time_of_day` (hour/minute/second record).
#'
#' Columns that are already `hms` are left unchanged. Date-time (`POSIXt`)
#' columns and plain text/numeric columns are not modified.
#'
#' List-columns are handled robustly: if they contain any time-only objects,
#' each element is converted to `hms`; unconvertible scalars become `NA_hms`
#' to avoid downstream failures. Inhomogeneities are reported via
#' `util_message()` / `util_warning()` using integrity indicator
#' `"int_vfe_inhom"`, analogous to existing handlers.
#'
#' @param study_data [data.frame]
#'   Input data frame whose time-only columns should be normalized.
#'
#' @return
#' A modified copy of `study_data` where detected time-only columns are of
#' class `hms`.
#'
#' @examples
#' \dontrun{
#'   d <- data.frame(
#'     t_num   = c(32, 47),
#'     t_chron = chron::times(c("09:10:00", "10:20:30")),
#'     t_text  = c("09:10:00", "10:20:30"),
#'     t_posix = as.POSIXct(c("2020-01-01 09:10:00", "2020-01-01 10:20:30")),
#'     stringsAsFactors = FALSE
#'   )
#'   # Optional if packages are available:
#'   # d$t_itime  <- data.table::as.ITime(c("09:10:00", "10:20:30"))
#'   # d$t_calcal <- calcal::time_of_day(
#'   #   hour = c(9, 10), minute = c(10, 20), second = c(0, 30)
#'   # )
#'   d2 <- util_normalize_time_only_columns(d)
#'   str(d2$t_chron)  # hms
#'   str(d2$t_text)   # character (unchanged)
#'   str(d2$t_posix)  # POSIXct (unchanged)
#' }
#'
#' @noRd
util_normalize_time_only_columns <- function(study_data) {
  util_expect_data_frame(study_data)

  # ---- helpers -------------------------------------------------------------

  is_scalar_na <- function(x) {
    r <- FALSE
    try(r <- length(x) == 1 && is.na(x), silent = TRUE)
    r
  }

  as_hms_na <- function() hms::as_hms(NA_character_)

  # Class predicates
  inherits_times <- function(x) inherits(x, "times")           # chron::times
  inherits_itime <- function(x) inherits(x, "ITime")           # data.table
  inherits_hms   <- function(x) inherits(x, "hms")
  inherits_posix <- function(x) inherits(x, "POSIXt")
  inherits_calcal_tod <- function(x) inherits(x, "time_of_day")  # calcal

  # Convert a single element (possibly from a list-column) to hms
  elem_to_hms <- function(x) {
    if (inherits_hms(x) && length(x) == 1) return(x)
    if (is_scalar_na(x)) return(as_hms_na())

    # chron::times -> fraction of day * 86400
    if (inherits_times(x)) {
      sec <- try(as.numeric(x) * 86400, silent = TRUE)
      if (util_is_try_error(sec) || length(sec) != 1 || is.na(sec)) {
        return(as_hms_na())
      }
      return(hms::hms(sec))
    }

    # data.table::ITime -> integer seconds since midnight
    if (inherits_itime(x)) {
      sec <- try(as.integer(x), silent = TRUE)
      if (util_is_try_error(sec) || length(sec) != 1 || is.na(sec)) {
        return(as_hms_na())
      }
      return(hms::hms(sec))
    }

    # calcal::time_of_day -> record (expect scalar in list-columns)
    if (inherits_calcal_tod(x)) {
      if (length(x) != 1) return(as_hms_na())
      hh <- try(as.numeric(unclass(x)$hour),   silent = TRUE)
      mm <- try(as.numeric(unclass(x)$minute), silent = TRUE)
      ss <- try(as.numeric(unclass(x)$second), silent = TRUE)
      ok <- !(util_is_try_error(hh) | util_is_try_error(mm) |
                util_is_try_error(ss)) &&
        is.finite(hh) && is.finite(mm) && is.finite(ss)
      if (!ok) return(as_hms_na())
      return(hms::hms(h = hh, m = mm, s = ss))
    }

    # Explicitly exclude POSIXt (date-time) from "time-only"
    if (inherits_posix(x)) return(as_hms_na())

    # Conservative extra: numeric seconds or difftime scalars are acceptable
    if (length(x) == 1 && (is.numeric(x) || inherits(x, "difftime"))) {
      r <- try(hms::as_hms(x), silent = TRUE)
      if (!util_is_try_error(r)) return(r)
    }

    # Everything else -> NA to keep downstream code stable
    as_hms_na()
  }

  # Decide if a whole column counts as "time-only" we should normalize
  is_timeonly_column <- function(x) {
    if (inherits_hms(x))        return(FALSE)
    if (inherits_posix(x))      return(FALSE)
    if (inherits_times(x))      return(TRUE)
    if (inherits_itime(x))      return(TRUE)
    if (inherits_calcal_tod(x)) return(TRUE)
    if (is.character(x))        return(FALSE)
    if (!is.list(x))            return(FALSE)
    any(vapply(
      x,
      function(e)
        inherits_times(e) || inherits_itime(e) || inherits_calcal_tod(e),
      FUN.VALUE = logical(1)
    ))
  }

  emit_log <- function(varname, count_non_hms, lost, is_hms_before) {
    in_pipe <- FALSE
    tmp <- try(.called_in_pipeline2(), silent = TRUE)
    if (!util_is_try_error(tmp)) in_pipe <- isTRUE(tmp)

    vn <- dQuote(if (length(varname)) varname else "data")
    label_time <- if (!is.null(DATA_TYPES$TIME)) sQuote(DATA_TYPES$TIME)
    else sQuote("time")

    if (!in_pipe) {
      .cnd <- if (lost > 0) util_warning else util_message
      .cnd(
        c("Found %d non-hms %s values in %s -- %d of which",
          "could not be casted to hms"),
        count_non_hms, label_time, vn, lost,
        integrity_indicator = "int_vfe_inhom", varname = varname
      )
    } else {
      util_message(
        c("Found %d non-hms %s values in %s"),
        sum(!is_hms_before), label_time, vn,
        integrity_indicator = "int_vfe_inhom", varname = varname
      )
    }
  }

  # ---- main ----------------------------------------------------------------

  for (j in seq_along(study_data)) {
    vname <- names(study_data)[j]
    x <- study_data[[j]]

    if (!is_timeonly_column(x)) next

    # Atomic chron::times vector
    if (inherits_times(x)) {
      sec <- try(as.numeric(x) * 86400, silent = TRUE)
      if (!util_is_try_error(sec)) {
        study_data[[j]] <- hms::hms(sec)
      }
      next
    }

    # Atomic data.table::ITime vector
    if (inherits_itime(x)) {
      sec <- try(as.integer(x), silent = TRUE)
      if (!util_is_try_error(sec)) {
        study_data[[j]] <- hms::hms(sec)
      }
      next
    }

    # Atomic calcal::time_of_day vector (vctrs record)
    if (inherits_calcal_tod(x)) {
      hh <- suppressWarnings(as.numeric(unclass(x)$hour))
      mm <- suppressWarnings(as.numeric(unclass(x)$minute))
      ss <- suppressWarnings(as.numeric(unclass(x)$second))
      ok <- is.finite(hh) & is.finite(mm) & is.finite(ss)
      seconds <- hh * 3600 + mm * 60 + ss
      seconds[!ok] <- NA_real_
      study_data[[j]] <- hms::hms(seconds)
      next
    }

    # List-column containing time-only objects (possibly mixed)
    if (is.list(x)) {
      is_hms_before <- vapply(x, inherits_hms, FUN.VALUE = logical(1))
      is_na_before  <- lapply(x, is_scalar_na)

      x2 <- lapply(x, elem_to_hms)

      is_na_after <- lapply(x2, is_scalar_na)
      lost <- sum(!vapply(
        mapply(is_na_before, is_na_after, FUN = identical, SIMPLIFY = FALSE),
        FUN.VALUE = logical(1), FUN = identity
      ))

      count_non_hms <- sum(!is_hms_before)
      if (count_non_hms > 0) {
        emit_log(vname, count_non_hms, lost, is_hms_before)
      }

      len1 <- vapply(x2, length, integer(1)) == 1L
      if (all(len1)) {
        vals <- suppressWarnings(vapply(
          x2,
          function(e) if (inherits(e, "hms") && length(e) == 1) as.numeric(e)
          else NA_real_,
          numeric(1)
        ))
        study_data[[j]] <- hms::hms(vals)
      } else {
        flat <- try(
          hms::as_hms(unlist(x2, recursive = FALSE, use.names = FALSE)),
          silent = TRUE
        )
        if (!util_is_try_error(flat)) {
          study_data[[j]] <- flat
        } else {
          study_data[[j]] <- x2
        }
      }
      next
    }
  }

  study_data
}
