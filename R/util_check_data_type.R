#' Verify the data type of a value
#'
#' Function to verify the data type of a value.
#'
#' @param x the value
#' @param type expected data type
#' @param check_convertible [logical] also try, if a conversion to the
#'                                    declared data type would work.
#' @param threshold_value [numeric] from=0 to=100. percentage of failing
#'                                  conversions allowed.
#' @param return_percentages [logical] return the percentage of
#'                                mismatches.
#' @param check_conversion_stable [logical] do not distinguish convertible
#'                                          from convertible, but with issues
#' @param robust_na [logical] treat white-space-only-values as `NA`
#' @param vname [character] variable name being checked -- for error messages.
#'
#' @return if `return_percentages`: if not `check_convertible`, the percentage
#'                                of mismatches instead of logical value,
#'                                if `check_convertible`, return a named
#'                                vector with the percentages of all cases
#'                                (names of the vector are
#'                                `match`, `convertible_mismatch_stable`,
#'                                `convertible_mismatch_unstable`,
#'                                `nonconvertible_mismatch`)
#'         if not `return_percentages`: if `check_convertible` is `FALSE`,
#'         [logical] whether `x` is of the expected type
#'         if `check_convertible` is `TRUE`
#'         [integer] with the states `0, 1, 2, 3`: 0 = Mismatch, not convertible
#'                                                 1 = Match
#'                                                 2 = Mismatch, but convertible
#'                                                 3 = Mismatch, convertible,
#'                                                     but with issues (e.g.,
#'                                                     loss of decimal places)
#' @importFrom stats setNames
#'
#' @family data_management
#' @concept data_management
#' @noRd
util_check_data_type <- function(x, type, check_convertible = FALSE,
                                 threshold_value = 0, return_percentages =
                                   FALSE, check_conversion_stable = FALSE,
                                 robust_na = FALSE,
                                 vname = "data") {
  # FIXME: SLOW!!
  hash_id <-
    rlang::hash(list(x,
                type,
                check_convertible,
                check_conversion_stable,
                threshold_value,
                robust_na,
                return_percentages))
  # HINT: if dep on dataframe env: as.list(.dataframe_environment()), also remind
  # global options(). Also remind possible side effects
  if (exists(hash_id, .cache[[".cache"]])) {
    return(get(hash_id, .cache[[".cache"]]))
  }
  if (is.list(x) &&
      any(not_1 <- vapply(x, length, FUN.VALUE = integer(1)) != 1)) {
    varname <- NULL
    if (!missing(vname)) {
      varname <- vname
      vname <- dQuote(vname)
    }
    util_message(
      "Found %d inconsistent data values in %s (length <> 1). Removed.",
      sum(not_1),
      vname,
      integrity_indicator = "int_vfe_inhom",
      varname = varname
      )
    x[not_1] <- NA
  }
  if (robust_na) {
    empty <- util_empty
  } else {
    empty <- is.na
  }
  checks <- setNames(
    list(
      util_is_integer,
      is.numeric,
      is.character,
      lubridate::is.timepoint,
      util_is_time_only
    ),
    nm = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$FLOAT,
      DATA_TYPES$STRING,
      DATA_TYPES$DATETIME,
      DATA_TYPES$TIME
    )
  )

  # function to check whether the data type matches
  .is <- try(checks[[type]], silent = TRUE)
  if (length(.is) != 1 || inherits(.is, "try-error")) {
    util_error("%s is not a known data type.", dQuote(type),
               applicability_problem = TRUE)
  }
  # function to check whether the data type matches (disregarding `NA`s)
  .is_or_na <- function(...) all(empty(...) | .is(...))
  mismatches <- !vapply(x, .is_or_na, FUN.VALUE = logical(1))

  if (length(mismatches) < 1) {
    pct_mismatches <- 0
  } else {
    pct_mismatches <- sum(mismatches) / length(mismatches) * 100
    attr(pct_mismatches, "which") <- mismatches
  }


  if (check_convertible) {
    # perform data type conversion
    x2 <- util_data_type_conversion(x, type)

    if (check_conversion_stable) {
      stable <- util_conversion_stable(x, type)
    } else {
      stable <- rep(TRUE, length(x))
    }

    convertible_mismatch_all <-
      mismatches &
      vapply(x, empty, FUN.VALUE = logical(1)) ==
        vapply(x2, empty, FUN.VALUE = logical(1))

    convertible_mismatch_unstable <- convertible_mismatch_all & !stable
    convertible_mismatch_stable <- convertible_mismatch_all & stable


    pct_convertible_mismatch_stable <-
      sum(convertible_mismatch_stable) /
      length(convertible_mismatch_stable) * 100
    attr(pct_convertible_mismatch_stable, "which") <-
      convertible_mismatch_stable

    pct_convertible_mismatch_unstable <-
      sum(convertible_mismatch_unstable) /
      length(convertible_mismatch_unstable) * 100
    attr(pct_convertible_mismatch_unstable, "which") <-
      convertible_mismatch_unstable

    nonconvertible_mismatch <-
      vapply(x, empty, FUN.VALUE = logical(1)) !=
      vapply(x2, empty, FUN.VALUE = logical(1))
    if (length(nonconvertible_mismatch) < 1) {
      pct_nonconvertible_mismatch <- 0
    } else {
      pct_nonconvertible_mismatch <-
        sum(nonconvertible_mismatch) /
        length(nonconvertible_mismatch) * 100
    }
    attr(pct_nonconvertible_mismatch, "which") <-
      nonconvertible_mismatch

    if (return_percentages) {
      result <- c(
        match = 100 - pct_mismatches,
        convertible_mismatch_stable = pct_convertible_mismatch_stable,
        convertible_mismatch_unstable = pct_convertible_mismatch_unstable,
        nonconvertible_mismatch = pct_nonconvertible_mismatch
      )
      attr(result, "which") <-
        list(
          match = attr(pct_mismatches, "which"),
          convertible_mismatch_stable = attr(pct_convertible_mismatch_stable, "which"),
          convertible_mismatch_unstable = attr(pct_convertible_mismatch_unstable, "which"),
          nonconvertible_mismatch = attr(pct_nonconvertible_mismatch, "which")
        )
    } else {
      if (pct_nonconvertible_mismatch > threshold_value)
        { # NB: Order does matter
        # 0 = Mismatch, not convertible
        result <- 0L
      } else if (pct_mismatches <= threshold_value) {
        # 1 = Match
        result <- 1L
      } else if (pct_nonconvertible_mismatch <= threshold_value) {
        # 2 = Mismatch, but convertible
        result <- 2L
      } else {
        util_error(
          "Internal error in util_check_data_type, sorry, and please report")
      }
    }
  } else {
    if (return_percentages) {
      result <- pct_mismatches
    } else {
      result <- pct_mismatches <= threshold_value
    }
  }

  assign(hash_id, value = result, envir = .cache[[".cache"]])

  return(result)
}
.cache <- new.env(parent = emptyenv()) # HINT: This is exported by dq_reoprt2, but not yet for parallel rendering, which is complicated an not fully working anyways.
util_reset_cache <- function() {
  assign(x = ".cache", value = new.env(parent = emptyenv()), envir = .cache)
}
util_reset_cache()
