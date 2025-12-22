#' Convert a [character()] to a `numeric_with_unit`
#'
#' @param x [character()] to convert/parse
#'
#' @returns `numeric_with_unit`
#' @noRd
util_as_numeric_with_unit <- function(x) {
  n <- trimws(gsub("^([\\d\\.\\-eE\\+, ]+).*$", "\\1", x, perl = TRUE))
  u <- trimws(gsub("^[\\d\\.\\-eE\\+, ]+", "", x, perl = TRUE))
  ok <- try({
    n <- as.numeric(n)
    bu <- util_unit2baseunit(u)
    prefix <- sub(paste0(bu, "$"), "", u)
    if (nzchar(prefix)) {
      fct <- UNIT_PREFIX_FACTORS[prefix]
    } else {
      fct <- 1
    }
    value <- fct * n
    unit <- bu
  }, silent = TRUE)
  if (!util_is_try_error(ok)) {
    return(structure(
      as.numeric(value),
      unit = unit,
      class = "numeric_with_unit"
    ))
  } else {
    util_warning("Could not parse number with unit: %s",
                 dQuote(x))
    return(value)
  }
}

#' Print a number with unit
#'
#' @param x number with unit
#' @param ... not used
#'
#' @returns `invisible(x)`
#' @export
print.numeric_with_unit <- function(x, ...) {
  util_stop_if_not("Intenal error, sorry, please report: Inadmissible call of print.numeric_with_unit" =
                   inherits(x, "numeric_with_unit"))
  cat(format(x))
  if (!!length(attr(x, "unit")) &&
    nzchar(attr(x, "unit"))) cat(" [", attr(x, "unit"), "]", sep = "")
  cat("\n")
  invisible(x)
}

#' Operator caring for units
#'
#' @param e1 first argument
#' @param e2 second argument
#'
#' @returns result
#' @keywords internal
util_op_numeric_with_unit <- function(e1, e2) { # TODO: Vectorize, nicer error messages, better support for 1 + 2%, better maybe some implementations of +.character and similar, either concatenating or summing up controlled by some option()
  # TODO: < > ==
  # assumes, e1 and e2 have been created using util_unit2baseunit(), so
  # unit prefixes have been normalized, already
  # preps
  count_def <- attr(UNIT_IS_COUNT, "def")
  count_def[""] <- 1
  # 1st try to make e1, e2 numeric only, if these have UNIT_IS_COUNT units or are not numeric_with_unit at all.
  is1 <- inherits(e1, "numeric_with_unit")
  is2 <- inherits(e2, "numeric_with_unit")
  if (is1) {
    u1 <- attr(e1, "unit")
    if (!length(u1)) {
      u1 <- ""
    }
    if (!!length(u1) && !is.na(count_def[u1])) {
      e1 <- as.numeric(count_def[u1] * as.numeric(e1))
      u1 <- NULL
    }
  }
  if (is2) {
    u2 <- attr(e2, "unit")
    if (!length(u2)) {
      u2 <- ""
    }
    if (!!length(u2) && !is.na(count_def[u2])) {
      e2 <- as.numeric(count_def[u2] * as.numeric(e2))
      u2 <- NULL
    }
  }

  # unitless stuff may now be plain numbers
  is1 <- inherits(e1, "numeric_with_unit")
  is2 <- inherits(e2, "numeric_with_unit")

  result_u <- NULL

  if (is1 || is2) {
    op <- rlang::call_name(sys.call())
    my_op <- sub("\\.numeric_with_unit", "", op)
    u1 <- attr(e1, "unit")
    if (!length(u1)) {
      u1 <- ""
    }
    u2 <- attr(e2, "unit")
    if (!length(u2)) {
      u2 <- ""
    }
    if (u1 == "") { is1 <- FALSE }
    if (u2 == "") { is2 <- FALSE }
    if (is1 && is2) { # both have a unit
      if (my_op %in% c("*", "/", "%%", "%/%")) {
        if (u1 != u2) {
          util_error("Cannot calclate %s %s %s",
                     u1, my_op, u2)
        }
        if (my_op %in% c("/", "%/%")) {
          result_u <- NULL
        } else if (my_op %in% c("%%")) {
          result_u <- u1
        } else if (my_op %in% c("*")) {
          util_warning("derived units not yet fully supported")
          result_u <- sprintf("%s%s%s", u1, my_op, u2)
        }
      } else if (my_op %in% c("+", "-")) {
        if (u1 != u2) {
          util_error("Cannot calclate %s %s %s",
                     u1, my_op, u2)
        }
        result_u <- u1
      } else if (my_op %in% c("^")) {
        util_error("Cannot calclate %s %s %s",
                   u1, my_op, u2)
      }
    } else { # only one has a real unit
      if (my_op %in% c("*", "/", "%%", "%/%")) {
        # return(NextMethod())
        result_u <- paste0(u1, u2)
      } else if (my_op %in% c("^")) {
        if (u2 == "") {
          util_warning("derived units not yet fully supported")
          result_u <- sprintf("%s^%d", u1, e2)
        } else {
          util_error("Cannot calclate %s %s %s",
                     u1, my_op, u2)
        }
      } else if (my_op %in% c("+", "-")) {
        if (u1 != u2) {
          util_error("Cannot calclate %s %s %s",
                     u1, my_op, u2)
        }
      }
    }
  }
  r <- NextMethod()
  if (is.null(result_u)) {
    r <- as.numeric(r)
  } else {
    attr(r, "unit") <- result_u
  }
  return(r)
}

#' @inherit util_op_numeric_with_unit
#' @export
`*.numeric_with_unit` <- util_op_numeric_with_unit

#' @inherit util_op_numeric_with_unit
#' @export
`+.numeric_with_unit` <- util_op_numeric_with_unit

#' @inherit util_op_numeric_with_unit
#' @export
`-.numeric_with_unit` <- util_op_numeric_with_unit

#' @inherit util_op_numeric_with_unit
#' @export
`/.numeric_with_unit` <- util_op_numeric_with_unit

#' @inherit util_op_numeric_with_unit
#' @export
`%%.numeric_with_unit` <- util_op_numeric_with_unit

#' @inherit util_op_numeric_with_unit
#' @export
`%/%.numeric_with_unit` <- util_op_numeric_with_unit

#' @inherit util_op_numeric_with_unit
#' @export
`^.numeric_with_unit` <- util_op_numeric_with_unit
