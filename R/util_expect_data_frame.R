#' Verify, that argument is a data frame
#'
#' stops with an error, if not. will add the columns, and return the resulting
#' extended data frame, and also updating the original data frame in the
#' calling environment, if #' `x` is empty (data frames easily break to
#' 0-columns in R, if they have not rows, e.g. using some `split`/`rbind`
#' pattern)
#'
#' @param x an object that is verified to be a `data.frame`.
#' @param col_names column names x must contain or named list of predicates to
#'                  check the columns (e.g.,
#'                  list(AGE=is.numeric, SEX=is.character))
#' @param convert_if_possible if given, for each column, a lambda can be given
#'                            similar to `col_names` check functions. This
#'                            lambda would be used to try a conversion. If
#'                            a conversion fails (returns `NA`, where the
#'                            input was not `util_empty'), an error
#'                            is still thrown, the data is converted, otherwise
#' @param dont_assign set `TRUE` to keep `x` in the caller environment untouched
#' @param keep_types [logical] keep types as possibly defined in a file, if the
#'                             data frame is loaded from one. set `TRUE` for
#'                             study data.
#' @param custom_errors [list] with error messages, specifically per column.
#'                      names of the list are column names, values are messages
#'                      ([character]).
#'
#' @return `invisible` data frame
#'
#' @family io
#' @concept data_frame
#' @noRd

util_expect_data_frame <- function(x, col_names, convert_if_possible,
                                   custom_errors,
                                   dont_assign, keep_types = FALSE) {
  # TODO: Custom error message
  if (missing(dont_assign)) dont_assign <- FALSE
  if (!missing(keep_types)) {
    util_expect_scalar(keep_types,
                       check_type = is.logical,
                       error_message =
                         sprintf("%s needs to be to be a logical value.",
                                 sQuote("keep_types")))
  }
  util_expect_scalar(dont_assign, check_type = is.logical, error_message =
                       sprintf("%s needs to be to be a logical value.",
                               sQuote("dont_assign")))
  symb <- substitute(x)
  mis_it <- try(eval.parent(call("missing", symb)), silent = TRUE)
  default <- try(get(symb, parent.frame()), silent = TRUE)
  if (inherits(default, "try-error") && identical(mis_it, TRUE)) {
    util_error("Missing %s",
               sQuote(util_deparse1(symb)))
  }
  if (missing(col_names)) {
    col_names <- character(0)
  }
  if (is.list(col_names)) {
    lambdas <- col_names
    if (!length(names(lambdas))) {
      util_error("%s needs to be a named list, if not a character vector",
                 sQuote("col_names"))
    }
    col_names <- names(lambdas)
  } else {
    util_expect_scalar(col_names,
                       allow_null = TRUE,
                       allow_more_than_one = TRUE)
    lambdas <- lapply(setNames(nm = col_names),
                      function(x) { function(y) TRUE })
  }
  if (missing(convert_if_possible)) {
    convert_if_possible <- NULL
  } else if (is.list(convert_if_possible)) {
    if (!length(names(convert_if_possible))) {
      util_error("%s needs to be a named list, if a list",
                 sQuote("convert_if_possible"))
    } else {
      util_ensure_in(names(convert_if_possible),
                     col_names,
                     err_msg = "",
                     error = FALSE)
    }
  } else if (length(convert_if_possible)) {
    util_error("%s needs to be a named list or NULL",
               sQuote("convert_if_possible"))
  }
  if (all(is.character(x)) && length(x) == 1) {
    # Handle data frame as char
    x <- prep_get_data_frame(x, keep_types = keep_types)
  }

  x <- util_cast_off(x, as.character(symb), .dont_cast_off_cols = TRUE)

  if (!is.data.frame(x)) {
    util_error("%s is not a data frame.", dQuote(symb))
  }
  if (any(!col_names %in% colnames(x))) {
    if (nrow(x) == 0) {
      cols <- union(
        colnames(x),
        col_names)
      x <- as.data.frame(
        matrix(nrow = 0, ncol = length(cols),
               dimnames = list(character(0), cols)),
        stringsAsFactors = FALSE, make.names = FALSE)
      if (!dont_assign)
        assign(as.character(symb), x, envir = parent.frame())
    } else {
      util_error("Missing columns %s from %s.",
                 paste(dQuote(col_names[!col_names %in% colnames(x)]),
                       collapse = ", "),
                 dQuote(symb))
    }
  }
  detect_mismatch <-function() {
    !vapply(names(lambdas[
      vapply(lambdas, is.function, FUN.VALUE = logical(1))]), function(cl) {
        r <- lambdas[[cl]](x[[cl]])
        if (length(r) == nrow(x) && is.logical(r)) {
          r <- all(r, na.rm = TRUE)
        }
        if (length(r) != 1 || is.na(r) || !is.logical(r)) {
          util_error(c("Internal error in util_expect_data_frame:",
                       "lambda %s did not return %s or %s"),
                     sQuote(cl), dQuote(TRUE), dQuote(FALSE),
                     applicability_problem = TRUE)
        }
        r
      }, FUN.VALUE = logical(1))
  }
  mismatch <- detect_mismatch()
  if (any(mismatch)) {
    if (is.list(convert_if_possible)) {
      for (cn in names(mismatch)) {
        convert <- convert_if_possible[[cn]]
        if (is.function(convert)) {
          x_cn <- convert(x[[cn]])
          if (!all(is.na(x_cn) == is.na(x[[cn]]))) {
            util_warning(
              "In %s, could not convert the whole column %s to match data type",
              dQuote(symb), dQuote(cn))
          } else {
            x[[cn]] <- x_cn
          }
        }
      }
    }
    mismatch <- detect_mismatch()
  }
  if (any(mismatch)) {
    if (!missing(custom_errors)) {
      util_stop_if_not("argument custom_errors needs to be a named list" =
                         is.list(custom_errors))
      cols_with_trouble <- names(which(mismatch))
      if (all(cols_with_trouble %in% names(custom_errors))) { # TODO: default should fill gaps in custom_errors
        util_error(
          applicability_problem = TRUE,
          paste0(custom_errors[cols_with_trouble], collapse = "\n"))
        }
    }
    util_error(
      applicability_problem = TRUE,
      "Data in columns in %s do not match: %s.\n%s",
      sQuote(symb),
      paste(dQuote(names(mismatch[mismatch])), collapse = ", "),
      do.call(paste, append(dQuote(vapply(lambdas[mismatch], util_deparse1,
                            FUN.VALUE = character(1))), list(collapse = ", ")))
    )
  }
  if (!dont_assign) {
    assign(as.character(symb), x, envir = parent.frame())
  }
  invisible(x)
}
