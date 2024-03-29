#' check, if a scalar/vector function argument matches expectations
#'
#' @param arg_name the argument
#' @param allow_more_than_one allow vectors
#' @param allow_null allow NULL
#' @param allow_na allow `NAs`
#' @param min_length minimum length of the argument's value
#' @param max_length maximum length of the argument's value
#' @param check_type a predicate function, that must return `TRUE` on the
#'                   argument's value.
#' @param convert_if_possible if given, a lambda can be given
#'                            similar to `check_type` This
#'                            lambda would be used to try a conversion. If
#'                            a conversion fails (returns `NA`, where the
#'                            input was not `util_empty'), an error
#'                            is still thrown, the data is converted, otherwise
#' @param dont_assign set `TRUE` to keep `x` in the caller environment untouched
#' @param conversion_may_replace_NA if set to `TRUE`, we can define a function
#'                    in `convert_if_possible` that replaces `NA` values without
#'                    causing a warning, but this option is set to `FALSE` by
#'                    default to catch possible conversion problems (use it with
#'                    caution).
#' @param error_message if `check_type()` returned `FALSE`, show this instead of
#'                      a default error message.
#'
#' @return the value of arg_name -- but this is updated in the calling
#'         frame anyway.
#'
#' @examples
#' \dontrun{
#' f <- function(x) {
#'   util_expect_scalar(x, check_type = is.integer)
#' }
#' f(42L)
#' try(f(42))
#' g <- function(x) {
#'   util_expect_scalar(x, check_type = is.integer, convert_if_possible =
#'           as.integer)
#' }
#' g(42L)
#' g(42)
#' }
#'
#' @family robustness_functions
#' @concept robustness
#' @keywords internal
util_expect_scalar <- function(arg_name,
                               allow_more_than_one = FALSE,
                               allow_null = FALSE,
                               allow_na = FALSE,
                               min_length = -Inf,
                               max_length = Inf,
                               check_type,# IDEA: attr(check_type, "error_message") as default for error_message and some prepared predicates with reasonable standard messages for standard case like is.character or is.numeric (or maybe, for the most simple ones, list them here)
                               convert_if_possible,
                               conversion_may_replace_NA = FALSE,
                               dont_assign = FALSE,
                               error_message) {
  if (!missing(error_message)) {
    util_expect_scalar(arg_name = error_message,
                       # error_message = Do not use error_message here to
                       #                 prevent infinite recursion!!
                       check_type = is.character)
  }

  if (missing(convert_if_possible)) {
    convert_if_possible <- NULL
  } else if (!is.function(convert_if_possible)) {
    util_error("%s needs to be a function",
               sQuote("convert_if_possible"))
  }
  if (missing(dont_assign)) dont_assign <- FALSE
  if (length(dont_assign) != 1 || is.na(dont_assign) ||
      !is.logical(dont_assign)) {
    util_error("%s needs to be TRUE or FALSE", sQuote("dont_assign"))
  }
  # interpret the arg_name argument
  try({
    arg_name <- as.character(substitute(arg_name))
  }, # if called with a symbol, get that symbol as a character,
  silent = TRUE # if called with a character, do nothing
  )
  if (!all(is.character(arg_name))) { # nocov start
    # if we did not get a character up to here, there is an error in
    # calling this util_expect_scalar
    # this should never happen anyway, so cannot test this
    util_error(c(
      "argument arg_name must be either one character or one symbol,",
      "wrong use of util_expect_scalar?"))
  } # nocov end
  if (length(arg_name) != 1) {
    # if we have 0 or more than one argument name, this is also an
    # error in calling util_expect_scalar
    util_error(c(
      "argument arg_name must be of length 1,",
      "wrong use of util_expect_scalar?"))
  }

  p <- parent.frame(1) # access the caller's environment

  if (!exists(arg_name, envir = p)) {
    # check, if the function argument exists in the calling function.
    util_error(
      c("Unknown function argument %s checked,",
        "wrong use of util_expect_scalar?"),
      arg_name)
  }

  missing_in_parent <- eval.parent(call("missing", as.symbol(arg_name)))

  arg_value <- try(get(arg_name, envir = p), silent = TRUE)
  # try to get the value of the callers argument called `arg_name`.
  if (inherits(arg_value, "try-error")) {
    if (missing_in_parent) {
      util_warning(
        c("Missing argument %s without default value. Setting to NULL. As",
          "a dataquieR developer, please add a default value for %s to",
          "remove this warning."),
        dQuote(arg_name), dQuote(arg_name),
        applicability_problem = TRUE)
    } else {
      util_warning(
        c("Could not get value of argument %s for unexpected reasons. Setting",
          "to NULL."), arg_name, applicability_problem = TRUE)
      util_warning(arg_value, applicability_problem = TRUE)
    }
    arg_value <- NULL
  }

  if (!allow_null && is.null(arg_value)) {
    # if we need the argument in arg_name, but the user provided NULL
    util_error("Argument %s is NULL", arg_name,
               applicability_problem = TRUE) # this is an error
  }

  if (allow_more_than_one) {
    # if we allow more than one arg_value in the argument arg_name
    if (!allow_null && length(arg_value) == 0) {
      # and we do need at least one arg_value name here, but the user did not
      # provide any
      util_error("Need at least one element in argument %s, got 0", arg_name,
                 applicability_problem = TRUE)
      # this is an error
    }

    if (length(dim(arg_value))) {
      util_error(
        "Need a vector in argument %s, got an object with %d dimensions",
        arg_name,
        length(dim(arg_value)),
        applicability_problem = TRUE)
    }

  } else {# if we expect one value in the argument arg_name at most
    if ((!allow_null && length(arg_value) != 1) ||
        (allow_null && length(arg_value) > 1)) {
      # but the user gave more than one or none although
      # allow_null prohibits this
      util_error("Need exactly one element in argument %s, got %d: [%s]",
                 arg_name, length(arg_value), paste0(arg_value, collapse = ", "),
                 applicability_problem = TRUE)
      # this is an error
    }
  }


  if (length(min_length) != 1) {
    if (!all(is.numeric(min_length))) {
      util_error(c("Need numeric min_length names in argument %s",
                   "wrong use of util_expect_scalar?"),
                 arg_name,
                 applicability_problem = FALSE)
    }
  }

  if (length(max_length) != 1) {
    if (!all(is.numeric(max_length))) {
      util_error(c("Need numeric max_length names in argument %s",
                   "wrong use of util_expect_scalar?"),
                 arg_name,
                 applicability_problem = FALSE)
    }
  }

  if (length(arg_value) < min_length || length(arg_value) > max_length) {
    if (!(allow_null && is.null(arg_value))) {
      util_error("Argument %s must have a length in [%s:%s]",
                 arg_name,
                 as.character(min_length),
                 as.character(max_length),
                 applicability_problem = TRUE)
    }
  }


  my_check_type <- function(arg_value) {
    type_match <- try(check_type(arg_value))
    if (length(type_match) != 1 ||
        !is.logical((type_match)) ||
        is.na(type_match)) {
      util_error(c("Need a lambda function as a predicate as %s for the",
                   "argument %s, but the function returned %s",
                   "wrong use of util_expect_scalar?"),
                 dQuote("check_type"),
                 arg_name,
                 sQuote(paste(deparse(type_match, nlines = 2),
                              collapse = "\n")),
                 applicability_problem = FALSE)
    }
    type_match
  }

  if (length(arg_value) > 0 && (!missing(check_type))) {
    if (!is.function(check_type)) {
      util_error(c("Need a lambda function as a predicate as %s for the",
                   "argument %s",
                   "wrong use of util_expect_scalar?"),
                 dQuote("check_type"),
                 arg_name,
                 applicability_problem = FALSE)
    }
    type_match <- my_check_type(arg_value)
    if (!type_match && !is.null(convert_if_possible)) {
      x_arg <- convert_if_possible(arg_value)
      if (!conversion_may_replace_NA) {
        if (!all(is.na(x_arg) == is.na(arg_value))) {
          util_warning(
            "In %s, could not convert the whole vector to match data type",
            dQuote(arg_name))
        } else {
          arg_value <- x_arg
        }
        type_match <- my_check_type(arg_value)
      } else {
        if (!all(which(is.na(x_arg)) %in% which(is.na(arg_value)))) {
          util_warning(
            "In %s, conversion introduced NAs",
            dQuote(arg_name))
        } else {
          arg_value <- x_arg
        }
        type_match <- my_check_type(arg_value)
      }
    }
    if (!type_match) {
      if (!missing(error_message)) {
        util_error(error_message,
                   applicability_problem = TRUE)
      } else {
        util_error("Argument %s must match the predicate %s",
                   arg_name,
                   dQuote(paste(head(deparse(check_type)), collapse = " ")),
                   applicability_problem = TRUE)
      }
    }
  }

  if (!allow_na && any(is.na(arg_value))) {
    util_error("Argument %s must not contain NAs",
               arg_name,
               applicability_problem = TRUE)
  }

  if (!dont_assign)
    assign(arg_name, arg_value, envir = p) # re-assign the possibly modified
  # argument value in the caller's environment

  arg_value
}
