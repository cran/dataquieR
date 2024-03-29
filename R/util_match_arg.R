#' `dataquieR` version of match.arg
#'
#' does not support partial matching, but will display the most likely
#' match as a warning/error.
#'
#' @param arg the argument
#' @param choices the choices
#' @param several_ok allow more than one entry in `arg`
#' @param error `stop()`, if `arg` is not in `choices`
#'              (warns and cleans `arg`, otherwise)
#'
#' @return "cleaned" `arg`
#'
#' @family robustness_functions
#' @concept string
#' @keywords internal
util_match_arg <- function(arg, choices, several_ok = FALSE, error = TRUE) {

  if (missing(arg)) {
    util_error("%s needs the argument %s",
               sQuote("util_match_arg"),
               sQuote("arg"))
  }

  arg_name <- util_deparse1(substitute(arg))

  calling_fkt <- sys.function(-1)
  fkt_name <- "<unknown function>"
  try({
    fkt_name <- as.character(sys.call(-1)[[1]])
  }, silent = TRUE)

  if (missing(choices)) {
    formal_args <-formals(calling_fkt)
    choices1 <- eval(
      formal_args[[arg_name]],
      envir = parent.frame()
    )
  } else {
    choices1 <- choices
  }

  choices1 <- util_ensure_character(
    choices1,
    error = TRUE,
    error_msg = c("For argument %s of function %s,",
                  "not all choices passed to %s could be interpreted as character."),
    sQuote(arg_name),
    sQuote(fkt_name),
    sQuote("util_match_arg")
  )

  choices1 <- unique(choices1)

  util_expect_scalar(
    choices1,
    allow_null = TRUE,
    allow_more_than_one = TRUE)

  if (length(choices1) < 1) {
    util_error(
      "the function %s does not provide any choice for its argument %s",
      sQuote(fkt_name), sQuote(arg_name))
  }

  if (eval.parent(call("missing", arg_name)) &&
      missing(choices) &&
      all(arg == choices1) &&
      length(arg) != 1 &&
      !several_ok) {
    arg <- NULL
  }

  arg1 <- util_ensure_character(
    arg,
    error = TRUE,
    error_msg = c("For argument %s of function %s,",
                  "not all choices passed to %s could be interpreted as character."),
    sQuote(arg_name),
    sQuote(fkt_name),
    sQuote("util_match_arg")
  )

  e <- new.env(parent = environment())

  assign(arg_name, arg1, e)

  cl <- call("util_expect_scalar", as.symbol(arg_name),
             allow_more_than_one = TRUE,
             allow_null = TRUE,
             allow_na = TRUE,
             check_type = is.character)

  eval(cl, envir = e)

  if (length(arg1) == 0) {
    arg1 <- choices1[[1]]
  }

  if (!several_ok && length(arg1) != 1) {
    util_error("the function %s needs exactly one entry in %s",
               sQuote(fkt_name), sQuote(arg_name))
  }

  util_ensure_in(
    arg1,
    choices1,
    err_msg =
      sprintf(
        paste("argument %s of function %s should be one of %s.",
              "%%s is not an allowed value, did you mean %%s?"),
        sQuote(arg_name),
        sQuote(fkt_name),
        util_pretty_vector_string(choices1)
    ),
    error = error)
}
