#' Search for a formal in the stack trace
#'
#' Similar to [dynGet()], find a symbol in the closest data quality indicator
#' function and return its value. Can `stop()`, if symbol evaluation causes a
#' stop.
#'
#' @param symbol symbol to find
#'
#' @return value of the symbol, if available, `NULL` otherwise
#'
#' @family condition_functions
#' @noRd
util_find_indicator_function_in_callers <- function(symbol = "resp_vars") {
  n <- 1
  found <- FALSE
  try({
    while (!any( rlang::call_name(rlang::caller_call(n)) %in%
                 names(.indicator_or_descriptor))) {
    # while (!any( rlang::call_name(rlang::caller_call(n)) %in%
    #              names(.indicator_or_descriptor)) ||
    #        # find ony, if resp_vars are in the call, so not missing, but you still could write ...(resp_vars = ,)
    #        !any(symbol %in% rlang::call_args_names(rlang::caller_call(n)))) {
#      str(rlang::call_args(rlang::caller_call(n)))
      n <- n + 1
    }
    found <- TRUE
  }, silent = TRUE)
  if (found) {
    r <- withr::with_language("en", try(dynGet(symbol,
                inherits = TRUE,
                ifnotfound = NULL,
                minframe = n), silent = TRUE)) # n - 1 is relative to my caller, but for dynGet from here, it fits
    if (inherits(r, "try-error")) {
      cnd <- attr(r, "condition")
      if (conditionMessage(cnd) ==
          sprintf("argument \"%s\" is missing, with no default", symbol)) {
        return(NULL)
      } else {
        util_error(cnd)
      }
    }
    return(r)
  } else {
    return(NULL)
  }
}
