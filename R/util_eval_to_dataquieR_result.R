#' Evaluate an expression and create a `dataquieR_result` object from
#' it's evaluated value
#'
#' if an error occurs, the function will return a corresponding object
#' representing that error. all conditions will be recorded and replayed,
#' whenever the result is printed by [print.dataquieR_result].
#'
#' @param expression the expression
#' @param env the environment to evaluate the expression in
#' @param filter_result_slots [character] regular expressions, only
#'                                               if an indicator function's
#'                                               result's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#'
#' @return a `dataquieR_result` object
util_eval_to_dataquieR_result <- function(expression, env = parent.frame(),
                                          filter_result_slots) {
  .dq2_globs$.called_in_pipeline <- TRUE
  on.exit({
    .dq2_globs$.called_in_pipeline <- FALSE
  })
  errors <- list()
  warnings <- list()
  messages <- list()
  e <- environment()
  collect_condition <- function(cnd) {
    if (inherits(cnd, "error")) {
      e$errors <- c(e$errors, list(cnd))
    } else if (inherits(cnd, "warning")) {
      e$warnings <- c(e$warnings, list(cnd))
    } else if (inherits(cnd, "message")) {
      e$messages <- c(e$messages, list(cnd))
    } else {
      util_error("")
    }
  }
  r <- list()
  class(r) <- union("empty", class(r))
  suppressWarnings(suppressMessages(try(withCallingHandlers(
    {
      r <-  eval(expression, envir = env)
      if (length(r)) {
        if (length(filter_result_slots)) {
          r <- util_filter_names_by_regexps(r,
                                            filter_result_slots)
        }
        r <- util_compress_ggplots_in_res(r)
      }
    },
    error = collect_condition,
    warning = collect_condition,
    message = collect_condition
  ), silent = TRUE)))
  if (length(r) == 0) {
    r <- list()
    class(r) <- union("dataquieR_NULL", class(r))
  }
  attr(r, "error") <- errors
  attr(r, "warning") <- warnings
  attr(r, "message") <- messages
  class(r) <- union("dataquieR_result", class(r))
  r
}

.dq2_globs <- new.env(parent = emptyenv())
.dq2_globs$.called_in_pipeline <- FALSE
