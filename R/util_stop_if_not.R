#' Verify assumptions made by the code, that must be TRUE
#'
#' @seealso [`stopifnot`]
#'
#' @param ... see [`stopifnot`]
#' @param label [character] a label for the assumptions, can be missing
#' @param label_only [logical] if `TRUE` and `label` is given, the condition
#'                                will not be displayed, if `FALSE`
#'
#' @return `invisible(FALSE)`, if not stopped.
#'
#'
#' @family robustness_functions
#' @concept condition
#' @noRd
util_stop_if_not <- function(..., label, label_only) { # FIXME: Strange ... problems in some cases
  cc <- rlang::current_call()
  # rlang::call_name(cl)
  # setdiff(rlang::call_args_names(rlang::caller_call(n = 0)), setdiff(names(formals(rlang::call_name(rlang::current_call()))), "..."))
  my_own <-
    setdiff(names(formals(rlang::call_name(cc))), "...")
  zappings <- rep(list(rlang::zap()), length(my_own))
  names(zappings) <- my_own
  mod_args <- c(list(.call = cc), zappings)
  cl <- do.call(rlang::call_modify, mod_args, quote = TRUE)
  cl[[1]] <- rlang::sym("stopifnot") # overwrites also <ns> in <ns>::
  ok <- try(eval.parent(cl), silent = TRUE)
  if (missing(label)) {
    label <- ""
  }
  if (missing(label_only)) {
    label_only <- TRUE
  }
  util_expect_scalar(label, check_type = is.character)
  util_expect_scalar(label_only, check_type = is.logical)
  if (inherits(ok, "try-error")) {
    cm <- conditionMessage(attr(ok, "condition"))
    if (label_only && nzchar(label)) cm <- label
    if (!label_only && nzchar(label) && nzchar(cm))
      cm <- paste0(label, ": ", cm)
    util_error(
      paste("Internal error:", gsub("%", "%%", fixed = TRUE, cm)),
      applicability_problem = FALSE)
  }
  invisible(ok)
}
