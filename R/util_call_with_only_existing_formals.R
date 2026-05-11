#' Call a function with `...`, dropping arguments not present in formals
#'
#' Helper to forward `...` to a target function while silently removing
#' arguments that are not part of the target's formals (unless the target
#' itself has `...`). The call is evaluated in the caller environment so that
#' functions depending on the calling frame (e.g. `ls()`) behave as expected.
#'
#' @param f A function (or something coercible via [rlang::as_function()]).
#' @param ... Arguments to forward.
#'
#' @return The return value of `f(...)`.
#'
#' @noRd
util_call_with_only_existing_formals <- function(f, ...) {
  f <- rlang::as_function(f)

  dots <- rlang::dots_list(...)
  fm   <- rlang::fn_fmls(f)

  if (!("..." %in% names(fm))) {
    dots <- dots[intersect(names(dots), names(fm))]
  }

  rlang::eval_bare(
    rlang::call2(f, !!!dots),
    env = rlang::caller_env()
  )
}
