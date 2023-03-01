#' Deprecate functions and arguments
#'
#' if available, it calls `lifecycle::deprecate_soft`. otherwise, it just
#' shows a warning.
#'
#' @param when A string giving the version when the behavior was deprecated.
#' @param what A string describing what is deprecated
#' @param with An optional string giving a recommended replacement for the deprecated behavior. This takes the same form as what.
#' @param details only used, if `lifecycle::deprecate_soft` is available
#' @param id  only used, if `lifecycle::deprecate_soft` is available
#' @param env only used, if `lifecycle::deprecate_soft` is available
#' @param user_env only used, if `lifecycle::deprecate_soft` is available
#'
#' @return NULL, invisibly.
util_deprecate_soft <- function(when,
                                what,
                                with = NULL,
                                details = NULL,
                                id = NULL,
                                env = rlang::caller_env(),
                                user_env = rlang::caller_env(2)) {
  if (suppressWarnings(util_ensure_suggested("lifecycle", err = FALSE))) {
    cl <- rlang::call_match()
    cl$env <- force(env)
    cl$user_env <- force(user_env)
    cl[[1]] <- rlang::call2("::",
                    rlang::sym("lifecycle"),
                    rlang::sym("deprecate_soft"))
    eval(cl, envir = rlang::caller_env())
  } else {
    if (length(with) == 1)
      w <- sprintf("\nPlease use `%s` instead.", as.character(with))
    else
      w <- ""
    util_warning("`%s` was deprecated in %s v%s.%s",
                 as.character(what),
                 packageName(),
                 as.character(when),
                 w)
    invisible(NULL)
  }
}
