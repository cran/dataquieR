#' Find externally called function in the stack trace
#'
#' intended use: error messages for the user
#'
#' @param sfs reverse [sys.frames] to search in
#' @param cls reverse [sys.calls] to search in
#'
#' @return vector of [logical]s stating for each index, if it had been called
#'          externally
#'
util_find_external_functions_in_stacktrace <-
  function(sfs = rev(sys.frames()),
           cls = rev(sys.calls())) {

  safe_parent_env <- function(...) {
    try(silent = TRUE,
        parent.env(...))
  }

  frame_parents <- lapply(sfs, safe_parent_env)
  frame_grand_parents <- lapply(frame_parents, safe_parent_env)
  frame_great_grand_parents <- lapply(frame_grand_parents, safe_parent_env)
  is_me <- vapply(frame_parents, identical, parent.env(environment()),
                  FUN.VALUE = logical(1))
  is_me <- is_me | vapply(frame_grand_parents, identical,
                          parent.env(environment()), FUN.VALUE = logical(1))
  is_me <- is_me | vapply(frame_great_grand_parents, identical,
                          parent.env(environment()),
                          FUN.VALUE = logical(1))
  is_base <- vapply(frame_parents, identical, asNamespace("base"),
                    FUN.VALUE =
                      logical(1)) # base never calls me, but by using do.call
  is_exception_handler_or_lambda <-
    withCallingHandlers(
    vapply(cls, function(cll) {
        if (length(cll) > 0) {
            if (!is.symbol(cll[[1]]))
              return(TRUE)
            if (cll[[1]] == as.symbol("withCallingHandlers"))
              return(TRUE)
            if (cll[[1]] == as.symbol("doTryCatch"))
              return(TRUE)
            if (cll[[1]] == as.symbol("tryCatchOne"))
              return(TRUE)
            if (cll[[1]] == as.symbol("tryCatchList"))
              return(TRUE)
            if (cll[[1]] == as.symbol("try"))
              return(TRUE)
      }
      return(FALSE)
    }, FUN.VALUE = logical(1)),
  error = browser
  )
  which(!is_me & !is_base & !is_exception_handler_or_lambda)

}
