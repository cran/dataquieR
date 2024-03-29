#' Find first externally called function in the stack trace
#'
#' intended use: error messages for the user
#'
#' @param sfs reverse [sys.frames] to search in
#' @param cls reverse [sys.calls] to search in
#'
#' @return reverse [sys.frames] index of first non-dataquieR function in
#'         this stack
#'
#' @family condition_functions
#' @keywords internal
util_find_first_externally_called_functions_in_stacktrace <-
  function(sfs = rev(sys.frames()), cls = rev(sys.calls())) {
  first <- util_find_external_functions_in_stacktrace(sfs, cls)
  if (length(first) == 0) {
    NA_integer_
  } else {
    head(first, 1) - 2
  }
}
