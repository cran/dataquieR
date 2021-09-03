#' Detect cores
#'
#' See `parallel::detectCores` for further details.
#'
#' @return number of available CPU cores.
util_detect_cores <- function() {
  if (requireNamespace("parallel", quietly = TRUE)) {
    return(parallel::detectCores())
  } else{
    util_warning(c("Suggested package parallel not found,",
                   "autodetection of CPU cores disabled --",
                   "using default of 1 core only."))
    return(1)
  }
}
