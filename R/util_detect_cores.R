#' Detect cores
#'
#' See `parallel::detectCores` for further details.
#'
#' @return number of available CPU cores.
#'
#' @family system_functions
#' @concept process
#' @keywords internal
util_detect_cores <- function() {
  if (requireNamespace("parallelly", quietly = TRUE)) {
    return(parallelly::availableCores())
  } else if (requireNamespace("rJava", quietly = TRUE)) {
    rJava::.jinit()
    rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;",
                        method = "getRuntime")
    cpus <- rJava::.jcall(rt, "I", "availableProcessors")
    return(cpus)
  } else if (requireNamespace("parallel", quietly = TRUE)) {
    r <- parallel::detectCores()
    if (length(r) != 1 || is.na(r) || !util_is_integer(r) || r < 1) {
      r <- 1
    }
    return(r)
  } else{
    util_warning(c("None of the suggested packages %s are found,",
                   "autodetection of CPU cores disabled --",
                   "using default of 1 core only."),
                 util_pretty_vector_string(c(
                   "parallel",
                   "parallelly",
                   "rJava")))
    return(1)
  }
}
