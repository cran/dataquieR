#' Set up an RStudio job
#'
#' Also defines a progress function and a `progress_msg` function in
#' the caller's environment.
#'
#' @param job_name a name for the job
#'
#' @return
#' the progress function
#'
#' @examples
#' \dontrun{
#'   test <- function() {
#'     util_setup_rstudio_job("xx")
#'     Sys.sleep(5)
#'     progress(50)
#'     progress_msg("halfway through")
#'     Sys.sleep(5)
#'     progress(100)
#'     Sys.sleep(1)
#'   }
#'   test()
#' }
util_setup_rstudio_job <- function(job_name = "Job") {
  p <- parent.frame()

  if (exists("rstudiojob", envir = p) &&
      !is.null(get("rstudiojob", envir = p))) {
    try(rstudioapi::jobRemove(get("rstudiojob", envir = p)), silent = TRUE)
    rstudiojob <- NULL
    assign("rstudiojob", rstudiojob, envir = p)
  }

  rstudiojob <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    try({
      rstudiojob <- rstudioapi::jobAdd(job_name,
                                      progressUnits = 100L)
    }, silent = TRUE)
  }

  assign("rstudiojob", rstudiojob, envir = p)

  do.call("on.exit", # TODO: see and maybe use withr::defer_parent?
          list(quote({
            Sys.sleep(1) # some strange concurrency problem with RStudio
            if (!is.null(rstudiojob)) { # nocov start
              try({
                rstudioapi::jobRemove(rstudiojob)
                rstudiojob <- NULL
              }, silent = TRUE)
            } # nocov end
          }), add = TRUE),
          envir = p)

  progress <- function(percent) {
    if (length(percent) != 1)
      return()
    if (is.na(percent))
      return()
    if (!is.numeric(percent))
      return()
    if (percent < 0)
      return()
    if (percent > 100)
      return()
    if (!is.null(rstudiojob)) { # nocov start
      try({
        rstudioapi::jobSetProgress(rstudiojob, percent)
      }, silent = TRUE)
    } # nocov end
  }
  assign("progress", progress, envir = p)

  progress_msg <- function(status, msg) {
    if (!is.null(rstudiojob)) { # nocov start
      try({
        rstudioapi::jobSetStatus(rstudiojob, status)
        rstudioapi::jobAddOutput(rstudiojob, paste0(msg, "\n"))
      }, silent = TRUE)
    } # nocov end
  }
  assign("progress_msg", progress_msg, envir = p)

  invisible(progress)
}
