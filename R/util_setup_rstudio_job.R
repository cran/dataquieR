#' Set up an RStudio job
#'
#' Also defines a progress function and a `progress_msg` function in
#' the caller's environment.
#'
#' In `RStudio` its job system will be used, for `shiny::withProgress`
#' based calls, this will require min and max being set to 0 and 1 (defaults).
#' If `cli` is available, it will be used, in all other cases, just `message`s
#' will be created.
#'
#' @param job_name a name for the job
#' @param n number of steps (needed, if, e.g., `progressr` should be used)
#'
#' @return
#' list: the `progress` function and the `progress_msg` function
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
#'
#' @family process_functions
#' @concept reporting
#' @noRd
util_setup_rstudio_job <- function(job_name = "Job", n) {

  if (missing(n)) {
    util_error(
      c("Internal error, sorry, please report: arguemnt %s is mandatory",
        "for %s"),
      sQuote("n"),
      sQuote("util_setup_rstudio_job"))
  }

  # Find context ----

  progress_init_fkt <-
    getOption("dataquieR.progress_init_fkt", dataquieR.progress_init_fkt_default)

  progress_fkt <-
    getOption("dataquieR.progress_fkt", dataquieR.progress_fkt_default)

  progress_msg_fkt <-
    getOption("dataquieR.progress_msg_fkt", dataquieR.progress_msg_fkt_default)

  is_shiny <- suppressWarnings(util_ensure_suggested("shiny", err = FALSE)) &&
    (!is.null(shiny::getDefaultReactiveDomain()))

  is_rstudio <-
    !is_shiny &&
    suppressWarnings(util_ensure_suggested("rstudioapi", err = FALSE)) &&
    rstudioapi::isAvailable()

  is_rstudio <- is_rstudio && util_really_rstudio()

  is_rstudio <- is_rstudio && !is.function(progress_msg_fkt)
  is_rstudio <- is_rstudio && !is.function(progress_fkt)

  is_cli <-
    !is_shiny &&
    !is_rstudio &&
    suppressWarnings(util_ensure_suggested("cli", err = FALSE))

  is_cli <- is_cli && !is.function(progress_msg_fkt)
  is_cli <- is_cli && !is.function(progress_fkt)

  p <- parent.frame()

  # RStudio: remove existing job, if a function calls us repeatedly ----
  if (is_rstudio && exists("rstudiojob", envir = p) &&
      !is.null(get("rstudiojob", envir = p))) {
    try(rstudioapi::jobRemove(get("rstudiojob", envir = p)), silent = TRUE)
    rstudiojob <- NULL
    assign("rstudiojob", rstudiojob, envir = p)
  }
  rstudiojob <- NULL

  # RStudio: Create new Job ----
  if (is_rstudio &&
      !is.function(progress_fkt) &&
      !is.function(progress_msg_fkt) &&
      !is.function(progress_init_fkt)
      ) {
    try({
      rstudiojob <- rstudioapi::jobAdd(job_name,
                                      progressUnits = 100L)
    }, silent = TRUE)
  }
  assign("rstudiojob", rstudiojob, envir = p) # always

  if (is_cli &&
      !is.function(progress_fkt) &&
      !is.function(progress_msg_fkt) &&
      !is.function(progress_init_fkt)) {
    cli::cli_progress_bar("Task...", total = 100, .envir = p)
  }

  # RStudio: Hook to remove job, if our caller is left ----
  do.call("on.exit", # TODO: see and maybe use withr::defer_parent?
          list(quote({
            Sys.sleep(1) # some strange concurrency problem with RStudio
            if (exists(".is_testing", inherits = FALSE)) {
              rm(".is_testing", inherits = FALSE)
            }
            if (!is.null(rstudiojob)) { # nocov start
              try({
                rstudioapi::jobRemove(rstudiojob)
                rstudiojob <- NULL
              }, silent = TRUE)
            } # nocov end
          }), add = TRUE),
          envir = p)

  assign(".is_testing",
    suppressWarnings(util_ensure_suggested("testthat", err = FALSE)) &&
    testthat::is_testing(),
    envir = p)

  # Define default progress functions ----
  progress <- function(percent, is_rstudio, is_shiny, is_cli, e) {
    if (isTRUE(e$.is_testing)) {
      return()
    }
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
    if (is_rstudio && !is.null(rstudiojob)) { # nocov start
      try({
        rstudioapi::jobSetProgress(rstudiojob, percent)
      }, silent = TRUE)
    } else if (is_shiny) {
      shiny::setProgress(value = percent / 100)
    } else if (is_cli) { # nocov end
      try(cli::cli_progress_update(set = percent, .envir = e), silent = TRUE)
    } else { # nocov start
      util_message("|%s>", paste(rep("#", percent), collapse = ""))
    } # nocov end
  }
  if (is.function(progress_fkt)) if
      (length(intersect(names(formals(progress_fkt)),
                names(formals(progress)))) ==
      length(union(names(formals(progress_fkt)),
                       names(formals(progress))))) {
    progress <- progress_fkt
  } else if (is.function(progress_fkt)) {
    util_error(
      c("option %s must refer to a compatible function",
        "in doubt, unset this option()"),
       dQuote("dataquieR.progress_fkt")
    )
  }
  formals(progress)$is_rstudio <- force(is_rstudio)
  formals(progress)$is_shiny <- force(is_shiny)
  formals(progress)$is_cli <- force(is_cli)
  formals(progress)$e <- p
  environment(progress) <- p
  assign("progress", progress, envir = p)

  progress_msg <- function(status, msg, is_rstudio, is_shiny, e) {
    if (isTRUE(e$.is_testing)) {
      return()
    }
    if (missing(msg) && !missing(status)) {
      msg <- status
      status <- ""
    }
    if (is_rstudio && !is.null(rstudiojob)) { # nocov start
      try({
        rstudioapi::jobSetStatus(rstudiojob, status)
        rstudioapi::jobAddOutput(rstudiojob, paste0(msg, "\n"))
      }, silent = TRUE)
    } else if (is_shiny) {
      shiny::setProgress(message = msg)
    } else if (is_cli) { # nocov end
      try(cli::cli_progress_update(status = msg, .envir = e), silent = TRUE)
    } else { # nocov start
      util_message("|%s ###", msg)
    } # nocov end
  }
  if (is.function(progress_msg_fkt)) if
      (length(intersect(names(formals(progress_msg_fkt)),
                       names(formals(progress_msg)))) ==
      length(union(names(formals(progress_msg_fkt)),
                   names(formals(progress_msg))))) {
    progress_msg <- progress_msg_fkt
  } else if (is.function(progress_msg_fkt)) {
    util_error(
      c("option %s must refer to a compatible function",
        "in doubt, unset this option()"),
      dQuote("dataquieR.progress_msg_fkt")
    )
  }
  formals(progress_msg)$is_rstudio <- force(is_rstudio)
  formals(progress_msg)$is_shiny <- force(is_shiny)
  formals(progress_msg)$is_cli <- force(is_cli)
  formals(progress_msg)$e <- p

  if (is.function(progress_init_fkt)) {
    if (!identical(names(formals(progress_init_fkt)), "n")) {
      util_error(
        c("option %s must refer to a compatible function",
          "in doubt, unset this option()"),
        dQuote("dataquieR.progress_init_fkt")
      )
    }
    progress_init_fkt(n = n)
  }

  environment(progress_msg) <- p
  assign("progress_msg", progress_msg, envir = p)

  progress_msg(job_name)

  invisible(list(progress = progress,
                 progress_msg = progress_msg))
}
