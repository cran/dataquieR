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
    util_call_progress_hooks("progress", percent = percent)
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
    util_call_progress_hooks("msg", status = status, msg = msg)
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
  if (rlang::is_missing(formals(progress_msg)$msg))
    formals(progress_msg)$msg <- ""

  if (is.function(progress_init_fkt)) {
    if (!identical(names(formals(progress_init_fkt)), "n")) {
      util_error(
        c("option %s must refer to a compatible function",
          "in doubt, unset this option()"),
        dQuote("dataquieR.progress_init_fkt")
      )
    }
    progress_init_fkt(n = n)
    util_call_progress_hooks("init", n = n)
  }

  environment(progress_msg) <- p
  assign("progress_msg", progress_msg, envir = p)

  progress_msg(job_name)

  invisible(list(progress = progress,
                 progress_msg = progress_msg))
}

.progress_hooks <- new.env(parent = emptyenv())

#' Register a hook function for progresses in computation/rendering
#'
#' The order hooks are called is not defined.
#'
#' @param type [character] what event
#' @param hook [function] hook function
#'
#' @returns [character] a handle for de-registering, `invisible`
#' @export
prep_register_progress_hook <- function(type = c("progress", "init", "msg"),
                                        hook) {
  type <- util_match_arg(type)
  util_expect_scalar(hook,
                     check_type = is.function,
                     allow_na = TRUE,
                     error_message = sprintf("%s must be a function",
                                             sQuote("hook")))
  if (type == "init" && !"n" %in% names(formals(hook))) {
    util_error("A progress hook of type %s must feature a formal argument %s",
               sQuote(hook), sQuote("n"))
  }
  if (type == "progress" && !"percent" %in% names(formals(hook))) {
    util_error("A progress hook of type %s must feature a formal argument %s",
               sQuote(hook), sQuote("percent"))
  }
  if (type == "msg" && !all(c("status", "msg") %in% names(formals(hook)))) {
    util_error(c("A progress hook of type %s must feature formal arguments %s",
                 "and %s"),
               sQuote(hook), sQuote("percent"), sQuote("msg"))
  }
  handle <- rlang::hash(list(type, hook))
  if (!exists(type, .progress_hooks, mode = "list")) {
    .progress_hooks[[type]] <- list()
  }
  if (!is.null(.progress_hooks[[type]][[handle]])) {
    util_message("This hook has already been registered, won't register twice")
  } else {
    .progress_hooks[[type]][[handle]] <- hook
  }
  return(invisible(handle))
}

#' De-register a hook function for progresses in computation/rendering
#'
#' @param handle [character] the handle
#' @param verbose [logical] message, if `handle` has currently no registration
#'
#' @returns [logical] `invisible(TRUE)` on success
#' @export
prep_deregister_progress_hook <- function(handle, verbose = TRUE) {
  util_expect_scalar(verbose,
                     check_type = is.logical,
                     error_message = sprintf("%s must be a logical",
                                             sQuote("verbose")))
  util_expect_scalar(handle,
                     check_type = is.character,
                     error_message = sprintf("%s must be a character",
                                             sQuote("handle")))
  found <- FALSE
  for (type in eval(formals(prep_register_progress_hook)[["type"]],
                    envir = baseenv())) {
    if (!exists(type, .progress_hooks, mode = "list")) {
      .progress_hooks[[type]] <- list()
    }
    if (!is.null(.progress_hooks[[type]][[handle]])) {
      .progress_hooks[[type]][[handle]] <- NULL
      found <- TRUE
    }
  }
  if (!found && verbose) {
    util_message(c("This hook has not been registered or already",
                   "de-registered, cannot de-register"))
  }
  return(invisible(found))
}

util_call_progress_hooks <- function(type = c("progress", "init", "msg"), ...) {
  type <- util_match_arg(type)
  if (!exists(type, .progress_hooks, mode = "list")) {
    .progress_hooks[[type]] <- list()
  }
  for (handle in names(.progress_hooks[[type]])) {
    if (is.function(.progress_hooks[[type]][[handle]])) {
      worked <-
        try(
          util_call_with_only_existing_formals(
            .progress_hooks[[type]][[handle]], ...),
          silent = TRUE)
      if (util_is_try_error(worked)) {
        util_message("Could not call a hook function: %s\n%s",
                     head(.progress_hooks[[type]][[handle]]),
                     conditionMessage(attr(worked, "condition")))
      }
    } else {
      util_message(
        c("Could not call a hook function, which should never",
          "have been registered: %s. Internal error, sorry. Please report."),
        dQuote(util_deparse1(.progress_hooks[[type]][[handle]]))
      )
    }
  }
}
