#' Make a function capturing errors and other conditions for parallelization
#'
#' @param fct [function] to prepare
#'
#' @return decorated [function]
util_make_function <- function(fct) {
  try(caller. <- sys.call(1), silent = TRUE)
  if (is.character(fct)) {
    .fct_name <- fct
  } else {
    .fct_name <- deparse(substitute(fct))
  }
  .fct <- match.fun(fct)

  function(...) { # call a dataquieR function producing a dataquieR_result
                  # (having warnings, messages and errors attached)
    rv <- dQuote(list(...)[["resp_vars"]])
    env <- new.env(parent = environment())
    env$e <- list()
    env$w <- list()
    env$m <- list()
    r <-
      tryCatch(withCallingHandlers(
        .fct(...),
        warning = function(.w) {
          .w$message <- sprintf("In %s (%s): %s", .fct_name, rv, .w$message)
          .w$call <- caller.
          env$w[[length(env$w) + 1]] <- .w
          invokeRestart("muffleWarning")
        },
        message = function(.m) {
          .m$message <- sprintf("In %s (%s): %s", .fct_name, rv, .m$message)
          .m$call <- caller.
          env$m[[length(env$m) + 1]] <- .m
          invokeRestart("muffleMessage")
        }
      ), error = function(.e) {
        .e$message <- sub("^In .fct:", sprintf("In %s (%s):", .fct_name, rv),
                          .e$message)
        .e$call <- caller.
        env$e[[length(env$e) + 1]] <- .e
        list()
      })
    attr(r, "error") <- env$e
    attr(r, "message") <- env$m
    attr(r, "warning") <- env$w
    class(r) <- c(class(r), "dataquieR_result")
    r
  }
}
