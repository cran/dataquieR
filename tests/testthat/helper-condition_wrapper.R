expect_conditions <- function(expr, regexps, classes) {
  cl <- substitute(expr)
  .res <- regexps
  .res[] <- ""
  classes <- paste0(.res, classes) # recycle
  for (ire in seq_along(regexps)) {
    re <- regexps[[ire]]
    cls <- classes[[ire]]
    cl <- do.call(rlang::call2,
                       list(.fn = paste0("expect_", cls),
                            object = quote(cl),
                            regexp = re))
  }
  eval(cl, envir = parent.frame())
}

suppressWarningsMatching <- function(expr, regexps) {
  withCallingHandlers(expr,
                      warning = function(cnd) {
                        msg <- conditionMessage(cnd)
                        if (any(vapply(regexps, function(x)
                          all(grepl(x, paste0(msg, collapse = "\n"))),
                          FUN.VALUE = logical(1)))) {
                          invokeRestart("muffleWarning")
                        }
                      })
}
