#' Isolate enclosure of a function to avoid exporting too much for parallel
#'
#' @param FUN [function()] the function to isolate
#' @param vars [character()] object names from `envir` to have in the
#'                           isolated version of `FUN`
#' @param envir [environment()] environment to copy the objects listed in
#'                              `vars` from
#'
#' @returns [function()] the isolated function
#' @noRd
util_isolate_function <- function(FUN, vars = character(),
                                  envir = parent.frame()) {
  # get objects to keep
  obj_list <- mget(vars, envir = envir, inherits = TRUE)

  # build clean environment
  fn_env <- list2env(obj_list, parent = environment(util_isolate_function))

  # update closure
  environment(FUN) <- fn_env

  return(FUN)
}
