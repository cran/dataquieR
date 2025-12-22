#' Get Access to Utility Functions
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param fkt function name
#' @param version version number to get
#'
#' @return an API object
#' @export
#'
#' @keywords internal
.get_internal_api <- function(fkt, version = API_VERSION, or_newer = TRUE) {
  # TODO: more life-cycle stuff, also for deprecation
  version <- try(as.package_version(version), silent = TRUE)
  if (util_is_try_error(version)) {
    util_error("You need to pass a version number in argument %s",
               sQuote("version"))
  }
  util_expect_scalar(or_newer, check_type = is.logical)
  if (!or_newer) {
    util_stop_if_not(version == API_VERSION)
  } else {
    util_stop_if_not(version <= API_VERSION)
  }
  f <- substitute(fkt)
  if (is.symbol(f)) {
    fkt <- as.character(f)
  }
  util_stop_if_not(is.character(fkt))
  util_stop_if_not(exists(fkt, mode = "function"))
  if (missing(version)) {
    util_warning(c("Hint: As a developer, you should not omit",
                   "the API version, when you access the internal API of %s,",
                   "write %s"),
                  sQuote(utils::packageName()),
                  sQuote(paste0(".get_internal_api(", fkt, ", version=\"",
                                API_VERSION, "\")"))
                 )
  }
  get(fkt, mode = "function")
}


#' Version of the API
#'
#' @seealso `.get_internal_api()`
#'
#' @export
#' @keywords internal
API_VERSION <- as.package_version("0.0.2")
