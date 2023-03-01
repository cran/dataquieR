#' Return the logged-in User's Full Name
#'
#' If `whoami` is not installed, the user name from
#' `Sys.info()` is returned.
#'
#' Can be overridden by options or environment:
#'
#' `options(FULLNAME = "Stephan Struckmann")`
#'
#' `Sys.setenv(FULLNAME = "Stephan Struckmann")`
#'
#' @return [character] the user's name
#'
#' @export
prep_get_user_name <- function() {
  if (length(options("FULLNAME")) == 1 &&
      length(options("FULLNAME")[[1]]) == 1 &&
      options("FULLNAME")[[1]] != "") {
    options("FULLNAME")
  } else if (Sys.getenv("FULLNAME", "") != "") {
    Sys.getenv("FULLNAME", "")
  } else if (requireNamespace('whoami', quietly = TRUE)) {
    whoami::fullname(fallback = Sys.info()[['user']])
  } else {
    Sys.info()[['user']]
  }
}
