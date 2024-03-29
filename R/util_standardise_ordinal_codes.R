#' Compute something comparable from an `ordered`
#'
#' interpolates categories of an ordinal variable
#'
#' @param codes [numeric()] `n` values
#' @param maxlevel_old [integer()] number of categories of `codes`
#' @param maxlevel_new [integer()] number of categories for output
#'
#' @return [integer()] `n` values in `{1, ..., maxlevel_new}`
#' @keywords internal
util_standardise_ordinal_codes <- function(codes, maxlevel_old, maxlevel_new) {
  util_expect_scalar(maxlevel_old, check_type = util_is_numeric_in(min = 2,
                                                                   whole_num = TRUE,
                                                                   finite = TRUE))
  util_expect_scalar(maxlevel_new, check_type = util_is_numeric_in(min = 2,
                                                                   whole_num = TRUE,
                                                                   finite = TRUE))
  util_expect_scalar(codes,
                     allow_more_than_one = TRUE,
                     check_type = util_is_numeric_in(min = 1,
                                                     max = maxlevel_old,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  as.integer(round((codes - 1) * ((maxlevel_new - 1) / (maxlevel_old - 1))) + 1)
}
