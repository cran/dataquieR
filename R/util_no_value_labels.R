#' Select really [numeric] variables
#'
#' Reduce `resp_vars` to those, which are either `float` or `integer` without
#' [VALUE_LABELS], i.e. likely [numeric] but not a [factor]
#'
#' @param resp_vars [variable list] len=1-2. the name of the continuous
#'                                           measurement variable
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param warn [logical] warn about removed variable names
#' @param stop [logical] stop on no matching `resp_var`
#'
#' @return [character] vector of matching `resp_vars`.
#'
#' @importFrom stats setNames
util_no_value_labels <- function(resp_vars, meta_data, label_col, warn = TRUE,
                                 stop = TRUE) {
  stopifnot(ncol(meta_data) > 1)
  matching_vars <- vapply(FUN.VALUE = logical(1), X = setNames(nm = resp_vars),
                          FUN = function(rv) {
    row <- meta_data[meta_data[[label_col]] == rv, , TRUE]
    vl <- row[[VALUE_LABELS]]
    dt <- row[[DATA_TYPE]]
    if (dt == DATA_TYPES$FLOAT) {
      return(TRUE)
    } else if (dt == DATA_TYPES$INTEGER) {
      return(length(vl) == 0 || is.na(vl) || trimws(vl) == "")
    } else {
      return(FALSE)
    }
  })
  if (stop && all(!matching_vars)) {
    util_error(c(
      "None of the variables %s is float or integer",
      "without VALUE_LABELS; aborting."),
      paste0(dQuote(resp_vars), collapse = ", "),
      applicability_problem = TRUE)
  } else if (warn && any(!matching_vars)) {
    util_warning(
      c("The variables %s are neither float nor integer",
        "without VALUE_LABELS. Ignoring those"),
      paste0(dQuote(resp_vars[!matching_vars]), collapse = ", "),
      applicability_problem = TRUE
    )
  }
  resp_vars[matching_vars]
}
