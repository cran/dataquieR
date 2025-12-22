#' @family missing_functions
#' @concept missing
#' @noRd
util_seg_table <- function(ds2, study_data, meta_data, expected_observations =
                         c("HIERARCHY",
                           "ALL",
                           "SEGMENT")) {
  make_df <- function(x, ...) {
    r <- data.frame(CODES = as.character(rownames(x)), Freq = x,
                    row.names = NULL)
    r
  }
  lapply(lapply(lapply(setNames(nm = colnames(ds2)), function(x) {
    y <- ds2[util_observation_expected(x, study_data, meta_data, VAR_NAMES,
                                       expected_observations =
                                         expected_observations), x,
             drop = TRUE]
    table(y)
  }), as.matrix), make_df)
}
